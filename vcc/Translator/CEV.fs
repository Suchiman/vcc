//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
  open Microsoft.FSharp.Math
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.TranslatorUtils
  open Microsoft.Research.Vcc.Util
  open System
  
  module C = Microsoft.Research.Vcc.CAST
  module B = Microsoft.Research.Vcc.BoogieAST
  
  type CEV(ctx:TranslationState) =
      let n = ref 0

      let loc_or_glob k =
        let str =
          match k with
            | C.Parameter -> "cev_parameter"
            | C.SpecParameter -> "cev_local"
            | C.OutParameter -> "cev_local"
            | C.Local -> "cev_local"
            | C.SpecLocal -> "cev_local"
            | C.Global -> "cev_global"
            | C.ConstGlobal -> "cev_global"
            | C.QuantBound -> "cev_implicit"
        er str

      let cevPrintVarOK (str : string) k = 
        match k with 
        | C.QuantBound -> false
        | _ -> not (str.Contains "ite") (* CLG: what other vars are added in automatically? *)

      let cevNIncr () = n := !n + 1;

      let cevSavePos n pos = B.Stmt.Assume (bEq (bCall "#cev_save_position" [B.Expr.IntLiteral(bigint.Parse((n).ToString()))]) (er pos) );

      let cevVarsIntroed = dict[]
      let addType = ctx.AddType
      let getTokenConst = ctx.GetTokenConst
      let varRef = ctx.VarRef
      let helper = ctx.Helper
      let registerToken = ctx.RegisterToken

      let rec processEList (elist : C.Expr list) varlist = 
        match elist with
        | e :: [] -> let _, vlist = lastStmtVars e varlist
                     vlist
        | e :: es -> let (_, vlist) = lastStmtVars e varlist
                     processEList es vlist
        | [] -> varlist 

      and lastStmtVars (body : C.Expr) varlist = 
        match body with
        | C.Expr.Prim(ec, o, elist) ->
            let varlist' = processEList elist varlist 
            if not (List.isEmpty elist) then 
                let (t, _) = lastStmtVars (List.head (List.rev elist)) varlist
                (t, varlist')
            else
                (ec.Token, varlist)
        | C.Expr.Call(ec, f, tlist, elist) -> 
            let varlist' = processEList elist varlist 
            if not (List.isEmpty elist) then
                let (t, _) = lastStmtVars (List.head (List.rev elist)) varlist
                (t, varlist')            
            else
                (ec.Token, varlist')
        | C.Expr.IntLiteral(ec, bi) -> 
            ec.Token, varlist
        | C.Expr.BoolLiteral(ec, b) -> 
            ec.Token, varlist
        | C.Expr.Deref(ec, e) -> 
            lastStmtVars e varlist 
        | C.Expr.Dot(ec, e, f) -> 
            lastStmtVars e varlist
        | C.Expr.Index(ec, e1, e2) -> 
            let t, vlist' = lastStmtVars e1 varlist
            lastStmtVars e2 vlist'
        | C.Expr.Cast(ec, cs, e) -> lastStmtVars e varlist
        | C.Expr.Quant(ec, qd) -> (* CLG fixme *)
            let b = qd.Body 
            lastStmtVars b varlist
        | C.Expr.Result(ec) -> 
            ec.Token, varlist
        | C.Expr.Old(ec, e1, e2) -> 
            let t, vlist' = lastStmtVars e1 varlist
            lastStmtVars e2 vlist'
        | C.Expr.VarDecl(ec, v) -> 
            ec.Token, varlist
        | C.Expr.VarWrite(ec, vlist, e) -> 
            lastStmtVars e (varlist @ vlist) (* FIXME *)
        | C.Expr.MemoryWrite(ec, e1, e2) -> (* CLG this memory write probably updates a var - do something different for that? *)
            let t, vlist' = lastStmtVars e1 varlist
            lastStmtVars e2 vlist'
        | C.Expr.If(ec, e1, e2, e3) -> 
            let t, vlist' = lastStmtVars e1 varlist
            let t', vlist'' = lastStmtVars e2 vlist'
            lastStmtVars e3 vlist''
        | C.Expr.Loop(ec, elist1, elist2, e) -> 
            lastStmtVars e varlist
        | C.Expr.Goto(ec, lid) -> ec.Token, varlist
        | C.Expr.Label(ec, lid) -> ec.Token, varlist
        | C.Expr.Block(ec, elist) -> 
            if not (List.isEmpty elist) then
                let vlist = processEList elist varlist
                let revlist = List.rev elist
                let h = List.head revlist
                let tok, vlist' = lastStmtVars h vlist
                (tok, vlist)                
            else (ec.Token, varlist)
        | C.Expr.Assert(ec, e) -> lastStmtVars e varlist
        | C.Expr.Assume(ec, e) -> lastStmtVars e varlist
        | C.Expr.Pure(ec, e) -> lastStmtVars e varlist
        | C.Expr.Return(ec, eopt) -> 
            match eopt with
            | Some(e) -> lastStmtVars e varlist
            | None -> ec.Token, varlist
        | C.Expr.Atomic(ec, elist, e) -> 
            let vlist = processEList elist varlist
            let _, vlist' = lastStmtVars e vlist
            if not (List.isEmpty elist) then
                lastStmtVars (List.head (List.rev elist)) vlist' (* CLG I think elist is the atomic block, right? *)
            else (ec.Token, vlist')
        | C.Expr.Comment(ec, s) -> ec.Token, varlist
        | C.Expr.Stmt(ec, e) -> 
            lastStmtVars e varlist
        | C.Expr.Macro(ec, s, elist) -> 
            if not (List.isEmpty elist) then
                lastStmtVars (List.head (List.rev elist)) varlist
            else (ec.Token, varlist)
        | C.Expr.UserData(ec, o) ->  
            ec.Token, varlist
        | _ -> 
            (body.Token, varlist)

      member this.VarIntro tok (incr : bool) (l:C.Variable) = 
        if helper.Options.PrintCEVModel && (cevPrintVarOK l.Name l.Kind) && (not (cevVarsIntroed.ContainsKey l)) then
            cevVarsIntroed.Add(l, true)
            let pos = getTokenConst tok 
            let name = "#loc." + l.Name
            registerToken name
            let valIs v = bCall "#cev_var_intro" [B.Expr.IntLiteral(bigint.Parse((!n).ToString())); (loc_or_glob l.Kind); er name; v; ctx.ToTypeId l.Type] 
            let cond =       
                match l.Type with
                | C.Ptr _ ->  
                    let v' = addType l.Type (varRef l)
                    (valIs (bCall "$ptr_to_int" [v']))
                | _ -> valIs (ctx.CastToInt (ctx.TrType l.Type) (varRef l))
            if incr then
                let assume = [cevSavePos !n pos; B.Stmt.Assume cond]
                cevNIncr ();                  
                assume                
            else [B.Stmt.Assume cond; ]
        else []

      member this.InitCall tok = 
        if helper.Options.PrintCEVModel then
            let pos = getTokenConst tok
            let valIs = B.Stmt.Assume(bCall "#cev_init" [B.Expr.IntLiteral(bigint.Parse((!n).ToString()))])
            let posSave = cevSavePos !n pos
            cevNIncr ()
            let name = "#loc.$s"
            registerToken name
            let intro_state = [B.Stmt.Assume (bCall "#cev_var_intro" [B.Expr.IntLiteral(bigint.Parse((!n).ToString())); (er "cev_implicit"); er name; er "$s"; er "^$#state_t"])]
            let posSave2 = cevSavePos !n pos
            let assume = [valIs; posSave;] @ intro_state @ [posSave2]
            cevNIncr ();
            assume
        else []

      member this.StateUpdate tok =
        if ctx.Helper.Options.PrintCEVModel then
            let pos = getTokenConst tok
            let valIs = bCall "#cev_var_update" [B.Expr.IntLiteral(bigint.Parse((!n).ToString())); (er "cev_implicit"); er "#loc.$s"; er "$s"]
            let assume = [cevSavePos !n pos; B.Stmt.Assume valIs]
            cevNIncr ();
            assume
        else []

      member this.VarUpdate tok (incr : bool) (l:C.Variable) =
        if ctx.Helper.Options.PrintCEVModel && (cevPrintVarOK l.Name l.Kind) then
            if cevVarsIntroed.ContainsKey l then 
                let pos = ctx.GetTokenConst tok 
                let name = "#loc." + l.Name
                ctx.RegisterToken name (* CLG - things are different for pointer updates! *)
                let valIs suff v = bCall ("#cev_var_update" + suff) [B.Expr.IntLiteral(bigint.Parse((!n).ToString())); (loc_or_glob l.Kind); er name; v]
                let cond =
                    match l.Type with
                    | C.Ptr _ -> 
                        let v' = ctx.AddType l.Type (varRef l)
                        (valIs "_ptr" (bCall "$ptr_to_int" [v']))
                    | _ -> valIs "" (ctx.CastToInt (ctx.TrType l.Type) (varRef l))
                let assume = [cevSavePos !n pos; B.Stmt.Assume cond]
                if incr then cevNIncr ()
                [B.Stmt.Assume cond;]
            else this.VarIntro tok incr l
        else []

      member this.VarUpdateList tok (l:C.Variable list) = // fixme: make sure we save state after this and increment n
        if helper.Options.PrintCEVModel then
            match l with
            | v :: vs -> if cevPrintVarOK v.Name v.Kind then (this.VarUpdate tok false v) @ (this.VarUpdateList tok vs) else (this.VarUpdateList tok vs)  
            | [] -> cevNIncr(); []
        else []

      member this.RegLoopBody (stmt : C.Expr) body = 
        if helper.Options.PrintCEVModel then
          let pos1 = getTokenConst stmt.Token
          let valIs = bCall "#cev_control_flow_event" [B.Expr.IntLiteral(bigint.Parse((!n).ToString())); er "loop_register"]
          let retval = [B.Stmt.Assume valIs; cevSavePos !n pos1]
          retval
        else []    
        
      member this.CondMoment tok =
        if helper.Options.PrintCEVModel then
          let pos = getTokenConst tok
          let valIs = bCall "#cev_control_flow_event" [B.Expr.IntLiteral(bigint.Parse((!n).ToString())); er "conditional_moment"; ] 
          let retval = [B.Stmt.Assume valIs; cevSavePos !n pos]
          cevNIncr (); retval
        else []
      
      member this.FunctionCall tok = (* put in updates for stuff referenced in args, I think *)
        if helper.Options.PrintCEVModel then
            let pos = getTokenConst tok
            let valIs = bCall "#cev_function_call" [B.Expr.IntLiteral(bigint.Parse((!n).ToString()))]
            let retval = [B.Stmt.Assume valIs; cevSavePos !n pos]
            cevNIncr ();
            retval
        else []
            
      member this.BranchChoice tok branchTaken =
        if helper.Options.PrintCEVModel then
            let pos = getTokenConst tok
            let valIs = bCall "#cev_control_flow_event" [B.Expr.IntLiteral(bigint.Parse((!n).ToString())); branchTaken; ] 
            let retval = [B.Stmt.Assume valIs; cevSavePos !n pos]
            cevNIncr ();
            retval
        else []
      
      member this.UpdateLastStmtVars tok stmt =
        let _, vlist = lastStmtVars stmt []
        this.VarUpdateList tok vlist
  
