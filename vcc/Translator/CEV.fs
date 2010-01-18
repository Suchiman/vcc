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
        
      let addType = ctx.AddType
      let getTokenConst = ctx.GetTokenConst
      let varRef = ctx.VarRef
      let helper = ctx.Helper
      let registerToken = ctx.RegisterToken
      let cevPc = er "$cev_pc"
      let cevStep tok = B.Stmt.Call (tok, [], "$cev_step", [er (getTokenConst tok)])      

      let cevVarsIntroed = gdict()
      let mutable loopId = 0

      let lastStmtVars (body : C.Expr) =
        let vars = glist []
        let tok = ref C.bogusToken
        let app _ (e:C.Expr) =
          if e.Token <> C.bogusToken then
            tok := e.Token
          match e with
          | C.Expr.VarWrite (_, lst, _) ->
            vars.AddRange lst
          | _ -> ()
          true
        body.SelfVisit app
        !tok, vars |> Seq.toList
      
      let valueIs (l:C.Variable) =
        let name = "#loc." + l.Name
        registerToken name
        let converted = 
          match l.Type with
            | C.Ptr _ ->  
              bCall "$ptr_to_int" [addType l.Type (varRef l)]
            | _ -> 
              ctx.CastToInt (ctx.TrType l.Type) (varRef l)
        [cevPc; loc_or_glob l.Kind; er name; converted] 
        
      member this.VarIntro tok (incr : bool) (l:C.Variable) = 
        if helper.Options.PrintCEVModel && cevPrintVarOK l.Name l.Kind && not (cevVarsIntroed.ContainsKey l) then
          cevVarsIntroed.Add(l, true)
          let cond = bCall "#cev_var_intro" (valueIs l @ [ctx.ToTypeId l.Type])
          if incr then
            [B.Stmt.Assume cond; cevStep tok]
          else
            [B.Stmt.Assume cond]
        else []

      member this.InitCall tok = 
        if helper.Options.PrintCEVModel then
          let name = "#loc.$s"
          registerToken name
          
          [B.Stmt.Assume (bCall "#cev_init" [cevPc]);
           cevStep tok;
           B.Stmt.Assume (bCall "#cev_var_intro" [cevPc; er "cev_implicit"; er name; bCall "$state_to_int" [er "$s"]; er "^$#state_t"]);
           cevStep tok]
        else []

      member this.StateUpdate tok =
        if ctx.Helper.Options.PrintCEVModel then
          [B.Stmt.Assume (bCall "#cev_var_update" [cevPc; er "cev_implicit"; er "#loc.$s"; bCall "$state_to_int" [er "$s"]]); 
           cevStep tok]
        else []

      member this.VarUpdate tok (incr : bool) (l:C.Variable) =
        if ctx.Helper.Options.PrintCEVModel && (cevPrintVarOK l.Name l.Kind) then
          if cevVarsIntroed.ContainsKey l then
            let cond = bCall "#cev_var_update" (valueIs l)
            if incr then
              [B.Stmt.Assume cond; cevStep tok]
            else
              [B.Stmt.Assume cond]            
          else this.VarIntro tok incr l
        else []

      member this.VarUpdateList tok (l:C.Variable list) =
        if helper.Options.PrintCEVModel then
          List.map (this.VarUpdate tok false) l @ [[cevStep tok]] |> List.concat
        else []

      member this.RegLoopBody (stmt : C.Expr) body = 
        if helper.Options.PrintCEVModel then
          let prePc = "$cev_pre_pc#" + loopId.ToString()
          loopId <- loopId + 1
          let stmts =
            [B.Stmt.VarDecl ((prePc, B.Type.Int), None);
             B.Stmt.Call (stmt.Token, [prePc], "$cev_pre_loop", [er (getTokenConst stmt.Token)])]
          let inv = B.Primitive ("<", [er prePc; cevPc])
          stmts, inv
        else [], bTrue    
        
      member this.CondMoment tok =
        if helper.Options.PrintCEVModel then
          [B.Stmt.Assume (bCall "#cev_control_flow_event" [cevPc; er "conditional_moment"]);
           cevStep tok]
        else []
      
      member this.FunctionCall tok = (* put in updates for stuff referenced in args, I think *)
        if helper.Options.PrintCEVModel then
          [B.Stmt.Assume (bCall "#cev_function_call" [cevPc]);
           cevStep tok]
        else []
            
      member this.BranchChoice tok branchTaken =
        if helper.Options.PrintCEVModel then
          [B.Stmt.Assume (bCall "#cev_control_flow_event" [cevPc; branchTaken]);
           cevStep tok]
        else []
      
      member this.UpdateLastStmtVars tok stmt =
        let _, vlist = lastStmtVars stmt
        this.VarUpdateList tok vlist
  
