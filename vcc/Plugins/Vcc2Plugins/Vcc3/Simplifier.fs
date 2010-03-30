//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

open Microsoft.Research.Vcc
open Microsoft.Research.Vcc.Util
open Microsoft.Research.Vcc3.Ast


type Abstraction =
  {
    FunAsk : FuncDecl
    FunDone : FuncDecl
    Body : QuantData 
  }
  
type StackState =
  {
    mutable RetTok : option<Token>
    mutable ErrorHandler : string -> unit
    mutable Final : bool
    mutable Abstractions : list<Abstraction>
  }

exception ProvingFailure

type Simplifier(helper:Helper.Env, pass:FromBoogie.Passyficator, options:Options) =
  let smt = new Z3Translator(helper, pass)
  let mutable disposed = false
  let trLevel = options.GetInt "trace" 1
  let vcc2 = not helper.Options.Vcc3
  let wr = printfn
  let mutable stack =
    let noErr s = failwith ("no error handler installed " + s)
    let emptyState =
      {
        RetTok = None
        ErrorHandler = noErr
        Final = true
        Abstractions = []
      }
    [ emptyState ]
  let mutable errCnt = 0
  let mutable id = 1000000000
  let functions = gdict()
  do 
    for f in pass.Functions do
      functions.Add (f.Name + f.Qualifier, f)
      
  member this.NextId () =
    id <- id + 1
    id
  
  member private this.Function name =
    functions.[name]
    
  interface System.IDisposable with
    member this.Dispose() =
      if not disposed then
        disposed <- true
        (smt :> System.IDisposable).Dispose()
  
  member private this.Cur = stack.Head    
  
  member private this.Push () =
    let c = this.Cur
    stack <- { c with RetTok = c.RetTok } :: stack
    smt.Push()
    if trLevel >= 2 then
      wr "{ Push"
    
  member private this.Pop () =
    smt.Pop()
    stack <- stack.Tail
    if trLevel >= 2 then
      wr "Pop }"
  
  member private this.Dump () =
    wr "DUMP %s" (smt.SmtToFile())
  
  member private this.Fail (expr:Expr, reason:string) =
    if this.Cur.Final then
      let app = if reason = null || reason = "" then "" else ": " + reason
      errCnt <- errCnt + 1
      if trLevel >= 2 then
        this.Dump()
      this.Cur.ErrorHandler null
      if trLevel >= 1 then
        wr "  Cannot prove %O%s" expr app
      if helper.Options.SaveModel then
        smt.SaveModel (options.GetString "MODEL_FILE" "error.vccmodel")
    false
    
  member private this.Fail (expr:Expr) =
    this.Fail (expr, "")
  
  member private this.Succeed () = true
  
  member private this.NestedValid negate expr =
    this.Push ()
    let res =
      try
        this.Cur.Final <- false
        this.Valid negate expr
      finally
        this.Pop()
    if res then
      let expr = Expr.MkNotCond negate expr
      this.LogAssume expr
    res
  
  member private this.RemoveBuiltins (expr:Expr) =
    let killBuiltin = function
      | Binder q as t when (hasAttr "builtin" q.Attrs || hasAttr "todo" q.Attrs || hasAttr "L1" q.Attrs) ->
        // wr "killing %O" t
        Expr.True
      | e -> e
    expr.Weaken killBuiltin
  
  member private this.MakeTempFun basename argTypes retType =
    let id = this.NextId()
    {
      Id = id
      Name = basename + "@" + id.ToString()
      Qualifier = ""
      RetType = retType
      ArgTypes = argTypes
      Attrs = []
      Body = Uninterpreted
    }
          
  member private this.Abstract (expr:Expr) =
    let aux = function
      | Binder q when hasAttr "L1" q.Attrs ->
        let fn = this.MakeTempFun "inst" (q.Vars |> List.map (fun v -> v.Typ)) Type.Bool
        let fnDone = 
          { fn with
              Id = this.NextId()
              Name = "instDone@" + fn.Id.ToString()
          }        
        smt.DeclareTempFunction fn
        smt.DeclareTempFunction fnDone
        let abstraction = { FunAsk = fn ; FunDone = fnDone ; Body = q }
        this.Cur.Abstractions <- abstraction :: this.Cur.Abstractions
        let abstracted = Binder { q with Attrs = [] ; Body = App (fn, List.map Ref q.Vars) }
        if trLevel >= 2 then
          wr "  abstract %O -> %O" (Binder q) abstracted
        Some abstracted 
      | _ -> None
    expr.Map aux
  
  member this.Simplify (expr:Expr) =
    let aux (self: Expr -> Expr) = function
      | Expr.App ({ Name = "select@" }, [Binder q; e]) ->
        if q.Kind <> QuantKind.Lambda then failwith ""
        match q.Vars with
          | [v] ->
            let repl = function
              | Expr.Ref v' when v.Id = v'.Id -> Some e
              | _ -> None
            Some ((self q.Body).Map repl)
          | _ -> None
      | _ -> None
    if trLevel >= 5 then
      wr "[SIMPL] %O" expr
    expr.SelfMap aux
    
  member this.ProverRewrites (expr:Expr) =
    expr.Expand() |> this.Abstract |> this.RemoveBuiltins |> this.Simplify |> this.UnLambda
    
  member private this.IntAssume (expr:Expr) =
    smt.Assume (this.ProverRewrites expr)
    
  member private this.LogAssume (expr:Expr) =
    if trLevel >= 3 then
      wr "[SMT] assume %O" expr
    this.IntAssume expr
    
  member private this.Assume (expr:Expr) =
    if trLevel >= 10 then
      wr "[SMT] assume(any) %O" expr
    this.IntAssume expr
  
  member private this.UnLambda (expr:Expr) =
    let aux self = function 
      | Expr.Binder ({ Kind = Lambda } as q) as expr ->
        let typ = expr.Type
        let vars = glist[]
        let mapping = gdict()
        let body:Expr = self q.Body
        let freshen (v:Var) = 
          let v' = { v with Id = this.NextId() }
          mapping.Add (v.Id, v')
          v'
            
        let idxs = [for v in q.Vars -> freshen v]
        let replBound = function
          | Expr.Ref v when mapping.ContainsKey v.Id ->
            Some (Expr.Ref mapping.[v.Id])
          | Expr.Ref v ->
            vars.Add v
            Some (Expr.Ref (freshen v))
          | _ -> None
        let body = body.Map replBound
        let originals = [for v in vars -> Expr.Ref v]
        let vars = [for v in vars -> mapping.[v.Id]]
        let fn = this.MakeTempFun "lambda" (vars |> List.map (fun v -> v.Typ)) typ
        let fnCall = App (fn, List.map Expr.Ref vars)
        let sel = this.App ("select@" + typ.ToString()) (fnCall :: List.map Expr.Ref idxs)
        let eq = this.Eq sel body
        let axq =
          {
            Kind = Forall
            Vars = vars @ idxs
            Triggers = [[ sel ]]
            Attrs = q.Attrs
            Body = eq
          }
        smt.DeclareTempFunction fn
        let expr = Expr.Binder axq
        if trLevel >= 3 then
          wr "[SMT] lambda assume %O" expr
        smt.Assume expr
        Some (App (fn, originals))
      | _ -> None
    let res = expr.SelfMap aux
    res
      
    
  member private this.App name args =
    App (this.Function name, args)
  
  member private this.Const name =
    this.App name []
  
  member private this.Eq (a:Expr) b =
    this.App ("==@" + a.Type.ToString()) [a; b]
  
  member private this.Goalize (expr:Expr) =    
    let isPtr = function
      | Type.Named { Name = "$ptr" } -> true
      | _ -> false
      
    let eq = this.Function "==@$ptr"
    let goal = this.Function "$goal"
    let dot = this.Function "$dot"
    let doAssumeGoal e = 
      if trLevel >= 4 then
        wr "    [GOAL] %O" e
      smt.Assume (this.Eq (this.App "$goal" [e]) e)
    let assumeGoal = function
      | App ({ Name = "$dot" }, [_; App ({ Name = "$f_typed" }, _)]) as e ->
        doAssumeGoal e
      | e ->
        doAssumeGoal e
        doAssumeGoal (this.App "$dot" [e; this.Const "$f_typed"])
    let aux self = function
      | Binder _ as expr -> Some expr // don't go inside binders
      | expr ->
        if isPtr expr.Type then
          assumeGoal expr
        None
    expr.SelfMap aux |> ignore
        
  member private this.SmtValid negate expr =
    let expr = Expr.MkNotCond negate expr
    smt.Push()
    let toCheck = expr.Expand() |> this.RemoveBuiltins
    if trLevel >= 3 then
      wr "[SMT] assert %O" toCheck
    this.Goalize toCheck
    match smt.AssertOrModel toCheck with
      | Some model ->
        let mutable numInst = 0
        for abstr in this.Cur.Abstractions do
          for args in smt.GetAppsExcluding (model, abstr.FunAsk, abstr.FunDone) do
            numInst <- numInst + 1
            let sub = List.fold2 (fun sub (v:Var) t -> Map.add v.Id t sub) Map.empty abstr.Body.Vars args
            let repl = function
              | Ref v when sub.ContainsKey v.Id -> Some sub.[v.Id]
              | _ -> None            
            let varRefs = List.map Expr.Ref abstr.Body.Vars
            let precond = Expr.App (abstr.FunAsk, varRefs)
            let outcome = Expr.App (abstr.FunDone, varRefs)
            if trLevel >= 4 then
              wr "[INST] %O [%s]" (Binder abstr.Body) (objConcat ", " args)
            let assump = Expr.MkImpl (precond, Expr.MkAnd (outcome, abstr.Body.Body))
            smt.Assume (assump.Map repl |> this.ProverRewrites)
        model.Dispose()
        if numInst = 0 then
          if trLevel >= 3 then
            wr "[SMT] Fail"
          let res = this.Fail expr
          smt.Pop()
          res
        else
          if trLevel >= 2 then
            wr "[SMT] Instantiated %d" numInst
          this.SmtValid negate expr
      | None ->
        if trLevel >= 3 then
          wr "[SMT] OK"
        if trLevel >= 4 then
          this.Dump()
        smt.Pop()
        this.Assume expr
        this.Succeed()
  
  member private this.Valid negate (expr:Expr) =
    let goalize (e:Expr) = e.Expand() |> this.Goalize
    let neg = Expr.MkNotCond negate
    let validOr a b =
      if this.NestedValid negate a then
        true
      else
        this.Push()        
        let res =
          try
            goalize a
            this.LogAssume (Expr.MkNotCond (not negate) a)
            this.Valid negate b
          finally
            this.Pop()
        // if res then this.LogAssume (Expr.MkOr (neg a, neg b))
        res
    
    let passToSMT expr = this.SmtValid negate expr
        
    match expr with      
      | PAnd (a, b) when not negate ->
        this.Valid negate a && this.Valid negate b
      | POr (a, b) when negate ->
        this.Valid negate a && this.Valid negate b
      | PAnd (a, b) when negate ->
        validOr a b
      | POr (a, b) when not negate ->
        validOr a b
      | PNot (a) ->
        this.Valid (not negate) a
        
      | PIte (a, b, c) ->
        if this.NestedValid false a then          
          this.Valid negate b
        else if this.NestedValid true a then
          this.Valid negate c
        else
          this.Push()
          this.LogAssume a
          goalize a
          let r1 = this.Valid negate b
          this.Pop()
          if r1 then
            this.Push()
            this.LogAssume (Expr.MkNot a)
            goalize a
            let r2 = this.Valid negate c
            this.Pop()
            r2
          else false
          
      | App (fn, args) ->
        match fn.Body with
          | Expand _ ->
            this.Valid negate (expr.Apply())
          //| ImpliedBy _ when not negate ->
          //  this.Valid negate (expr.Apply())
          | _ ->
            passToSMT expr
            
      | Binder q ->        
        if (negate && q.Kind = Exists) || (not negate && q.Kind = Forall) then
          let vars = gdict()
          for v in q.Vars do
            let fn = this.MakeTempFun ("sk@" + v.Name) [] v.Typ
            smt.DeclareTempFunction fn
            vars.Add (v.Id, App (fn, []))
          let sk_hack = function
            | [App ({ Name = "sk_hack" }, [arg])] -> [arg]
            | _ -> []
          let body = q.Body.Subst vars
          match q.Triggers |> List.collect sk_hack with
            | [] -> this.Valid negate body
            | hacks ->          
              this.Push()
              try
                for h in hacks do
                  let body = (h.Subst vars).Expand()
                  if trLevel >= 3 then
                    wr "[SK_HACK] %O" body
                  goalize body
                  smt.Assume (this.App "$instantiate_bool" [body])
                this.Valid negate body
              finally
                this.Pop()
        else
          passToSMT expr
        
      | Lit (Lit.Bool v) ->
        if v <> negate then
          this.Succeed ()
        else
          passToSMT expr
          
      | Ref v ->
        passToSMT expr
        
      // type error
      | Lit (Lit.Int _)
      | Lit (Lit.Bv _) ->
        failwith ""
        
  member this.Init () =
    smt.Init()
    
    let isBareVars vars args =
      let vv = gdict()
      for (v:Var) in vars do vv.Add (v.Id, true)
      let check = function
        | Ref v when vv.ContainsKey v.Id -> vv.Remove v.Id |> ignore; true
        | _ -> false
      List.length vars = List.length args && List.forall check args
      
    for a in pass.Axioms do
      let stripRef = function
        | Ref v -> v
        | _ -> failwith ""
      // TODO: handle more than a single implication axiom per function
      match a.Body with
        | PForall (_, vars, PApp (_, "==", [App (fn, args); expr])) when not vcc2 && isBareVars vars args ->
          if fn.Body = Uninterpreted then
            fn.Body <- ImpliedBy (List.map stripRef args, expr)
        | PForall (_, vars, POr (expr, App (fn, args))) when not vcc2 && isBareVars vars args ->
          if fn.Body = Uninterpreted then
            fn.Body <- ImpliedBy (List.map stripRef args, Expr.MkNot expr)
        | _ -> ()
        
      this.Assume a.Body
 
  member this.VerifyProc (proc:BlockProc, handler:Microsoft.Boogie.VerifierCallback) =
    if trLevel >= 4 then
      for f in pass.Functions do wr "%O" f
      for f in pass.Axioms do wr "%O" f
        
    if trLevel >= 3 then
      for b in proc.Blocks do wr "%O" b
                
    let prevErrs = errCnt
    
    let rec check (b:Ast.Block) =
      let cur = this.Cur
      for c in b.Cmds do
        match c with
          | Ast.Assert (tok, cond) ->
            match cond with
              | Ast.App ({ Name = "$position_marker" }, [] ) ->
                cur.RetTok <- Some (tok :> Token)
              | _ -> ()
            if trLevel >= 2 then
              wr "*** assert %O" cond
            
            let err0 = errCnt
            this.Push()
            this.Cur.ErrorHandler <- fun str -> handler.OnCounterexample (tok.GetCounterexample(cur.RetTok), str)            
            let valid = this.Valid false cond
            this.Pop()
            
            if not valid && err0 = errCnt then failwith ""
            
            this.Assume cond // subsumption
            
          | Ast.Assume (_, cond) ->
            if trLevel >= 2 then
              wr "*** assume %O" cond
            if trLevel >= 3 then
              wr "    -----> %O" (this.ProverRewrites cond)
            this.Assume cond
      
      match b.Exits with
        | [] -> ()
        | [e] -> check e
        | _ ->
          for e in b.Exits do
            this.Push()
            check e
            this.Pop()
            
    smt.BeginProc proc
    check proc.Blocks.Head
    smt.FinishProc ()
    
    errCnt - prevErrs
    