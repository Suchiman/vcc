//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

open Microsoft.Research.Vcc
open Microsoft.Research.Vcc.Util
open Microsoft.Research.Vcc3.Ast


type StackState =
  {
    mutable RetTok : option<Token>
    mutable ErrorHandler : string -> unit
    mutable Final : bool
  }

exception ProvingFailure
 
type Simplifier(pass:FromBoogie.Passyficator) =
  let z3 = new Z3Translator(pass)
  let mutable disposed = false
  let mutable trLevel = 0
  let wr = printfn
  let noErr s = failwith ("no error handler installed " + s)
  let mutable stack = [ { RetTok = None; ErrorHandler = noErr ; Final = true } ]
  let mutable errCnt = 0
  let definingAxiom (q:QuantData) =
    List.tryPick (function ExprAttr ("vcc3def", e) -> Some e | _ -> None) q.Attrs  
    
  interface System.IDisposable with
    member this.Dispose() =
      if not disposed then
        disposed <- true
        (z3 :> System.IDisposable).Dispose()
  
  member private this.Cur = stack.Head    
  
  member private this.Push () =
    let c = this.Cur
    stack <- { c with RetTok = c.RetTok } :: stack
    z3.Push()
    if trLevel >= 2 then
      wr "{ Push"
    
  member private this.Pop () =
    z3.Pop()
    stack <- stack.Tail
    if trLevel >= 2 then
      wr "Pop }"
  
  member private this.ForProver (expr:Expr) =
    let killBuiltin = function
      | Binder q as t when (hasAttr "builtin" q.Attrs || hasAttr "todo" q.Attrs || (definingAxiom q).IsSome) ->
        // wr "killing %O" t
        Expr.True
      | PApp (_, "==", [e; PLambda _]) 
      | PApp (_, "==", [PLambda _; e]) ->
        Expr.True
      | e -> e
    expr.Expand().Weaken killBuiltin
  
  member private this.Fail (expr:Expr, reason:string) =
    if this.Cur.Final then
      let app = if reason = null || reason = "" then "" else ": " + reason
      errCnt <- errCnt + 1
      if trLevel >= 2 then
         wr "DUMP %s" (z3.SmtToFile())
      this.Cur.ErrorHandler null
      wr "  Cannot prove %O%s" expr app
    false
    
  member private this.Fail (expr:Expr) =
    this.Fail (expr, "")
  
  member private this.Succeed () = true
  
  member private this.NestedValid negate expr =
    this.Push ()
    try
      this.Cur.Final <- false
      this.Valid negate expr
    finally
      this.Pop()
  
  member private this.Assume (expr:Expr) =
    z3.Assume (this.ForProver expr)
    
  member private this.Valid negate (expr:Expr) =
    match expr with      
      | PAnd (a, b) when not negate ->
        this.Valid negate a && this.Valid negate b
      | POr (a, b) when negate ->
        this.Valid negate a && this.Valid negate b
      | PAnd (a, b) when negate ->
        this.NestedValid negate a || this.NestedValid negate b
      | POr (a, b) when not negate ->
        this.NestedValid negate a || this.NestedValid negate b
      | PNot (a) ->
        this.Valid (not negate) a
      | PIte (a, b, c) ->
        if this.NestedValid false a then
          this.Valid negate b
        else if this.NestedValid true b then
          this.Valid negate c
        else
          this.Push()
          this.Assume a
          let r1 = this.Valid negate b
          this.Pop()
          if r1 then
            this.Push()
            this.Assume (Expr.MkNot a)
            let r2 = this.Valid negate c
            this.Pop()
            r2
          else false
          
      | App (fn, args) ->
        match fn.Body with
          | DelayExpand _
          | Expand _ ->
            this.Valid negate (expr.Apply())
          | Uninterpreted ->
            if z3.Assert (this.ForProver expr) then
              this.Succeed()
            else
              this.Fail expr
            
      | Binder q ->
        this.Fail expr
      | Lit (Lit.Bool v) ->
        if v <> negate then
          this.Succeed ()
        else
          this.Fail expr
      | Ref v ->
        this.Fail expr
      // type error
      | Lit (Lit.Int _)
      | Lit (Lit.Bv _) ->
        failwith ""
        
  member this.Init () =
    z3.Init()
    
    let isBareVars vars args =
      let vv = gdict()
      for (v:Var) in vars do vv.Add (v.Id, true)
      let check = function
        | Ref v when vv.ContainsKey v.Id -> vv.Remove v.Id |> ignore; true
        | _ -> false
      List.length vars = List.length args && List.forall check args
      
    for a in pass.Axioms do
      match a.Body with
        | PForall (_, vars, PApp (_, "==", [App (fn, args); expr])) when isBareVars vars args ->
          let stripRef = function
            | Ref v -> v
            | _ -> failwith ""
          fn.Body <- DelayExpand (List.map stripRef args, expr)
          this.Assume a.Body
        | _ ->
          this.Assume a.Body
 
  member this.VerifyProc (proc:BlockProc, handler:Microsoft.Boogie.VerifierCallback) =
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
              wr "assert %O" cond
            
            this.Push()
            this.Cur.ErrorHandler <- fun str -> handler.OnCounterexample (tok.GetCounterexample(cur.RetTok), str)            
            let valid = this.Valid false cond
            this.Pop()
            
            // this.Assume cond // subsumption
            
          | Ast.Assume (_, cond) ->
            if trLevel >= 2 then
              wr "assume %O" cond
            this.Assume cond
      
      match b.Exits with
        | [] -> ()
        | [e] -> check e
        | _ ->
          for e in b.Exits do
            this.Push()
            check e
            this.Pop()
            
    z3.BeginProc proc
    check proc.Blocks.Head
    z3.FinishProc ()
    
    errCnt - prevErrs
    