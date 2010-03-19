﻿//-----------------------------------------------------------------------------
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
    mutable DefiningAxioms : Map<int, QuantData>
  }

exception ProvingFailure
 
type Simplifier(helper:Helper.Env, pass:FromBoogie.Passyficator, options:Options) =
  let smt = new Z3Translator(helper, pass)
  let mutable disposed = false
  let trLevel = options.GetInt "trace" 1
  let vcc2 = not helper.Options.Vcc3
  let wr = printfn
  let noErr s = failwith ("no error handler installed " + s)
  let mutable stack = [ { RetTok = None; ErrorHandler = noErr ; Final = true ; DefiningAxioms = Map.empty } ]
  let mutable errCnt = 0
  let definingAxiom (q:QuantData) =
    List.tryPick (function ExprAttr ("vcc3def", e) -> Some e | _ -> None) q.Attrs  
    
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
        smt.SaveModel "model.vccmodel"
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
      | Binder q as t when (hasAttr "builtin" q.Attrs || hasAttr "todo" q.Attrs || (definingAxiom q).IsSome) ->
        // wr "killing %O" t
        Expr.True
      | PApp (_, "==", [e; PLambda _]) 
      | PApp (_, "==", [PLambda _; e]) ->
        Expr.True
      | e -> e
    expr.Weaken killBuiltin
  
  member private this.IntAssume (expr:Expr) =
    let exp = expr.Expand()
    smt.Assume (this.RemoveBuiltins exp)
    
  member private this.LogAssume (expr:Expr) =
    if trLevel >= 3 then
      wr "[SMT] assume %O" expr
    this.IntAssume expr
    
  member private this.Assume (expr:Expr) =
    if trLevel >= 10 then
      wr "[SMT] assume(any) %O" expr
    this.IntAssume expr
    
  member private this.SmtValid negate expr =
    let expr = Expr.MkNotCond negate expr
    if trLevel >= 3 then
      wr "[SMT] assert %O" expr
    smt.Push()
    if smt.Assert (this.RemoveBuiltins (expr.Expand())) then
      if trLevel >= 3 then
        wr "[SMT] OK"
      if trLevel >= 4 then
        this.Dump()
      smt.Pop()
      this.Assume expr
      this.Succeed()
    else
      if trLevel >= 3 then
        wr "[SMT] Fail"
      let res = this.Fail expr
      smt.Pop()
      res
  
  member private this.Valid negate (expr:Expr) =
    let validOr a b =
      if this.NestedValid negate a then
        true
      else
        this.Assume (Expr.MkNotCond (not negate) a)
        this.Valid negate b
    
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
        else if this.NestedValid true b then
          this.Valid negate c
        else
          this.Push()
          this.LogAssume a
          let r1 = this.Valid negate b
          this.Pop()
          if r1 then
            this.Push()
            this.LogAssume (Expr.MkNot a)
            let r2 = this.Valid negate c
            this.Pop()
            r2
          else false
          
      | App (fn, args) ->
        match fn.Body with
          | Expand _ ->
            this.Valid negate (expr.Apply())
          | DelayExpand _ ->
            if vcc2 then
              passToSMT expr
            else
              this.Valid negate (expr.Apply())
          | Uninterpreted ->
            passToSMT expr
            
      | Binder q ->
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
    