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
  }
 
type Simplifier(pass:FromBoogie.Passyficator) =
  let z3 = new Z3Translator(pass)
  let mutable disposed = false
  let mutable trLevel = 10
  let wr = printfn
  let mutable stack = [ { RetTok = None } ]
  
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
  
  member this.Init () =
    z3.Init()
    for a in pass.Axioms do
      z3.Assume (a.Body.Expand())
 
  member this.VerifyProc (proc:BlockProc, handler:Microsoft.Boogie.VerifierCallback) =
    if trLevel >= 3 then
      for b in proc.Blocks do wr "%O" b
                
    let numErrs = ref 0
    
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
            let cond' = cond.Expand()
            
            z3.Push()
            
            let res = z3.Assert cond'
            
            if not res then
              if trLevel >= 2 then
                wr "DUMP %s" (z3.SmtToFile())
              handler.OnCounterexample (tok.GetCounterexample(cur.RetTok), null)
              incr numErrs
              
            z3.Pop()
            
            z3.Assume cond' // subsumption
            
          | Ast.Assume (_, cond) ->
            if trLevel >= 2 then
              wr "assume %O" cond
            z3.Assume (cond.Expand())
      
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
    
    !numErrs
    