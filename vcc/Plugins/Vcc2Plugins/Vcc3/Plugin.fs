//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

    open Microsoft.FSharp.Math
    open Microsoft
    
    open Microsoft.Research.Vcc
    open Microsoft.Research.Vcc.Util

    [<System.ComponentModel.Composition.Export("Microsoft.Research.Vcc.VCGenPlugin")>]
    type Vcc3Plugin() =
      inherit VCGenPlugin()
      
      override this.Name = "Vcc3"
      override this.VerifyImpl (helper, vcgen, impl, prog, handler) =
        let wr (s:obj) = System.Console.WriteLine s
        
        
        let opt = FromBoogie.Passyficator(prog, helper, [])
        opt.Init()
        
        (*
        for a in opt.Functions do
          wr a
        for a in opt.Axioms do
          wr a
        *)
          
        let proc = opt.Passify impl
        
        //for b in proc.Blocks do wr b
        
        use z3 = new Z3Translator(opt)
        z3.Init ()
        
        let rec check (b:Ast.Block) =
          for c in b.Cmds do
            match c with
              | Ast.Assert (tok, cond) ->
                //wr (cond.ToString())
                z3.Push()
                let res = z3.Assert (cond.Expand())
                //let fn = z3.SmtToFile()
                if not res then
                  handler.OnCounterexample (tok.GetCounterexample(new Microsoft.Boogie.BlockSeq()), null)
                z3.Pop()
              | Ast.Assume (_, cond) ->
                z3.Assume (cond.Expand())
          
          match b.Exits with
            | [] -> ()
            | [e] -> check e
            | _ ->
              for e in b.Exits do
                z3.Push()
                check e
                z3.Pop()
        
        z3.BeginProc proc
        check proc.Blocks.Head
        z3.FinishProc ()

        VC.VCGen.Outcome.Inconclusive
      
