//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

    open Microsoft.Research.Vcc
    open Microsoft.FSharp.Math
    open CAST
    
    open Microsoft
    open Microsoft.Research.Vcc.Util
    open Microsoft.Research.Vcc.BoogieAST
    open Microsoft.Research.Vcc.ToBoogieAST

    [<System.ComponentModel.Composition.Export("Microsoft.Research.Vcc.VCGenPlugin")>]
    type Vcc3Plugin() =
      inherit VCGenPlugin()
      
      override this.Name = "Vcc3"
      override this.VerifyImpl (helper, vcgen, impl, prog, handler) =
        let wr (s:obj) = System.Console.WriteLine s
        
        
        wr ""
        
        let opt = FromBoogie.Passyficator(prog, helper, [])
        opt.Init()
        
        (*
        for a in opt.Functions do
          wr a
        for a in opt.Axioms do
          wr a
        *)
          
        let proc = opt.Passify impl
        
        for b in proc.Blocks do wr b
        
        use z3 = new Z3Translator(opt)
        z3.Init ()
        
        let rec check (b:Ast.Block) =
          for c in b.Cmds do
            if c.IsAssert then
              wr (c.Condition.ToString())
              z3.Push()
              let res = z3.Assert (c.Condition.Expand())
              //let fn = z3.SmtToFile()
              let fn = ""
              if res then
                wr ("   OK " + fn)
              else
                wr ("   FAILED " + fn)              
              z3.Pop()
            else
              z3.Assume (c.Condition.Expand())
          
          match b.Exits with
            | [] -> wr "DONE"
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
      
