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
        
        use z3 = new Z3Translator()
        
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
        
        z3.Init opt
        
        let rec check (b:Ast.Block) =
          for c in b.Cmds do
            if c.IsAssert then
              wr (c.Condition.ToString())
              let res = z3.Assert c.Condition
              if res then
                wr "   OK"
              else
                wr "   FAILED"
            else
              z3.Assume c.Condition
          
          match b.Exits with
            | [] -> wr "DONE"
            | [e] -> check e
            | _ ->
              for e in b.Exits do
                z3.Push()
                check e
                z3.Pop()
        
        check proc.Blocks.Head

        VC.VCGen.Outcome.Inconclusive
      
