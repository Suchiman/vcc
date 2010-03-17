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
        let tr = false
        
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
        
        if tr then
          for b in proc.Blocks do wr b
        
        use z3 = new Z3Translator(opt)
        z3.Init ()
        
        let numErrs = ref 0
        
        let rec check retTok (b:Ast.Block) =
          let mutable retTok = retTok
          for c in b.Cmds do
            match c with
              | Ast.Assert (tok, cond) ->
                match cond with
                  | Ast.App ({ Name = "$position_marker" }, [] ) -> retTok <- Some (tok :> Token)
                  | _ -> ()
                if tr then
                  wr ("assert " + cond.ToString())
                let cond' = cond.Expand()
                z3.Push()
                let res = z3.Assert (cond')
                
                if not res then
                  if tr then
                    wr ("DUMP " + z3.SmtToFile())
                  handler.OnCounterexample (tok.GetCounterexample(retTok), null)
                  incr numErrs
                z3.Pop()
                z3.Assume cond' // subsumption
              | Ast.Assume (_, cond) ->
                if tr then
                  wr ("assume " + cond.ToString())
                z3.Assume (cond.Expand())
          
          match b.Exits with
            | [] -> ()
            | [e] -> check retTok e
            | _ ->
              for e in b.Exits do
                if tr then
                  wr ("{ Push")
                z3.Push()
                check retTok e
                z3.Pop()
                if tr then
                  wr ("Pop }")
        
        z3.BeginProc proc
        check None proc.Blocks.Head
        z3.FinishProc ()

        if !numErrs > 0 then
          VC.VCGen.Outcome.Errors
        else
          VC.VCGen.Outcome.Correct
      
