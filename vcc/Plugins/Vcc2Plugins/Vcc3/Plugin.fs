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
          
        use simpl = new Simplifier (opt)
        simpl.Init ()
        let proc = opt.Passify impl        
        let numErrs = simpl.VerifyProc (proc, handler)
        
        if numErrs > 0 then
          VC.VCGen.Outcome.Errors
        else
          VC.VCGen.Outcome.Correct
      
