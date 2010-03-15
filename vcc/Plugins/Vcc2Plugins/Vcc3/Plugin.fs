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
        
        use cfg = new Z3.Config()
        use z3 = new Microsoft.Z3.Context(cfg)
        
        let opt = FromBoogie.Passyficator(prog, helper, [])
        opt.Init()
        for a in opt.Functions do
          wr a
        for a in opt.Axioms do
          wr a
          
        let blocks = opt.Passify impl
        
        vcgen.VerifyImplementation(impl, prog, handler)
      
