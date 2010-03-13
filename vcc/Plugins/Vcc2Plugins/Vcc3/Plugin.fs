//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc

    open Microsoft.Research.Vcc
    open Microsoft.FSharp.Math
    open CAST
    
    open Microsoft
    open Microsoft.Research.Vcc.Util
    open Microsoft.Research.Vcc.BoogieAST
    open Microsoft.Research.Vcc.ToBoogieAST

    type CustomPassyficator (prog:Boogie.Program, helper:Helper.Env, options) =
      inherit ToBoogieAST.Passyficator(prog, helper, options)      
      override this.Optimize (proc:BlockProc) = ()
      
      member this.GetBlocks impl =
        this.Passify impl

    [<System.ComponentModel.Composition.Export("Microsoft.Research.Vcc.VCGenPlugin")>]
    type Vcc3Plugin() =
      inherit VCGenPlugin()
      
      override this.Name = "Vcc3"
      override this.VerifyImpl (helper, vcgen, impl, prog, handler) =
        let z3 = new Microsoft.Z3.Context(Z3.Config())
        let opt = CustomPassyficator(prog, helper, [])
        let proc = opt.GetBlocks impl
        
        vcgen.VerifyImplementation(impl, prog, handler)
      
