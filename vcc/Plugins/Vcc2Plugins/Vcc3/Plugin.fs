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

open System.IO

[<System.ComponentModel.Composition.Export("Microsoft.Research.Vcc.VCGenPlugin")>]
type Vcc3Plugin() =
  inherit VCGenPlugin()
  let opts = Options()
  let mutable methodNo = 0
  
  override this.Name = "Vcc3"
  
  override this.UseCommandLineOptions o =
    Seq.iter opts.Set o
  
  override this.VerifyImpl (helper, vcgen, impl, prog, handler) =
      
    if methodNo = 0 then
      let filename =
        if helper.Options.FileNames.Count = 0 then "testcase"
        else helper.Options.FileNames.[0]
      if helper.Options.SaveModel then
        let name = Path.ChangeExtension (filename, "vccmodel")
        if File.Exists name then
          File.Delete name
        opts.Set ("MODEL_FILE=" + name)        
    methodNo <- methodNo + 1
    
    let pass = FromBoogie.Passyficator(prog, helper, [])
    pass.Init()
      
    use simpl = new Simplifier (helper, pass, opts)
    simpl.Init ()
    let proc = pass.Passify impl
    let numErrs = simpl.VerifyProc (proc, handler)
    
    if numErrs > 0 then
      VC.VCGen.Outcome.Errors
    else
      VC.VCGen.Outcome.Correct
  
