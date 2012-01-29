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
  let opts = Options.Create()
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
    let proc = pass.Passify impl

    if opts.simplify then
      use simpl = new Simplifier (helper, pass, opts)
      simpl.Init ()
      let numErrs = simpl.VerifyProc (proc, handler)

      if numErrs > 0 then
        VC.VCGen.Outcome.Correct
      else
        VC.VCGen.Outcome.Errors
    else
      let prog', impl = ToBoogie.Translate (pass, proc, opts.boogie_file)
      Boogie.LambdaHelper.ExpandLambdas(prog')
      vcgen.program <- prog'
      vcgen.VerifyImplementation (impl, prog', handler)
  
