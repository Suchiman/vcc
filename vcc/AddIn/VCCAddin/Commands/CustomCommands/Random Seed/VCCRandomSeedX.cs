//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Settings;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  class VCCRandomSeedX  : VCCCommand {

    private readonly int seed;

    public VCCRandomSeedX(DTE2 dte, AddIn addin, int seed) :
      base(dte, addin, String.Format("cmdVCCRandomSeed{0}", seed), CommandCaption(seed), "Init Z3 with Seed X", 0, null, CommandBarName.VCCRandomSeed) {
      this.seed = seed;
    }

    private static string CommandCaption(int seed) {
      if (seed == 9 )
        return  String.Format("Try 0...4");
      else
        return String.Format("Random seed &{0}", seed);   
    }
    
    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      if (AddinSettingsManager.RandomSeedEnabled && AddinSettingsManager.RandomSeed == this.seed) {
        AddinSettingsManager.RandomSeedEnabled = false;
      }
      else
        AddinSettingsManager.RandomSeed = this.seed;
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText)
    {
      if (AddInGlobals.ActiveDocument.IsCodeFile && !VerifyManager.isRunning) {
        if (!AddinSettingsManager.RandomSeedEnabled || AddinSettingsManager.RandomSeed != this.seed) {
          commandText = CommandCaption(seed);
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
        } else {
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported | vsCommandStatus.vsCommandStatusLatched;
        }
      } else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }
    }
  }
}
