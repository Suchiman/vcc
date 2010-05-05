using EnvDTE;
//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  class VCCSwapAssumeToAssert : VCCCommand {
    public VCCSwapAssumeToAssert(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCSwapAssumeToAssert", "&Swap Assume to Assert", "Assume to Assert", (int)VCCMenuIcons.Dummy, VCCBindings.AssumeToAssert, CommandBarName.VCCMoreCommands) { 
    }

    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      ReplaceAssumeToAssert();
      return true;
    }

    private void ReplaceAssumeToAssert() {
      AddInGlobals.ActiveDocument.replaceSelectedText("assume", "assert");
    }

    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {

      if (AddInGlobals.ActiveDocument.IsCodeFile && !VerifyManager.isRunning)
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      else
        status = vsCommandStatus.vsCommandStatusInvisible;
    }
  }
}
