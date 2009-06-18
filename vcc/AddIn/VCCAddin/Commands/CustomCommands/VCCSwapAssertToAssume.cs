//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using EnvDTE80;
using EnvDTE;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {

  class VCCSwapAssertToAssume : VCCCommand {

    public VCCSwapAssertToAssume(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCSwapAssertToAssume", "&Swap Assert to Assume", "Assert to Assume", (int)VCCMenuIcons.Dummy, VCCBindings.AssertToAssume, CommandBarName.VCCMoreCommands) { 
    }

    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;

      ReplaceAssertToAssume();

      return true;
    }

    private void ReplaceAssertToAssume() {
      AddInGlobals.ActiveDocument.replaceSelectedText("assert", "assume");
    }

    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      if (AddInGlobals.ActiveDocument.IsCodeFile && !VerifyManager.isRunning)
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      else
        status = vsCommandStatus.vsCommandStatusInvisible;
    }
  }
}
