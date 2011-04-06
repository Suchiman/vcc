//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCCancelCommand : VCCCommand {

    public VCCCancelCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCCancel", "&Cancel VCC", "Canceling a running verification", (int)VCCMenuIcons.Cancel, VCCBindings.Cancel, CommandBarName.Project, CommandBarName.CodeWindow, CommandBarName.Solution, CommandBarName.Item, CommandBarName.VCCMain) { 
    }

    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      VerifyManager.Cancel();
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {      
      commandText = Caption;
      if (VerifyManager.isRunning) {
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      }
      else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }
    }
  }
}
