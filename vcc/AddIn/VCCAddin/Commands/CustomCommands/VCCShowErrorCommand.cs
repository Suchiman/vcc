//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using EnvDTE;
using EnvDTE80;

// This is in invisible command, that is used internaly for callback functionality

namespace VerifiedCCompilerAddin.Commands {
  public class VCCShowErrorCommand : VCCCommand {

    public VCCShowErrorCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "showVCCErrors", "Verify single solution", "Verifies a entry solution", (int)VCCMenuIcons.SingleSolution,null,null) {
    }                   

    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      string[] Args = ((string)varIn).Split('ê');
      AddInGlobals.VCCMarkerManger.CreateMarker(Args[0], Args[2], Args[3], Convert.ToInt32(Args[1]), false);
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted nkeededText, ref vsCommandStatus status, ref object commandText) {
      status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      commandText = "Verification Errors";
    }

  }
}
