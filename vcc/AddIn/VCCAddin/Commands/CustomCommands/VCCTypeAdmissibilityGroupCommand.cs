//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCTypeAdmissibilityGroupCommand : VCCCommand {

    public VCCTypeAdmissibilityGroupCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCGroupOnlyAdmissibility", "Verify type admissibility", "Verifies admissibility of a structured type", (int)VCCMenuIcons.SingleFileFunction, VCCBindings.TypeGroupAdmissibility, CommandBarName.CodeWindow)
    {
    }
    
    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.TypeAndGroupAdmissiblityCheck));
      VerifyManager.Execute();
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {
      if (AddInGlobals.ActiveDocument.IsInGroup && AddInGlobals.ActiveDocument.IsInType && (!VerifyManager.isRunning)) {
          commandText = "Check admissibility of '" + AddInGlobals.ActiveDocument.CurrentTypeName+ "::" + AddInGlobals.ActiveDocument.CurrentGroupName+ "'";
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
        return;
      } 

      status = vsCommandStatus.vsCommandStatusInvisible;      
    }
  }
}
