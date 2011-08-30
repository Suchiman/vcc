//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands  {
  public class VCCTypeAdmissibilityTypeOnlyCommand : VCCCommand
  {

    public VCCTypeAdmissibilityTypeOnlyCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCTypeOnlyAdmissibility", "Verify type admissibility", "Verifies admissibility of a structured type", (int)VCCMenuIcons.SingleFileFunction, VCCBindings.TypeOnlyAdmissibility, CommandBarName.CodeWindow)
    {
    }

    
    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.TypeAdmissiblityCheck));
      VerifyManager.Execute();
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {
      if (AddInGlobals.ActiveDocument.IsInType && (!VerifyManager.isRunning)) {
        commandText = "Check admissibility of '" + AddInGlobals.ActiveDocument.CurrentTypeName + "'";
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
          return;
      } 
      
      status = vsCommandStatus.vsCommandStatusInvisible;      
    }
  }
}
