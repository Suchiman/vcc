//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands  {
  public class VCCSingleFileFunctionCommand : VCCCommand{

    public VCCSingleFileFunctionCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCSingleFunction", "Verify single function", "Verifies a single function in a file", (int)VCCMenuIcons.SingleFileFunction, VCCBindings.SingleFileFunction, CommandBarName.CodeWindow) { 
    }
        
    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;

      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.SingleFileFunction));
      VerifyManager.Execute();
      
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {
      if (AddInGlobals.ActiveDocument.IsInFunction && (!VerifyManager.isRunning)) {
        commandText = "Verify function '" + AddInGlobals.ActiveDocument.CurrentFunctionName + "'";
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
        return;
      } 
      
      if (AddInGlobals.ActiveDocument.getSelectedText() != string.Empty) {
        commandText = "Verify function '" + AddInGlobals.ActiveDocument.getSelectedText() + "'";
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
        return;
      }

      status = vsCommandStatus.vsCommandStatusInvisible;
    }
  }
}
