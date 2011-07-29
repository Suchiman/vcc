using EnvDTE;
//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCCustomFileCommand : VCCCommand{
    public VCCCustomFileCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCCustomSingleFile", "Verify single File", "Verifies a single file", (int)VCCMenuIcons.CustomSingleFile, VCCBindings.CustomSingleFile, CommandBarName.CodeWindow) {
      }

    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      
      handled = true;
      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.CustomVerify));
      VerifyManager.Execute();

      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {      
      if (AddInGlobals.ActiveDocument.IsCodeFile && (!VerifyManager.isRunning)) {
        commandText = "Custom verify file '" + AddInGlobals.ActiveDocument.FileName + "'";
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      }
      else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }           
    }

  }
}
