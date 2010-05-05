//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCSingleFileCommand : VCCCommand{
  
    public VCCSingleFileCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCSingleFile", "Verify single &File", "Verifies a single file", (int)VCCMenuIcons.SingleFile, VCCBindings.SingleFile, CommandBarName.CodeWindow, CommandBarName.VCCMain) { 
    }
    
    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.SingleFileVerify));
      VerifyManager.Execute();
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {      
      
      if (AddInGlobals.ActiveDocument.IsCodeFile && (!VerifyManager.isRunning)) {
        commandText = "Verify &file '" + AddInGlobals.ActiveDocument.FileName + "'";
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      }
      else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }           
    }

  }
}
