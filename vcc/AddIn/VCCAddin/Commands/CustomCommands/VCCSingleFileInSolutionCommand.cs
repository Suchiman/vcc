using EnvDTE;
//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCSingleFileInSolutionCommand  : VCCCommand {

    public VCCSingleFileInSolutionCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCSingleFile2", "Verify single File", "Verifies a single file", (int)VCCMenuIcons.SingleFile, null, CommandBarName.Item) { 
    }
    
    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.SingleFileInSolution));
      VerifyManager.Execute();     
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {
      

      if (AddInGlobals.DTE.SelectedItems.Count != 1 || VerifyManager.isRunning) {
        status = vsCommandStatus.vsCommandStatusInvisible;
        return;
      }

      SelectedItem sitem = AddInGlobals.DTE.SelectedItems.Item(1);      
      commandText = "Verify file '" + sitem.Name + "'";

      if (!sitem.Name.EndsWith(".c"))
      {
        status = vsCommandStatus.vsCommandStatusInvisible;
        return;
      }

      status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;     
    }

  }
}
