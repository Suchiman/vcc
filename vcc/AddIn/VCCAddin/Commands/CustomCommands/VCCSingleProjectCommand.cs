//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCSingleProjectCommand : VCCCommand {

    public VCCSingleProjectCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCSingleProject", "Verify single project", "Verifies a single project", (int)VCCMenuIcons.SingleProject, null, CommandBarName.Project) {
    }

    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;

      Project prj = getSelectedProject();
      VerifyManager.Init();

      Utilities.NavigateProject(prj);

      VerifyManager.Execute();
      return true;
    }

    private Project getSelectedProject() {
      SelectedItems selItems = DTE.SelectedItems;
      SelectedItem sItem = selItems.Item(1);   //Its not possible to mark more than one project!!! (At this time)
      return sItem.Project;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {
      if (!VerifyManager.isRunning && Utilities.IsVCPrj(getSelectedProject())) {
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
        commandText = "Verify Project";
      }
      else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }
    }

  }
}
