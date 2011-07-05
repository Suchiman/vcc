using EnvDTE;
//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  class VCCSingleSolutionCommand : VCCCommand{


    public VCCSingleSolutionCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCSingleSolution", "Verify single solution", "Verifies a entry solution", (int)VCCMenuIcons.SingleSolution, null, CommandBarName.Solution) {
    }

    public override bool Exec(vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;

      Solution Sln = AddInGlobals.DTE.Solution;
      VerifyManager.Init();

      foreach (Project prj in Sln.Projects) {
        Utilities.NavigateProject(prj);
      }

      VerifyManager.Execute();
      return true;
    }

    public override void QueryStatus(vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText) {
      if (!VerifyManager.isRunning && Utilities.ContainsVCPrjType(AddInGlobals.DTE.Solution.Projects)) {
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
        commandText = "Verify Solution";
      }
      else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }
    }
  }
}
