//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCLaunchZ3Inspector : VCCCommand {

    public VCCLaunchZ3Inspector(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCLaunchZ3Inspector", "Launch Z3 Inspector", "Launch Z3 Inspector", (int)VCCMenuIcons.Dummy, VCCBindings.LaunchZ3Inspector, CommandBarName.VCCMoreCommands) {
    }
    
    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;      
      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.LaunchZ3Inspector));      
      VerifyManager.Execute();
      return true;
    }


    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      if (!VerifyManager.isRunning)
      {
        // Gruppe
        //if (AddInGlobals.ActiveDocument.IsInGroup) {
        //  commandText = "Launch Z3 Axiom Profiler for admissibility check of '" + AddInGlobals.ActiveDocument.CurrentTypeName + "::" + AddInGlobals.ActiveDocument.CurrentGroupName + "'";
        //  status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
        //  return;
        //}

        // Typ
        //if (AddInGlobals.ActiveDocument.IsInType) {
        //  commandText = "Launch Z3 Axiom Profiler for admissibility check of type '" + AddInGlobals.ActiveDocument.CurrentTypeName + "'";
        //  status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
        //  return;
        //}

        // Funktion
        if (AddInGlobals.ActiveDocument.IsInFunction)
        {
          commandText = "Launch Z3 Inspector for '" + AddInGlobals.ActiveDocument.CurrentFunctionName + "'";
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
          return;
        }

        if (AddInGlobals.ActiveDocument.getSelectedText() != string.Empty)
        {
          commandText = "Launch Z3 Inspector for '" + AddInGlobals.ActiveDocument.CurrentFunctionName + "'";
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
          return;
        }

        status = vsCommandStatus.vsCommandStatusInvisible;
        return;
      }
    }    
  }
}
