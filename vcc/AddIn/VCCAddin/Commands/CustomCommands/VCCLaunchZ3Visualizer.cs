//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using EnvDTE80;
using EnvDTE;
using System.IO;
using VerifiedCCompilerAddin.Manager.Settings;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCLaunchZ3Visualizer : VCCCommand {

    public VCCLaunchZ3Visualizer(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCLaunchZ3Visualizer", "Launch Z3 Axiom Profiler", "Launch Z3 Axiom Profiler", (int)VCCMenuIcons.LaunchZ3Visualizer, VCCBindings.LaunchZ3Viewer, CommandBarName.VCCMoreCommands) {
    }
    
    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;      
      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.LaunchZ3Visualizer));      
      VerifyManager.Execute();
      return true;
    }


    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      if (!VerifyManager.isRunning) {
        // Gruppe
        if (AddInGlobals.ActiveDocument.IsInGroup) {
          commandText = "Launch Z3 Axiom Profiler for admissibility check of '" + AddInGlobals.ActiveDocument.CurrentTypeName + "::" + AddInGlobals.ActiveDocument.CurrentGroupName + "'";
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
          return;
        }

        // Typ
        if (AddInGlobals.ActiveDocument.IsInType) {
          commandText = "Launch Z3 Axiom Profiler for admissibility check of type '" + AddInGlobals.ActiveDocument.CurrentTypeName + "'";
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
          return;
        }

        // Funktion
        if (AddInGlobals.ActiveDocument.IsInFunction) {
          commandText = "Launch Z3 Axiom Profiler for '" + AddInGlobals.ActiveDocument.CurrentFunctionName + "'";
          status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
          return;
        }
      } else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }
    }    
  }
}
