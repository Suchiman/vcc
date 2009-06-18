//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class LaunchZ3Visualizer : VerifyJob {

    public LaunchZ3Visualizer(ActiveDocument activeDocument) :
      base(activeDocument.getFileName(true),
         activeDocument.VCCSettings,
         Utilities.GetActivePlattformID(activeDocument.ProjectItem)) {

      string functionName = activeDocument.CurrentFunctionName;
      this.AdditionalParameter = "/t";

      //Is a type ?
      if (activeDocument.IsInType) {
        functionName = activeDocument.CurrentTypeName;

        if (activeDocument.IsInGroup) { // Have we a group ??
          functionName += "::" + activeDocument.CurrentGroupName;
        }
      }

      this.FunctionToVerify = functionName;
        
      System.Windows.Forms.Control ctrl = new System.Windows.Forms.Control(AddInGlobals.VCCPane, "");
     
      this.AfterExecuteDelegate = delegate() {
        AfterExecute(this.FullFileName, this.FunctionToVerify, ctrl); 
      };
    }


    public static void AfterExecute(string FileName, string FunctionName, System.Windows.Forms.Control ctrl) {
      string BPLFileName = Path.ChangeExtension(FileName, ".bpl");
      string VCCPreludeFileName = Utilities.getVCCPrelude();

      try {
        Z3AxiomProfiler.Z3AxiomProfiler Z3Visualizer = new Z3AxiomProfiler.Z3AxiomProfiler(true, ctrl);
        Z3Visualizer.load(BPLFileName, FunctionName, VCCPreludeFileName, 
                          AddinSettingsManager.RandomSeedEnabled ? AddinSettingsManager.RandomSeed : 0, 
                          AddinSettingsManager.RandomSeedEnabled, 
                          AddinSettingsManager.VCCCommandLineSwitchesEnabled ? AddinSettingsManager.VCCCommandLineSwitches : "");
        Z3Visualizer.ShowDialog();
        Z3Visualizer.Close();
        Z3Visualizer.Dispose();
        
      } catch (Exception ex) {
        AddInGlobals.BuildPane.OutputString("Visualizer Exception - Can't launch Z3 Axiom Profiler." + Environment.NewLine);
        AddInGlobals.BuildPane.OutputString("Error Dump:" + Environment.NewLine);
        AddInGlobals.BuildPane.OutputString(ex.Message + Environment.NewLine);
        AddInGlobals.BuildPane.OutputString(ex.StackTrace);
        AddInGlobals.BuildPane.Activate();
        System.Windows.Forms.MessageBox.Show(ex.Message + Environment.NewLine + "Can't execute Z3 Axiom Profiler, for detail see Build Pane", "Z3 Axiom Profiler Exception");

      }
    }
  }

}
