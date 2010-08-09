//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Manager.Verify;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCViewErrorModel : VCCCommand{

    public VCCViewErrorModel(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdViewErrorModel", "Create Model for Function", "Creates Z3 error model for a single function", (int)VCCMenuIcons.VCCModelViewer, VCCBindings.ShowFunctionModel, CommandBarName.CodeWindow)
    { 
    }
    
    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;

      string FileToOpen = AddInGlobals.ActiveDocument.FullFileName;
      string ActiveFile = FileToOpen;
      int modelNumber = Utilities.getErrorModelnumber(AddInGlobals.ActiveDocument.CurrentLine, ActiveFile);
      Utilities.LaunchModelViewer(FileToOpen, AddInGlobals.ActiveDocument.CurrentLine, modelNumber);
 
      return true;
    }


    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      if ((!VerifyManager.isRunning) && Utilities.isVCCErrorHere(AddInGlobals.ActiveDocument.CurrentLine, AddInGlobals.ActiveDocument.FullFileName)) {
        commandText = "Show Z3 error model";
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      }
      else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }
    }
  }
}
