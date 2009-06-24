//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.IO;
using EnvDTE80;
using EnvDTE;
using VccModelViewer;
using VerifiedCCompilerAddin.Manager.Marker;
using VerifiedCCompilerAddin.Manager.Verify;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCViewErrorModelFromErrorList : VCCCommand {

    public VCCViewErrorModelFromErrorList(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdViewErrorModel2", "Create Model for Function", "Creates Z3 error model for a single function", (int)VCCMenuIcons.VCCModelViewer, null, CommandBarName.ErrorWindow)
    { 
    }

    
    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      StartModelViewer();
      return true;
    }

    private static ErrorItem getSelectedErrorItem() {
      ErrorList eList = AddInGlobals.DTE.ToolWindows.ErrorList;
      Array Items = eList.SelectedItems as System.Array;
      if (Items.Length > 0) {
        return Items.GetValue(0) as ErrorItem;
      } else {
        return null;
      }
    }

    
    private static bool isVCCModelErrorItem(ErrorItem eitem) {
      VCCErrorItem vccErrorItem = Utilities.getVCCErrorItemFromErrorItem(eitem);
      if (vccErrorItem != null)
        return true;
      else
        return false;
    }

    private static void StartModelViewer() {
      ErrorItem i = getSelectedErrorItem();

      string FileToOpen = i.FileName;

      int modelNumber = Utilities.getErrorModelnumber(i.Line, i.FileName);
      int errorLine = i.Line;

      Utilities.LaunchModelViewer(FileToOpen, errorLine, modelNumber);
    }

    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {

      if (isVCCModelErrorItem(getSelectedErrorItem()) && (!VerifyManager.isRunning)) {
        commandText = "Show Z3 error model";
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      }
      else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }
    }
  }
}
