//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.VisualStudio.CommandBars;
using EnvDTE80;
using EnvDTE;
using System.Diagnostics;

namespace VerifiedCCompilerAddin.Commands {
  public class VCCMenueViewCommand : VCCCommand{

    public VCCMenueViewCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCViewVCCPane", "VCC &Window", "VCC Window", (int)VCCMenuIcons.VCCPane, VCCBindings.VCCPane, CommandBarName.VCCMain) { 
    }
    
    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      AddInGlobals.VCCWindow.Visible = !AddInGlobals.VCCWindow.Visible;
      AddInGlobals.VCCPane.Visible = true;
      return true;
    }

    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      commandText = Caption;
      if (AddInGlobals.ActiveDocument.IsCodeFile) {
        status = (EnvDTE.vsCommandStatus.vsCommandStatusEnabled | EnvDTE.vsCommandStatus.vsCommandStatusSupported);
      }
      else {
        status = EnvDTE.vsCommandStatus.vsCommandStatusInvisible;
      }
    }
  }
}
