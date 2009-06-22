//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using EnvDTE;
using EnvDTE80;
using VerifiedCCompilerAddin.Forms.AddInSettings;

namespace VerifiedCCompilerAddin.Commands {
  class VCCMenueSettings : VCCCommand{

    public VCCMenueSettings(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCMenueSettings", "&Settings", "Add-in Settings", (int)VCCMenuIcons.Settings, VCCBindings.Settings, CommandBarName.VCCMain) { 
    }

    
    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      AddInSettingsForm frm = new AddInSettingsForm();
      frm.ShowDialog();
      return true;
    }

    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      commandText = Caption;
      status = (EnvDTE.vsCommandStatus.vsCommandStatusEnabled | EnvDTE.vsCommandStatus.vsCommandStatusSupported);
    }
  }
}
