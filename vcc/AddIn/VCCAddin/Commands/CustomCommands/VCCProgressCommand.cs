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
using VerifiedCCompilerAddin.Manager.Verify;

// This is in invisible command, that is used internaly for callback functionality

namespace VerifiedCCompilerAddin.Commands {
  class VCCProgressCommand : VCCCommand {

    public VCCProgressCommand(DTE2 dte, AddIn addin) :
      base(dte, addin, "showVCCProgress", "Updates Progress", "Updates Progress", (int)VCCMenuIcons.CustomSingleFile,null,null) {      
    }

    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      //Progress Update!
      if (VerifyManager.isRunning) {
        VerifyManager.UpdateProgress(varIn as String);
      }
      
      return true;
    }

    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      status = (EnvDTE.vsCommandStatus.vsCommandStatusEnabled | EnvDTE.vsCommandStatus.vsCommandStatusSupported);
      commandText = "(internal)";
    }

  }
}
