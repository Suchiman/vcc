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
  public class VCCCreateBoogie : VCCCommand{

    public VCCCreateBoogie(DTE2 dte, AddIn addin) :
      base(dte, addin, "cmdVCCCreateOpenBoogie", "Open Boogie for File", "Open Boogie for a file", (int)VCCMenuIcons.CreateOpenBoogie,VCCBindings.CreateBoogie, CommandBarName.VCCMoreCommands) { 
    }
    
    public override bool Exec(EnvDTE.vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled) {
      handled = true;
      VerifyManager.Init();
      VerifyManager.AddJob(JobFactory.CreateJob(JobType.CreateBoogie));
      VerifyManager.Execute();
      return true;
    }

    
 
    public override void QueryStatus(EnvDTE.vsCommandStatusTextWanted neededText, ref EnvDTE.vsCommandStatus status, ref object commandText) {
      if (AddInGlobals.ActiveDocument.IsCodeFile && (!VerifyManager.isRunning)) {
        commandText = "Open &Boogie for '" + AddInGlobals.ActiveDocument.FileName + "'";
        status = vsCommandStatus.vsCommandStatusEnabled | vsCommandStatus.vsCommandStatusSupported;
      }
      else {
        status = vsCommandStatus.vsCommandStatusInvisible;
      }        
    }
  }
}
