//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------


using System.IO;
using EnvDTE;

namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class CreateBoogie : VerifyJob {

    public CreateBoogie(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(true),
             activeDocument.VCCSettings,
             Utilities.GetActivePlatformID(activeDocument.ProjectItem)) {
      this.AdditionalParameter = "/t";
      this.AfterExecuteDelegate = delegate() { AfterExecute(this.FullFileName); };
    }

    public static void AfterExecute(string FileToOpen) {
      FileToOpen = Path.ChangeExtension(FileToOpen, ".bpl");

      try {
        Window wnd = AddInGlobals.DTE.OpenFile(EnvDTE.Constants.vsViewKindTextView, FileToOpen);
        wnd.Visible = true;
      } catch { }
    }
  } 

}
