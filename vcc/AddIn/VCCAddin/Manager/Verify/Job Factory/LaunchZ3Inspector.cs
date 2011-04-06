//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------


namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class LaunchZ3Inspector : VerifyJob {

    public LaunchZ3Inspector(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(true),
             activeDocument.VCCSettings,
             Utilities.GetActivePlattformID(activeDocument.ProjectItem),
             activeDocument.CurrentFunctionName,
             "/b:/proverOpt:INSPECTOR=Z3Inspector.exe")
    { }
  }

}

