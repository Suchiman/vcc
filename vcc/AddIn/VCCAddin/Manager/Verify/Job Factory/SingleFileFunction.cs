//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------


namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class SingleFileFuntion : VerifyJob {

    public SingleFileFuntion(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(true),
              activeDocument.VCCSettings,
              Utilities.GetActivePlatformID(activeDocument.ProjectItem),
              activeDocument.CurrentFunctionName) { }
  }

}

