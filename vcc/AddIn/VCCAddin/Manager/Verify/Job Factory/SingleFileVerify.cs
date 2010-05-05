//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------


namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class SingleFileVerify : VerifyJob {

    public SingleFileVerify(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(false),
             activeDocument.VCCSettings,
             Utilities.GetActivePlattformID(activeDocument.ProjectItem)) {
    }
  }
}
