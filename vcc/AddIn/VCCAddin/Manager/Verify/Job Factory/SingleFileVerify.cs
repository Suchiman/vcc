//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;

namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class SingleFileVerify : VerifyJob {

    public SingleFileVerify(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(false),
             activeDocument.VCCSettings,
             Utilities.GetActivePlattformID(activeDocument.ProjectItem)) {
    }
  }
}
