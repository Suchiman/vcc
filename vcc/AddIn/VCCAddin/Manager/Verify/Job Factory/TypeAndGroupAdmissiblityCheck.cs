//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class TypeAndGroupAdmissiblityCheck : VerifyJob {

    public TypeAndGroupAdmissiblityCheck(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(true),
            activeDocument.VCCSettings,
            Utilities.GetActivePlattformID(activeDocument.ProjectItem),
            activeDocument.getAdmissibilityCheckString(true)) { }

    }
  }
