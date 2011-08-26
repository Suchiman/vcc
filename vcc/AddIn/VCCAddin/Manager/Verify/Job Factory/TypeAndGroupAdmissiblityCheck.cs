//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------


namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class TypeAndGroupAdmissiblityCheck : VerifyJob {

    public TypeAndGroupAdmissiblityCheck(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(true),
            activeDocument.VCCSettings,
            Utilities.GetActivePlatformID(activeDocument.ProjectItem),
            activeDocument.getAdmissibilityCheckString(true)) { }

    }
  }
