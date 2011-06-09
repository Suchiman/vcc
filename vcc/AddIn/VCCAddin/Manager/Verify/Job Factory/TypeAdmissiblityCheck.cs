//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------


namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class TypeAdmissiblityCheck : VerifyJob {

    public TypeAdmissiblityCheck(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(true),
            activeDocument.VCCSettings,
            Utilities.GetActivePlattformID(activeDocument.ProjectItem),
            activeDocument.getAdmissibilityCheckString(false)) {
   
    }
  }
}
