//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using VerifiedCCompilerAddin.Forms;

namespace VerifiedCCompilerAddin.Manager.Verify {
  internal sealed class CustomVerify : VerifyJob {

    static string AdditionalInput = String.Empty;
    public CustomVerify(ActiveDocument activeDocument)
      : base(activeDocument.getFileName(true),
             activeDocument.VCCSettings,
             Utilities.GetActivePlattformID(activeDocument.ProjectItem)) {

      string FktName = activeDocument.CurrentFunctionName;
      bool isInFkt = activeDocument.IsInFunction;
      CustomVerifyForm frm = new CustomVerifyForm();
      frm.cbFunction.Enabled = isInFkt;
      frm.cbFunction.Checked = isInFkt;
      frm.txtAddidtional.Text = AdditionalInput;

      if (frm.ShowDialog() == System.Windows.Forms.DialogResult.OK) {

        AdditionalInput = frm.txtAddidtional.Text;
        string Fkt = String.Empty;
        if (frm.cbFunction.Checked && isInFkt) {
          Fkt = FktName;
        }

        this.AdditionalParameter = AdditionalInput;
        this.FunctionToVerify = Fkt;
      } else {
        this.InvalidJob = true;
      }

    }
  }
}
