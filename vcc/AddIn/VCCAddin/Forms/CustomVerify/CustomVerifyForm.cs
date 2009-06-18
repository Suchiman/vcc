//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin.Forms {
  public partial class CustomVerifyForm : Form {
    public CustomVerifyForm() {
      InitializeComponent();
    }

    private void button2_Click(object sender, EventArgs e) {
      

    }

    private void CustomVerifyForm_Shown(object sender, EventArgs e) {
      cbFunction.Enabled = AddInGlobals.ActiveDocument.IsInFunction;
      cbFunction.Text = String.Format("Use function '{0}' ", AddInGlobals.ActiveDocument.CurrentFunctionName);

      if (AddinSettingsManager.VCCCommandLineSwitchesEnabled) {
        Height = 142;
        label3.Text = AddinSettingsManager.VCCCommandLineSwitches;
      }
      else {
        Height = 110;
      }

    }

    private void button1_Click(object sender, EventArgs e) {

    }
  }
}
