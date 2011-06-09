using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using EnvDTE;
using VerifiedCCompilerAddin.Manager.Settings;
using System.Reflection;

namespace VerifiedCCompilerAddin.Forms.ToolsPage {
  public partial class VCCToolsPage : UserControl, IDTToolsOptionsPage {
    public VCCToolsPage() {
      InitializeComponent();
    }

    #region IDTToolsOptionsPage Members

    public void GetProperties(ref object PropertiesObject) {
      //don't used!
    }

    public void OnAfterCreated(DTE DTEObject) {
      vccSettingsControl1.ReadOptions();
    }  

    public void OnCancel() {
      //Discard changed options...
    }

    public void OnHelp() {
      //Do Nothing
    }

    public void OnOK() {
      vccSettingsControl1.SaveOptions();
    }

    #endregion
    
  }
}
