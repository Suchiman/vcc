using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using VerifiedCCompilerAddin.Manager.Settings;
using System.Reflection;

namespace VerifiedCCompilerAddin.Forms.VCCSettingsControl {
  public partial class VCCSettingsControl : UserControl {

    string version = string.Empty;

    public String Version {
      get { return version; }
      set { 
            version = value;
            lbVersion.Text = value;
          }
    }

    int vccVersion;
    public int VCCVersion {
      get { return vccVersion; }
      set {
        vccVersion = value;
        UpdateVCCVersion();
      }
    }

    bool bAdditionalCommandLineOptions = false;
    public bool UseAdditionalCmdOptions { 
      get { return bAdditionalCommandLineOptions; }
      set {
        bAdditionalCommandLineOptions = value;
        VccCommandLineSwitches.Enabled = bAdditionalCommandLineOptions;
        VccCommandLineSwitchesActive.Checked = bAdditionalCommandLineOptions;
      }
    }

    string sAdditionalCommandLineOptions = string.Empty;
    public string AdditionalCommandLineOptions {
      get { return sAdditionalCommandLineOptions; }
      set {
        sAdditionalCommandLineOptions = value;
        VccCommandLineSwitches.Text = sAdditionalCommandLineOptions;
      } 
    }


    bool bUseHints = false;
    public bool UseHints {
      get { return bUseHints; } 
      set {
        bUseHints = value;
        cbHintsActive.Checked = bUseHints;
        gbAssumtions.Enabled = bUseHints;
      } 
    }


    bool bHint001 = false;
    public bool Hint001 {
      get { return bHint001; }
      set { 
        bHint001 = value;
        cbHint001.Checked = bHint001;
      }
    }

    bool bHint002 = false;
    public bool Hint002 {
      get { return bHint002; }
      set {
        bHint002 = value;
        cbHint002.Checked = bHint002;
      }
    }

    bool bHint003 = false;
    public bool Hint003 {
      get { return bHint003; }
      set {
        bHint003 = value;
        cbHint003.Checked = bHint003;
      }
    }


    private void UpdateVCCVersion() {
      switch (vccVersion) {
        case 1:
          rbVersion1.Checked = true;
          break;
        case 2:
          rbVersion2.Checked = true;
          break;
        default:
          throw new ArgumentOutOfRangeException("VCCVersion");
      }
    }

    
    public VCCSettingsControl() {
      InitializeComponent();
    }

    public void ReadOptions()
    {
      VCCVersion = AddinSettingsManager.UseVCC2 ? 2 : 1;
      UseAdditionalCmdOptions = AddinSettingsManager.VCCCommandLineSwitchesEnabled;
      AdditionalCommandLineOptions = AddinSettingsManager.VCCCommandLineSwitches;
      
      UseHints = AddinSettingsManager.HintsEnabled;
      Hint001 = AddinSettingsManager.Hint001;
      Hint002 = AddinSettingsManager.Hint002;
      Hint003 = AddinSettingsManager.Hint003;
      this.Version = getVersionString();
    }

    private string getVersionString() {
      return Assembly.GetExecutingAssembly().GetName().Version.ToString();
    }

    public void SaveOptions() {
      AddinSettingsManager.VCCCommandLineSwitches = AdditionalCommandLineOptions;
      AddinSettingsManager.VCCCommandLineSwitchesEnabled = UseAdditionalCmdOptions;
      AddinSettingsManager.HintsEnabled = UseHints;
      AddinSettingsManager.Hint001 = Hint001;
      AddinSettingsManager.Hint002 = Hint002;
      AddinSettingsManager.Hint003 = Hint003;
      if (VCCVersion == 1) {
        AddinSettingsManager.UseVCC2 = false;
      } else {
        AddinSettingsManager.UseVCC2 = true;
      }
      AddinSettingsManager.Save();
    }

    private void rbVersion2_CheckedChanged(object sender, EventArgs e) {
      if (rbVersion2.Checked)
      {
      VCCVersion = 2;
      }
    }

    private void rbVersion1_CheckedChanged(object sender, EventArgs e) {
      if (rbVersion1.Checked) {
        VCCVersion = 1;
      }
    }

    private void VccCommandLineSwitchesActive_CheckedChanged(object sender, EventArgs e) {
      UseAdditionalCmdOptions = VccCommandLineSwitchesActive.Checked;
    }

    private void VccCommandLineSwitches_TextChanged(object sender, EventArgs e) {
      AdditionalCommandLineOptions = VccCommandLineSwitches.Text;
    }

    private void cbHintsActive_CheckedChanged(object sender, EventArgs e) {
      UseHints = cbHintsActive.Checked;
    }

    private void cbHint001_CheckedChanged(object sender, EventArgs e) {
      Hint001 = cbHint001.Checked;
    }

    private void cbHint002_CheckedChanged(object sender, EventArgs e) {
      Hint002 = cbHint002.Checked;
    }

    private void cbHint003_CheckedChanged(object sender, EventArgs e) {
      Hint003 = cbHint003.Checked;
    }

  }
}
