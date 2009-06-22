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
using EnvDTE;
using System.Diagnostics;

namespace VerifiedCCompilerAddin.Forms.AddInSettings {
  public partial class AddInSettingsForm : Form {
    public AddInSettingsForm() {
      InitializeComponent();
      
      string cmdswitches = AddinSettingsManager.VCCCommandLineSwitches;
      // Options for Z3 Distributed Z3
      cb_DistZ3.Checked = extractCmdArgument("/b:/proverOpt:DIST", ref cmdswitches);
      // Options for Z3 Inspector
      cb_Inspector.Checked = extractCmdArgument("/b:/proverOpt:INSPECTOR=Z3Inspector.exe", ref cmdswitches);
      VCCCommandLineSwitches.Text = cmdswitches;
      VccCommandLineSwitchesActive.Checked = AddinSettingsManager.VCCCommandLineSwitchesEnabled;
      cbWarnForHeader.Checked = AddinSettingsManager.WarnForHeaderFile;
      cbShowBallonTip.Checked = AddinSettingsManager.ShowBallonTip;
    }

    private void button1_Click(object sender, EventArgs e) {
      string cmdswitches = VCCCommandLineSwitches.Text;
      // Options for Z3 Distributed Z3
      addCmdArgument("/b:/proverOpt:DIST", ref cmdswitches, cb_DistZ3.Checked);
      // Options for Z3 Inspector
      addCmdArgument("/b:/proverOpt:INSPECTOR=Z3Inspector.exe", ref cmdswitches, cb_Inspector.Checked);
      AddinSettingsManager.VCCCommandLineSwitches = cmdswitches;
      AddinSettingsManager.VCCCommandLineSwitchesEnabled = VccCommandLineSwitchesActive.Checked;
      AddinSettingsManager.WarnForHeaderFile = cbWarnForHeader.Checked;
      AddinSettingsManager.ShowBallonTip = cbShowBallonTip.Checked;
      AddinSettingsManager.Save();

      if (AddinSettingsManager.ShowBallonTip)
      {
        Utilities.InstallNotifyIcon();
      }
      else
      {
        Utilities.RemoveNotifyIcon();
      }
    }

    private string getVCCVersionString() {
      return System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString();
    }

    private void AddInSettingsForm_Load(object sender, EventArgs e) {
      lbVersion.Text = getVCCVersionString();
    }

    private bool extractCmdArgument(string vccarg, ref string switches)
    {
      if (containsCmdArgument(vccarg, switches))
      {
        int pos = switches.IndexOf(vccarg);
        switches = switches.Remove(pos, vccarg.Length).Trim();
        while ((switches.Length > pos) && (switches[pos] == ' '))
        {
            switches = switches.Remove(pos, 1);
        }
        return true;
      }
      else
      {
        return false;
      }
    }

    private void addCmdArgument(string vccarg, ref string switches, bool add)
    {
      if (add && (!containsCmdArgument(vccarg, switches)))
      {
        if ((switches.Length > 0) && (!switches.EndsWith(" ")))
        {
          switches += " ";
        }
        switches += vccarg;
      }
    }

    private bool containsCmdArgument(string vccarg, string switches)
    {
      return switches.Contains(vccarg);
    }

  
  }
}
