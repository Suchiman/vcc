//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using EnvDTE;
using System.IO;

namespace VerifiedCCompilerAddin.ProjectExtender.Options {
  
  public class MasterFileOptions {
    private string m_Filemame = String.Empty;
    private bool m_Active = false;
    private Project m_ActiveProject;

    [Editor(typeof(FilteredFileNameEditor), typeof(System.Drawing.Design.UITypeEditor))]
    [Description("Uses this file as masterfile for verification")]
    [NotifyParentProperty(true),
     RefreshProperties(RefreshProperties.Repaint)] 
    public string Filename { 
      get { return m_Filemame; } 
      set {
        string AbsoluteFileName = value;
        if (CheckFileName(value) && m_Filemame != value) {
          value = Utilities.EvaluateRelativePath(Path.GetDirectoryName(m_ActiveProject.FullName), AbsoluteFileName);
          m_Filemame = value;
          WriteSettings();
        }
        }       
    }

    // Cheks if fileName exist.
    private bool CheckFileName(string value) {
      if (File.Exists(value))
        return true;

      // If Input is an relative path, convert it to absolute path an check it.
      if (File.Exists(Utilities.EvaluateAbsolutePath(Path.GetDirectoryName(Utilities.ActiveProject().FullName), value)))
        return true;

      System.Windows.Forms.MessageBox.Show("File does not exists!");
      return false;
    }

    [Description("Enables VCC to use the given filename as masterfile")]
    public bool Active { get { return m_Active; } set { m_Active = value; WriteSettings(); } }


    // Writes settings to project globals
    private void WriteSettings()
    {
      m_ActiveProject.Globals["MasterFileName"] = Filename;
      m_ActiveProject.Globals["ActiveMasterFile"] = Active.ToString();
      m_ActiveProject.Globals.set_VariablePersists("MasterFileName", true);
      m_ActiveProject.Globals.set_VariablePersists("ActiveMasterFile", true);
    }

    // Reads settings from project globlas
    private void ReadSettings() {
      if (m_ActiveProject.Globals.get_VariableExists("MasterFileName")) {
        m_Filemame = (string)m_ActiveProject.Globals["MasterFileName"];
      }

      if (m_ActiveProject.Globals.get_VariableExists("ActiveMasterFile")) {
        m_Active = Convert.ToBoolean((string)m_ActiveProject.Globals["ActiveMasterFile"]);
      }
    }


    public MasterFileOptions(Project ActiveProject) {
      m_ActiveProject = ActiveProject;
      ReadSettings();
    }
   
  }
}
