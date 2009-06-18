//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using VerifiedCCompilerAddin.ProjectExtender.Options;
using VerifiedCCompilerAddin.ProjectExtender.TypeConverter;
using EnvDTE;
using VerifiedCCompilerAddin.Manager.Settings;
using Microsoft.VisualStudio.VCProjectEngine;
using System.Diagnostics;

namespace VerifiedCCompilerAddin.ProjectExtender.Options  {

  [TypeConverter(typeof(VCCOptionsConverter)),
   Description("Expand to see vcc options for current project.")]
  
  public class VCCOptions {
    [Category("VCC MasterFile")]
    [TypeConverter(typeof(VCCMasterFileConverter))]
    [DisplayName("Masterfile options")]
    [ReadOnly(true)]
    [Description("Expand to set and active masterfile")]
    public MasterFileOptions MasterFileOptions { get; set; }

    private string m_AdditionalCommandLineArguments;
    [DisplayName("Commandline Arguments")]
    [Description("This arguments will be added to vcc commandline everytime it is executed.")]
    [NotifyParentProperty(true),
    RefreshProperties(RefreshProperties.Repaint)]
    public string AdditionalCommandLineArguments { get { return m_AdditionalCommandLineArguments; } set { m_AdditionalCommandLineArguments = value; WriteSettings(); } }

#if DefineVerifyOption
    private bool m_define_Verify;
    [DisplayName("Define VERIFY")]
    [Description("This options defines the VERIFY definition.")]
    public bool Define_Verify {
      get { return m_define_Verify; }
      set { m_define_Verify = value; ChangeDefineOfVerify(); }
    }

    private VCCLCompilerTool getCurrentCompilerTool() {
       VCProject Project = m_ActiveProject.Object as VCProject;
      string ActiveSetting = Utilities.GetActiveConfigOfProject(m_ActiveProject);

      //Projects can have there settings in a PropertySheet or Tools Collection.     
      IVCCollection CollectionOfConfigurations = Project.Configurations as IVCCollection;

      //Extract Config from Collection, with Name stored in ActiveSetting
      VCConfiguration Configuration = CollectionOfConfigurations.Item(ActiveSetting) as VCConfiguration;

      //1st collect Tool Data from Project Setting!  *****************************************************    
      IVCCollection Tools = Configuration.Tools as IVCCollection;

      try {
        //Get VCCLCompilerTool
        VCCLCompilerTool CompilerTool = Tools.Item("VCCLCompilerTool") as VCCLCompilerTool;
        return CompilerTool;
      }    catch
      {}

      return null;
    }


    private bool isVerifyDefined()
    {
      VCCLCompilerTool CompilerTool = getCurrentCompilerTool();
      if (CompilerTool != null) {
        if (CompilerTool.PreprocessorDefinitions != null)
          return CompilerTool.PreprocessorDefinitions.Contains("VERIFY");
      }
      return false;
    }


    private void ChangeDefineOfVerify() {      
      VCCLCompilerTool CompilerTool = getCurrentCompilerTool();
      if (CompilerTool == null)
        return;

      //If Property empty (not set) then the value is null!
      //And we will define first VERIFY, later it can be undefined bei toggling the option.
      if (CompilerTool.PreprocessorDefinitions == null) {
        CompilerTool.PreprocessorDefinitions = "VERIFY";
        return;
      }
       
        List<string> DefinesList = new List<string>();
        foreach (string def in CompilerTool.PreprocessorDefinitions.Split(';')) {
          DefinesList.Add(def);
        }

        if (m_define_Verify) {
          if (!DefinesList.Contains("VERIFY"))
            DefinesList.Add("VERIFY");
        } else {
          if (DefinesList.Contains("VERIFY"))
            DefinesList.Remove("VERIFY");
        }

        CompilerTool.PreprocessorDefinitions = string.Join(";", DefinesList.ToArray());
    }

#endif

    private void WriteSettings() {
        m_ActiveProject.Globals["VCCCmdArgs"] = AdditionalCommandLineArguments;
        m_ActiveProject.Globals.set_VariablePersists("VCCCmdArgs", true);
    }

    private void ReadSettings() {
      if (m_ActiveProject.Globals.get_VariableExists("VCCCmdArgs")) {
        m_AdditionalCommandLineArguments = (string)m_ActiveProject.Globals["VCCCmdArgs"];
      } else {
        m_AdditionalCommandLineArguments = "";
      }
    }

    private Project m_ActiveProject;

    public VCCOptions(Project ActiveProject) {
      m_ActiveProject = ActiveProject;
      MasterFileOptions = new MasterFileOptions(ActiveProject);
#if DefineVerifyOption      
      m_define_Verify = isVerifyDefined();
#endif
      ReadSettings();
    }

  }
}
