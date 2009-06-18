//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Diagnostics;
using EnvDTE;
using Microsoft.VisualStudio.VCProjectEngine;

namespace VerifiedCCompilerAddin.Manager.Settings {

  public class VCCSettings {
    const string NoInherit = "$(NOINHERIT)";

    private string sPreprocessorDefinitions = String.Empty;
    private string sAdditionalIncludeDirectories = String.Empty;
    private string sForcedIncludeFiles = String.Empty;
    private bool bIgnoreStandardIncludePath = false;

    public string PreprocessorDefinitions
    {
      get { return sPreprocessorDefinitions; }
      set { sPreprocessorDefinitions = value; }
    }

    public bool IgnoreStandardIncludePath {
      get { return bIgnoreStandardIncludePath; }
      set { bIgnoreStandardIncludePath = value; }
    }


    public string AdditionalIncludeDirectories
    {
      get { return sAdditionalIncludeDirectories; }
      set { sAdditionalIncludeDirectories = value; }
    }

    public string ForcedIncludeFiles
    {
      get { return this.sForcedIncludeFiles; }
      set { this.sForcedIncludeFiles = value; }
    }
    
    public bool InheritPreprocessorDefinitions
    {
      get { return !PreprocessorDefinitions.Contains(NoInherit); }
    }
    
    public bool InheritAdditionalIncludeDirectories
    {
      get { return !AdditionalIncludeDirectories.Contains(NoInherit); }
    }
    
    public bool InheritForcedIncludeFiles
    {
      get { return !ForcedIncludeFiles.Contains(NoInherit); }
    }

    public VCCSettings(ProjectItem prjItem, string ActiveSetting) {
      
      VCProject Project = prjItem.ContainingProject.Object as VCProject;
      VCFile File = prjItem.Object as VCFile;
      
      SettingsManager.AddSettingsFromVCFile(this, File, ActiveSetting);
      SettingsManager.AddSettingsFromVCProject(this, Project, ActiveSetting);

      AdditionalIncludeDirectories = SettingsManager.ExecuteMacroProject(Project, ActiveSetting, AdditionalIncludeDirectories);
      PreprocessorDefinitions = SettingsManager.ExecuteMacroProject(Project, ActiveSetting, PreprocessorDefinitions);
      ForcedIncludeFiles = SettingsManager.ExecuteMacroProject(Project, ActiveSetting, ForcedIncludeFiles);

      string VCIncludeDirs = SettingsManager.GetVCIncludeDirs(prjItem);
      if (InheritAdditionalIncludeDirectories && !IgnoreStandardIncludePath)
        AdditionalIncludeDirectories = VCIncludeDirs + AdditionalIncludeDirectories;
    }
  }

  public static class SettingsManager {
    
    internal static void AddSettingsForCompilerTool(VCCSettings settings, VCCLCompilerTool compilerTool)
    {
      if (compilerTool != null) {
        settings.IgnoreStandardIncludePath |= compilerTool.IgnoreStandardIncludePath;
        string prePro = compilerTool.PreprocessorDefinitions;


        if (prePro != null && settings.InheritPreprocessorDefinitions)
          settings.PreprocessorDefinitions += prePro + ";";

        string additionalDirs = compilerTool.AdditionalIncludeDirectories;
        if (additionalDirs != null && settings.InheritAdditionalIncludeDirectories)
          settings.AdditionalIncludeDirectories += additionalDirs + ";";

        string forcedIncludes = compilerTool.ForcedIncludeFiles;
        if (forcedIncludes != null && settings.InheritForcedIncludeFiles)
          settings.ForcedIncludeFiles += forcedIncludes.ToString() + ";";
      }
    }

    /// <summary>
    /// Gets current Settings for an VCProject.
    /// Reads out Settings of the entry project.
    /// 1. Main Project Settings
    /// 2. Property Sheet Settings of current project.
    /// </summary>
    /// <param name="oVCProject">Object Reference to a VCProject</param>
    /// <param name="ActiveSetting">Active Setting Debug/Release ...</param>
    /// <returns>IncludeDirs and PreprocessorDefines</returns>
    internal static void AddSettingsFromVCProject(VCCSettings settings, VCProject Project, string ActiveSetting)
    {
      //Projects can have there settings in a PropertySheet or Tools Collection.     
      IVCCollection CollectionOfConfigurations = Project.Configurations as IVCCollection;
       
      //Extract Config from Collection, with Name stored in ActiveSetting
      VCConfiguration Configuration = CollectionOfConfigurations.Item(ActiveSetting) as VCConfiguration;
      
      //1st collect Tool Data from Project Setting!  *****************************************************    
      IVCCollection Tools = Configuration.Tools as IVCCollection;

      try {
        //Get VCCLCompilerTool
        VCCLCompilerTool CompilerTool = Tools.Item("VCCLCompilerTool") as VCCLCompilerTool;
        AddSettingsForCompilerTool(settings, CompilerTool);
      } catch { }

      //2nd collect Tool Data from PropertySheets! *****************************************************
      //PropertySheets Collection
      IVCCollection CollectionOfPropertySheets = Configuration.PropertySheets as IVCCollection;
      //Count Sheets...
      int SheetCount = CollectionOfPropertySheets.Count;

      for (int i = 0; i < SheetCount; i++) {
        try {
          //Get Sheet from Collection
          VCPropertySheet PropertySheet = CollectionOfPropertySheets.Item(i + 1) as VCPropertySheet;
          //Get Tools                
          IVCCollection CollectionOfTools = PropertySheet.Tools as IVCCollection;
          //Get VCCLCompilerTool
          VCCLCompilerTool CompilerTool = CollectionOfTools.Item("VCCLCompilerTool") as VCCLCompilerTool;
          AddSettingsForCompilerTool(settings, CompilerTool);
        } catch (Exception) {
        }
      }
    }

    /// <summary>
    /// Gets current Settings for an VCFile.
    /// </summary>
    /// <param name="ActiveSetting">Active Setting Debug/Release ...</param>
    /// <param name="oVCFile">VCFile Object</param>
    /// <param name="ActiveSetting">Current Configuration to get settings for</param>
    /// <returns>IncludeDirs and PreprocessorDefines</returns>
    internal static void AddSettingsFromVCFile(VCCSettings settings, VCFile File, string ActiveSetting)
    {
      IVCCollection CollectionOfFileConfigurations = File.FileConfigurations as IVCCollection;
      //Extract Config from Collection, with Name stored in ActiveSetting
      VCFileConfiguration FileConfiguration = CollectionOfFileConfigurations.Item(ActiveSetting) as VCFileConfiguration;
      
      try {
        //Get Tool  
        VCCLCompilerTool CompilerTool = FileConfiguration.Tool as VCCLCompilerTool;
        AddSettingsForCompilerTool(settings, CompilerTool);
      } catch (Exception) {
      }
    }

    internal static string ExecuteMacroFile(VCFile File, string ActiveConfigutation, string MarcroToEvaluate)
    {
      IVCCollection CollectionOfFileConfigurations = File.FileConfigurations as IVCCollection;
      //Extract Config from Collection, with Name stored in ActiveSetting
      VCFileConfiguration FileConfiguration = CollectionOfFileConfigurations.Item(ActiveConfigutation) as VCFileConfiguration;

      return FileConfiguration.Evaluate(MarcroToEvaluate); 
    }

    internal static string ExecuteMacroProject(VCProject Project, string ActiveConfiguration, string MacroToEvaluate)
    {
      IVCCollection CollectionOfConfigurations = Project.Configurations as IVCCollection;
      //Extract Config from Collection, with Name stored in ActiveSetting
      VCConfiguration Configuration = CollectionOfConfigurations.Item(ActiveConfiguration) as VCConfiguration;
      return Configuration.Evaluate(MacroToEvaluate);
    }

    internal static string GetVCIncludeDirs(ProjectItem prjItem)
    {
      EnvDTE80.DTE2 dte = AddInGlobals.DTE;
      EnvDTE.Properties properties = dte.get_Properties("Projects", "VCDirectories");

      string VCIncludeDir = properties.Item("IncludeDirectories").Value.ToString();
      string[] aVCDirs = VCIncludeDir.Split('|');
      Dictionary<string, string> IncludesForPlattforms = new Dictionary<string, string>();
      for (int i = 0; i < aVCDirs.Length; i += 2) {
        IncludesForPlattforms.Add(aVCDirs[i], aVCDirs[i + 1]);
      }

      string IncDirsForActivePlattform = IncludesForPlattforms[Utilities.GetActivePlattformOfProject(prjItem.ContainingProject)];
      IncDirsForActivePlattform = ExecuteMacroProject((prjItem.ContainingProject.Object as VCProject),
                                  Utilities.GetActiveConfigOfProject(prjItem.ContainingProject),
                                  IncDirsForActivePlattform);

      if (IncDirsForActivePlattform.EndsWith(";"))
        return IncDirsForActivePlattform;
      else
        return IncDirsForActivePlattform + ";";
    }
  }
}
