//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using EnvDTE;
using Microsoft.VisualStudio.VCProjectEngine;
using System.Text;

namespace Microsoft.Research.Vcc.VSPackage {

  public class CompilerSettings {
    const string NoInherit = "$(NOINHERIT)";

    private string preprocessorDefinitions;
    private string additionalIncludeDirectories;
    private string forcedIncludeFiles;
    private bool ignoreStandardIncludePath;
    
    bool InheritPreprocessorDefinitions
    {
      get { return preprocessorDefinitions == null || !preprocessorDefinitions.Contains(NoInherit); }
    }
    
    bool InheritAdditionalIncludeDirectories
    {
      get { return additionalIncludeDirectories == null || !additionalIncludeDirectories.Contains(NoInherit); }
    }
    
    bool InheritForcedIncludeFiles
    {
      get { return forcedIncludeFiles == null || !forcedIncludeFiles.Contains(NoInherit); }
    }

    public CompilerSettings(ProjectItem prjItem) 
    {     
      var vcProject = prjItem.ContainingProject.Object as VCProject;
      var file = prjItem.Object as VCFile;
      var activeConf = prjItem.ContainingProject.ConfigurationManager.ActiveConfiguration;
      var activeSetting = activeConf.ConfigurationName + "|" + activeConf.PlatformName;

      CompilerSettings.AddSettingsFromVCFile(this, file, activeSetting);
      CompilerSettings.AddSettingsFromVCProject(this, vcProject, activeSetting);

      additionalIncludeDirectories = CompilerSettings.ExecuteMacroProject(vcProject, activeSetting, "");
      preprocessorDefinitions = CompilerSettings.ExecuteMacroProject(vcProject, activeSetting, "");
      forcedIncludeFiles = CompilerSettings.ExecuteMacroProject(vcProject, activeSetting, "");

      string VCIncludeDirs = CompilerSettings.GetVCIncludeDirs(prjItem);
      if (InheritAdditionalIncludeDirectories && !ignoreStandardIncludePath)
        additionalIncludeDirectories = VCIncludeDirs + additionalIncludeDirectories;
    }
    
    public string ToVccOptions()
    {
      var ppDefs = CompilerSettings.CheckPreProcessorDefs(this.preprocessorDefinitions);
      var forcedIncDirs = CompilerSettings.CheckIncludeDirs(this.forcedIncludeFiles, "/FI");
      var incDirs = CompilerSettings.CheckIncludeDirs(this.additionalIncludeDirectories, "/I");
      var args = new StringBuilder();

      if (ppDefs.Length > 0)
      {
        args.Append(ppDefs);
      }

      if (forcedIncDirs.Length > 0)
      {
        if (args.Length > 0)
        {
          args.Append(';');
        }
        args.Append(forcedIncDirs);
      }

      if (incDirs.Length > 0)
      {
        if (args.Length > 0)
        {
          args.Append(';');
        }
        args.Append(incDirs);
      }

      if (this.ignoreStandardIncludePath)
      {
        if (args.Length > 0)
        {
          args.Append(';');
        }
        args.Append("/X");
      }

      return args.Length > 0 ? " /p:" + args : String.Empty;
    }

    private static void AddSettingsForCompilerTool(CompilerSettings settings, VCCLCompilerTool compilerTool)
    {
      if (compilerTool != null) {
        settings.ignoreStandardIncludePath |= compilerTool.IgnoreStandardIncludePath;
        string prePro = compilerTool.PreprocessorDefinitions;


        if (prePro != null && settings.InheritPreprocessorDefinitions)
          settings.preprocessorDefinitions += prePro + ";";

        string additionalDirs = compilerTool.AdditionalIncludeDirectories;
        if (additionalDirs != null && settings.InheritAdditionalIncludeDirectories)
          settings.additionalIncludeDirectories += additionalDirs + ";";

        string forcedIncludes = compilerTool.ForcedIncludeFiles;
        if (forcedIncludes != null && settings.InheritForcedIncludeFiles)
          settings.forcedIncludeFiles += forcedIncludes + ";";
      }
    }

    /// <summary>
    /// Gets current Settings for an VCProject.
    /// Reads out Settings of the entry project.
    /// 1. Main Project Settings
    /// 2. Property Sheet Settings of current project.
    /// </summary>
    /// <param name="Project"></param>
    /// <param name="ActiveSetting">Active Setting Debug/Release ...</param>
    /// <param name="settings"></param>
    /// <returns>IncludeDirs and PreprocessorDefines</returns>
    private static void AddSettingsFromVCProject(CompilerSettings settings, VCProject Project, string ActiveSetting)
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
        } catch { }
      }
    }

    /// <summary>
    /// Gets current Settings for an VCFile.
    /// </summary>
    /// <returns>IncludeDirs and PreprocessorDefines</returns>
    private static void AddSettingsFromVCFile(CompilerSettings settings, VCFile File, string ActiveSetting)
    {
      IVCCollection CollectionOfFileConfigurations = File.FileConfigurations as IVCCollection;
      //Extract Config from Collection, with Name stored in ActiveSetting
      VCFileConfiguration FileConfiguration = CollectionOfFileConfigurations.Item(ActiveSetting) as VCFileConfiguration;
      
      try {
        //Get Tool  
        VCCLCompilerTool CompilerTool = FileConfiguration.Tool as VCCLCompilerTool;
        AddSettingsForCompilerTool(settings, CompilerTool);
      } catch { }
    }

    private static string ExecuteMacroProject(VCProject Project, string ActiveConfiguration, string MacroToEvaluate)
    {
      IVCCollection CollectionOfConfigurations = Project.Configurations as IVCCollection;
      //Extract Config from Collection, with Name stored in ActiveSetting
      VCConfiguration Configuration = CollectionOfConfigurations.Item(ActiveConfiguration) as VCConfiguration;
      return Configuration.Evaluate(MacroToEvaluate);
    }

    private static string GetActiveConfigOfProject(Project prj)
    {
      return prj.ConfigurationManager.ActiveConfiguration.ConfigurationName;
    }

    private  static string GetActivePlattformOfProject(Project prj)
    {
      return prj.ConfigurationManager.ActiveConfiguration.PlatformName;
    }


    private static string GetVCIncludeDirs(ProjectItem prjItem)
    {
      EnvDTE80.DTE2 dte = (EnvDTE80.DTE2)VSIntegration.DTE;
      EnvDTE.Properties properties = dte.Properties["Projects", "VCDirectories"];

      string VCIncludeDir = properties.Item("IncludeDirectories").Value.ToString();
      string[] aVCDirs = VCIncludeDir.Split('|');
      Dictionary<string, string> IncludesForPlattforms = new Dictionary<string, string>();
      for (int i = 0; i < aVCDirs.Length; i += 2) {
        IncludesForPlattforms.Add(aVCDirs[i], aVCDirs[i + 1]);
      }

      string IncDirsForActivePlattform = IncludesForPlattforms[GetActivePlattformOfProject(prjItem.ContainingProject)];
      IncDirsForActivePlattform = ExecuteMacroProject((prjItem.ContainingProject.Object as VCProject),
                                  GetActiveConfigOfProject(prjItem.ContainingProject),
                                  IncDirsForActivePlattform);

      if (IncDirsForActivePlattform.EndsWith(";"))
        return IncDirsForActivePlattform;
      else
        return IncDirsForActivePlattform + ";";
    }

    private static string CheckPreProcessorDefs(string PreProcessorDefs) {
      if (PreProcessorDefs != null) {
        PreProcessorDefs = PreProcessorDefs.Replace("$(NOINHERIT);", "");
        string[] PreProcessorDefsArray = PreProcessorDefs.Split(';');
        StringBuilder VCCDefs = new StringBuilder();

        foreach (string Defs in PreProcessorDefsArray) {
          //Emtpy Dirs dont needed     
          if (Defs == String.Empty || Defs== "VERIFY" ) continue;
          VCCDefs.Append("/D" + Defs + ";");
        }
        while (VCCDefs.ToString().EndsWith(";")) {
          VCCDefs = VCCDefs.Remove(VCCDefs.Length - 1, 1);
        }
        return VCCDefs.ToString();
      } else {
        return String.Empty;
      }
    }

    private static string CheckIncludeDirs(string DirsOrFiles, string optionString)
    {
      //Cleanup NOINHERIT FLAG
      if (DirsOrFiles != null)
      {
        DirsOrFiles = DirsOrFiles.Replace("$(NOINHERIT);", "");
        string[] DirsOrFilesArray = DirsOrFiles.Split(';');
        StringBuilder VCCIncDirs = new StringBuilder();

        foreach (string Dir in DirsOrFilesArray)
        {
          //Emtpy Dirs dont needed     
          if (Dir == String.Empty) continue;

          string help = Dir.Replace("\"", "");
          if (help.Contains(" "))
            help = "\"" + help + "\"";

          VCCIncDirs.Append(optionString).Append(help).Append(";");
        }
        while (VCCIncDirs.ToString().EndsWith(";"))
        {
          VCCIncDirs = VCCIncDirs.Remove(VCCIncDirs.Length - 1, 1);
        }
        return VCCIncDirs.ToString();
      }
      else
      {
        return String.Empty;
      }
    }
  }
}
