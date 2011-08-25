//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Reflection;
using System.Text;
using Microsoft.VisualStudio.VCProjectEngine;
using EnvDTE;

namespace Microsoft.Research.Vcc.VSPackage {

  public class CompilerSettings {
    const string NoInherit = "$(NOINHERIT)";

    private string preprocessorDefinitions;
    private string additionalIncludeDirectories;
    private string forcedIncludeFiles;
    
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

      this.additionalIncludeDirectories = CompilerSettings.ExecuteMacroProject(vcProject, activeSetting, this.additionalIncludeDirectories);
      this.preprocessorDefinitions = CompilerSettings.ExecuteMacroProject(vcProject, activeSetting, this.preprocessorDefinitions);
      this.forcedIncludeFiles = CompilerSettings.ExecuteMacroProject(vcProject, activeSetting, this.forcedIncludeFiles);
    }

    public string ToVccOptions()
    {
        var args = new StringBuilder();

        args.Append(SplitAndFormatArgs(this.preprocessorDefinitions, "/p:/D", false));
        args.Append(SplitAndFormatArgs(this.additionalIncludeDirectories, "/p:/I", true));
        args.Append(SplitAndFormatArgs(this.forcedIncludeFiles, "/p:/I", true));

        return args.ToString();
    }

    private static void AddSettingsForCompilerTool(CompilerSettings settings, VCCLCompilerTool compilerTool)
    {
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

    private static void AddSettingsForCompilerTool(CompilerSettings settings, VCNMakeTool nMakeTool)
    {
        var additionalOptions = GetSettingsFromAdditionalOptions(nMakeTool);

        string prePro = nMakeTool.PreprocessorDefinitions ?? "";
        if (settings.InheritPreprocessorDefinitions)
            settings.preprocessorDefinitions += prePro + ";" + additionalOptions.Item1 + ";";

        string additionalDirs = nMakeTool.IncludeSearchPath ?? "";
        if (settings.InheritAdditionalIncludeDirectories)
            settings.additionalIncludeDirectories += additionalDirs + ";" + additionalOptions.Item2 + ";";

        string forcedIncludes = nMakeTool.ForcedIncludes ?? "";
        if (settings.InheritForcedIncludeFiles)
            settings.forcedIncludeFiles += forcedIncludes + ";" + additionalOptions.Item3 + ";";
    }

    private static Tuple<string,string,string> GetSettingsFromAdditionalOptions(VCNMakeTool nMakeTool)
    {
        string additionalOptionsString = GetAdditionalOptionsValue(nMakeTool);
        if (!String.IsNullOrEmpty(additionalOptionsString))
        {
            string[] additionalOptions = additionalOptionsString.Split(new[] {' '},
                                                                        StringSplitOptions.RemoveEmptyEntries);

            var defines = new StringBuilder();
            var includes = new StringBuilder();
            var forcedIncludes = new StringBuilder();

            foreach (var opt in additionalOptions)
            {
                if (opt.StartsWith("/D") || opt.StartsWith("-D"))
                {
                    defines.Append(opt.Substring(2)).Append(';');
                }

                if (opt.StartsWith("/I") || opt.StartsWith("-I"))
                {
                    includes.Append(opt.Substring(2)).Append(';');
                }

                if (opt.StartsWith("/FI") || opt.StartsWith("-FI"))
                {
                    forcedIncludes.Append(opt.Substring(3)).Append(';');
                }
            }

            return Tuple.Create(defines.ToString(), includes.ToString(), forcedIncludes.ToString());
        }

        else return Tuple.Create("", "", "");
    }

      private static string GetAdditionalOptionsValue(VCNMakeTool nMakeTool)
      {
          const BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.FlattenHierarchy |
                                            BindingFlags.NonPublic | BindingFlags.Public;
          var strProp = nMakeTool.GetType().GetProperty("StronglyTypedRule", bindingFlags);
          var str = strProp.GetValue(nMakeTool, null);
          var aoProp = str.GetType().GetProperty("AdditionalOptions", bindingFlags);
          var ao = aoProp.GetValue(str, null);
          var vasProp = ao.GetType().GetProperty("ValueAsString", bindingFlags);
          return (string)vasProp.GetValue(ao, null);
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
        if (CompilerTool != null) AddSettingsForCompilerTool(settings, CompilerTool);
      } catch { }

      try {
          VCNMakeTool nMakeTool = Tools.Item("VCNMakeTool") as VCNMakeTool;
          if (nMakeTool != null) AddSettingsForCompilerTool(settings, nMakeTool);
      } catch {  }

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
          if (CompilerTool != null) AddSettingsForCompilerTool(settings, CompilerTool);

          VCNMakeTool nMakeTool = CollectionOfTools.Item("VCNMakeTool") as VCNMakeTool;
          if (nMakeTool != null) AddSettingsForCompilerTool(settings, nMakeTool);
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
        if (CompilerTool != null) AddSettingsForCompilerTool(settings, CompilerTool);
      } catch { }
    }

    private static string ExecuteMacroProject(VCProject Project, string ActiveConfiguration, string MacroToEvaluate)
    {
      IVCCollection CollectionOfConfigurations = Project.Configurations as IVCCollection;
      //Extract Config from Collection, with Name stored in ActiveSetting
      VCConfiguration Configuration = CollectionOfConfigurations.Item(ActiveConfiguration) as VCConfiguration;
      return Configuration.Evaluate(MacroToEvaluate);
    }

      private static string SplitAndFormatArgs(string options, string optionPrefix, bool quoteFileNames)
    {
        if (String.IsNullOrEmpty(options)) return "";

        StringBuilder result = new StringBuilder();
        var opts = options.Split(new[] {';'}, StringSplitOptions.RemoveEmptyEntries);
        foreach (var opt in opts)
        {
            if (opt == NoInherit) continue;
            if (quoteFileNames && opt.Contains(" ") && !opt.StartsWith("\""))
                result.Append(optionPrefix).Append('"').Append(opt).Append('"').Append(' ');
            else
                result.Append(optionPrefix).Append(opt).Append(' ');
        }

        return result.ToString();
    }
  }
}
