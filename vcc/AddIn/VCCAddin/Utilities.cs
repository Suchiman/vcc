//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using EnvDTE;
using EnvDTE80;
using Microsoft.VisualStudio.CommandBars;
using VccModelViewer;
using VerifiedCCompilerAddin.Commands;
using VerifiedCCompilerAddin.Forms;
using VerifiedCCompilerAddin.Manager.Marker;
using VerifiedCCompilerAddin.Manager.Settings;
using VerifiedCCompilerAddin.Manager.Verify;
using VerifiedCCompilerAddin.Resources;

namespace VerifiedCCompilerAddin {

  public static class Utilities {

    internal static string GetActivePlatformID(ProjectItem prjItem) {
      return prjItem.ConfigurationManager.ActiveConfiguration.PlatformName;
    }

    internal static NotifyIcon NotifyIcon
    {
      get; set;
    }

    public static int GetLastIndexBefore(string Text, string SearchFor, int last) {
      String s = Text.Substring(0,Math.Min(last + SearchFor.Length, Text.Length));

      int index = s.LastIndexOf(SearchFor);      
      if (index < last)
      {
      return index;      
      }
      else{
      return -1;
      }
    }

    internal static string GetVCCVersionString() {
      return System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString();
    }
    
    internal static string getMasterFileName() {
      return EvaluateAbsolutePath(Path.GetDirectoryName(ActiveProject().FullName), AddinSettingsManager.MasterFileName);
    }

    internal static string getVCCPrelude() {
      string AddInAssemblyLocation = typeof(VCCLaunchZ3Visualizer).Assembly.Location;
      string AddInPath = Path.GetDirectoryName(AddInAssemblyLocation);
      string HeadersPath = AddInPath.ToUpperInvariant().Replace("ADDIN", "Headers");
      return Path.Combine(HeadersPath, "VccPrelude.bpl");
    }

    internal static bool SaveAllOpenDocuments() {
      bool saved = false;

      for (int i = 1; i <= AddInGlobals.DTE.Documents.Count; i++) {
        Document doc = AddInGlobals.DTE.Documents.Item(i);
        saved |= (!doc.Saved);

      }
      if (saved) {
        return SaveChangedFilesMessage();
      }
      return true;
    }

    delegate bool SaveChangedFilesMessageDelegate();
    
    private static bool SaveChangedFilesMessage() {

      if (AddInGlobals.VCCPane.InvokeRequired) {
        return (bool)AddInGlobals.VCCPane.Invoke(new SaveChangedFilesMessageDelegate(SaveChangedFilesMessage));
      } else {
        if (MessageBox.Show("There are unsaved files. Would you like to save them now?",
                              "Save current File(s)?",
                              MessageBoxButtons.YesNo,
                              MessageBoxIcon.None) == DialogResult.Yes) {
          AddInGlobals.DTE.Documents.SaveAll();
          return true;
        } else {
          return false;
        }
      }
    }

    private static string GetVSCOMNTOOLS() {
      string Version = AddInGlobals.DTE.Version;      //returns something like 8.0
      string CleanVersionTag = Version.Replace(".", "");
      string VSDir = Environment.GetEnvironmentVariable(String.Format("VS{0}COMNTOOLS", CleanVersionTag));

      return VSDir;
    }

    internal static string GetIDEPath() {
      string VPath = GetVSCOMNTOOLS();
      VPath = VPath.ToUpperInvariant().Replace("TOOLS", "IDE");
      return VPath;
    }

    internal static string GetCLPath(string ActivePlatform) {
      string CL = (ActivePlatform ==  "x64") ? "VC\\bin\\x86_amd64\\cl.exe" : "VC\\bin\\cl.exe";
      return GetVSCOMNTOOLS().ToUpperInvariant().Replace("COMMON7\\TOOLS\\", CL);
    }
       
    internal static string GetActiveConfigOfProject(Project prj) {
      return prj.ConfigurationManager.ActiveConfiguration.ConfigurationName;
    }
    
    internal static string GetActivePlatformOfProject(Project prj) {
      return prj.ConfigurationManager.ActiveConfiguration.PlatformName;
    }

    internal static string CheckIncludeDirs(string DirsOrFiles, string optionString) {
      //Cleanup NOINHERIT FLAG
      if (DirsOrFiles != null) {
        DirsOrFiles = DirsOrFiles.Replace("$(NOINHERIT);", "");
        string[] DirsOrFilesArray = DirsOrFiles.Split(';');
        StringBuilder VCCIncDirs = new StringBuilder();

        foreach (string Dir in DirsOrFilesArray) {
          //Emtpy Dirs dont needed     
          if (Dir == String.Empty) continue;

          string help = Dir.Replace("\"", "");
          if (help.Contains(" "))
            help = "\"" + help + "\"";

          VCCIncDirs.Append(optionString).Append(help).Append(";");
        }
        while (VCCIncDirs.ToString().EndsWith(";")) {
          VCCIncDirs = VCCIncDirs.Remove(VCCIncDirs.Length - 1, 1);
        }
        return VCCIncDirs.ToString();
      } else {
        return String.Empty;
      }
    }

    internal static string CheckPreProcessorDefs(string PreProcessorDefs) {
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

    internal static void NavigateProject(Project prj) {
      NavigateProjectItems(prj.ProjectItems);
    }

    internal static void NavigateProjectItems(ProjectItems prjItems) {
      foreach (ProjectItem pi in prjItems) {
        if (pi.ProjectItems != null) {
          NavigateProjectItems(pi.ProjectItems);
        }

        if (pi.Name.ToLower().EndsWith(".c")) {
          VCCSettings sets = new VCCSettings(pi,
                                       Utilities.GetActiveConfigOfProject(pi.ContainingProject));

          VerifyManager.AddJob(new VerifyJob(pi.Properties.Item("FullPath").Value.ToString(),
                                            sets,
                                            Utilities.GetActivePlatformID(pi)
                                            ));
        }
      }
    }

    internal static bool IsActiveDocumentInVCProj() {
      try {
        Document doc = AddInGlobals.ActiveDocument.Document;
        ProjectItem prjItem = doc.ProjectItem;
        Project prj = prjItem.ContainingProject;
        return IsVCPrj(prj);
      } catch {
        return false;
      }
    }

    internal static bool ContainsVCPrjType(Projects projects)
    {
      return projects.Cast<Project>().Any(IsVCPrj);
    }

    internal static bool IsVCPrj(Project prj) {
      if (prj == null)
        return false;

      if (prj.Kind == "{8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942}")
        return true;
      else
        return false;
    }

    internal static VCCErrorItem getVCCErrorItemFromErrorItem(ErrorItem eItem) {
      MarkerManager mm = AddInGlobals.VCCMarkerManger;
      if (mm == null)
        return null;

      foreach (VCCErrorItem item in mm.Errors) {
        if (item.Line == eItem.Line - 1)
          if (item.FileName == eItem.FileName) {
            return item;
          }
      }
      return null;
    }

    internal static bool isVCCErrorHere(int line, string filename) {
      MarkerManager mm = AddInGlobals.VCCMarkerManger;
      if (mm == null)
        return false;

      foreach (VCCErrorItem item in mm.Errors) {
        if (item.Line == line - 1)
          if (item.FileName.ToLower() == filename.ToLower()) {
            return true;
          }
      }
      return false;
    }

    internal static int getErrorModelnumber(int line, string filename) {
      MarkerManager mm = AddInGlobals.VCCMarkerManger;
      for (int i = 0; i < mm.Errors.Count; i++) {
        if (mm.Errors[i].Line == line - 1)
          if (mm.Errors[i].FileName == filename) {
            return i;
          }
      }
      return -1;
    }

    internal static void LaunchModelViewer(string SourceFile, int errorLine, int modelNumber) {
      AddInGlobals.LastestFileName = SourceFile;
      AddInGlobals.ModelViewerObj.LoadModel(AddInGlobals.LastestModelFileName, errorLine, modelNumber);
      AddInGlobals.ModelViewerWindow.Visible = true;
      AddInGlobals.ModelViewerObj.Visible = true;     
    }
   
    internal static void InstallVCCWindow(DTE2 application, AddIn addInInstance, WindowEvents winEvents) {
      object programmableObject = null;
      const string guidString = "{9FFC9D9B-1F39-4763-A2AF-66AED06C799E}";
      Windows2 windows = (Windows2)application.Windows;
      Assembly asm = Assembly.GetExecutingAssembly();

      AddInGlobals.VCCWindow = windows.CreateToolWindow2(addInInstance,
                                                      asm.CodeBase,
                                                      "VerifiedCCompilerAddin.Forms.VCCPane",
                                                      "VCC Window",
                                                      guidString,
                                                      ref programmableObject);

      AddInGlobals.VCCWindow.SetTabPicture(ImageConverter.Convert(ImageResources._9999));
      AddInGlobals.VCCPane = (VCCPane)AddInGlobals.VCCWindow.Object;
      AddInGlobals.VCCPane.ApplicationObject = application;
      AddInGlobals.VCCPane.RefreshData();

      winEvents.WindowActivated += AddInGlobals.VCCPane.OnWindowActivated;
    }

    internal static void InstallModelViewerWindow(DTE2 application, AddIn addInInstance) {
      object programmableObject = null;
      const string guidString = "{15032E00-DDC4-44d1-927B-2A08C23D3F8F}";
      Windows2 windows = (Windows2)application.Windows;
      Assembly asm = Assembly.GetExecutingAssembly();
      string CodeBase = asm.CodeBase.ToUpperInvariant().Replace("VERIFIEDCCOMPILERADDIN.DLL", "VccModelViewer.exe");

      AddInGlobals.ModelViewerWindow = windows.CreateToolWindow2(addInInstance,
                                                      CodeBase,
                                                      "VccModelViewer.ModelViewer",
                                                      "VCC Model Viewer",
                                                      guidString,
                                                      ref programmableObject);

      AddInGlobals.ModelViewerWindow.SetTabPicture(ImageConverter.Convert(ImageResources._9890));
      AddInGlobals.ModelViewerObj = (ModelViewer)AddInGlobals.ModelViewerWindow.Object;
      AddInGlobals.ModelViewerObj.LineColumnChanged += ModelViewerObj_LineColumnChanged;
    }

    static void ModelViewerObj_LineColumnChanged(object sender, LineColumnChangedEventArgs e) {
      try {
        string FileNameFromModelViewer = e.fileName;
        if (!File.Exists(e.fileName)) {
          FileNameFromModelViewer = EvaluateRelativePath(AddInGlobals.LastestWorkDir, e.fileName);
        }

        if (!File.Exists(FileNameFromModelViewer)) {
          Debug.WriteLine(String.Format("File {0} not found to open for ModelViewer ColumnChanged", FileNameFromModelViewer));
          return;
        }
        
        Window wnd = AddInGlobals.DTE.OpenFile(EnvDTE.Constants.vsViewKindTextView, FileNameFromModelViewer);
        wnd.Visible = true;
        wnd.Document.Activate();
        Document Doc = wnd.Document;
        TextDocument textDocument = Doc.Object(null) as TextDocument;
        int tabsize = Doc.TabSize;
        textDocument.Selection.MoveTo(e.lineNumber, e.columnNumber, false);
        textDocument.Selection.SelectLine();
        string Text = textDocument.Selection.Text;

        int newcolumn = 0;
        for (int idx = 0; idx < e.columnNumber; idx++) {
          if (Text[idx] == '\t') {
            newcolumn += tabsize - (newcolumn % tabsize);
          } else {
            newcolumn++;
          }
        }

        textDocument.Selection.MoveTo(e.lineNumber, newcolumn);

      } catch {
        return;
      }

    }

    internal static string EvaluateAbsolutePath(string ProjectPath, string RelativeFilePath) {
      try {
        string currDir = Environment.CurrentDirectory;
        Environment.CurrentDirectory = ProjectPath;
        string AbsolutePath = Path.GetFullPath(RelativeFilePath);
        Environment.CurrentDirectory = currDir;
        return AbsolutePath;
      }
      catch (Exception ex)
      {
        MessageBox.Show(ex.Message, "Error...");
        return String.Empty;
      }
    }

    /// <summary>
    /// Gets CommandLineSwitches for VCC, from ProjectExtender and/or global AddIn-Settings
    /// </summary>
    /// <returns>String of CommandLineSwitches</returns>
    internal static string GetVCCCommandLineSwitches() {
      StringBuilder Arguments = new StringBuilder();

      //Read Settings for CommandLine from ProjectExtender
      if (Utilities.ActiveProject().Globals.VariableExists["VCCCmdArgs"]) {
        string ArgumentsFromProject = (string)Utilities.ActiveProject().Globals["VCCCmdArgs"];
        if (ArgumentsFromProject != null) {
          if (ArgumentsFromProject != String.Empty) {
            Arguments.Append(ArgumentsFromProject);
          }
        }
      }

      //Read Settings from global AddIn-Settings
      if (AddinSettingsManager.VCCCommandLineSwitchesEnabled) {
        if (AddinSettingsManager.VCCCommandLineSwitches != null) {
          if (AddinSettingsManager.VCCCommandLineSwitches != String.Empty) {
            if (Arguments.Length > 0) {
              Arguments.AppendFormat(" {0}", AddinSettingsManager.VCCCommandLineSwitches);
            } else {
              Arguments.Append(AddinSettingsManager.VCCCommandLineSwitches);
            }
          }
        }
      }
      
      return Arguments.ToString();
    }

    internal static List<string> GetArgumentsInLine(string Line) {
      int open = 0;
      int close = 0;

      List<string> Arguments = new List<string>();
      string Argument = String.Empty;

      foreach (char c in Line) {
        if (c == '(') {
          open++;
          if (open == 1) {
            Argument = String.Empty;
            continue;
          }
        }

        if (c == ')') {
          close++;
          if (open == close) {
            Arguments.Add(Argument.Trim());
            Argument = String.Empty;
            continue;
          }
        }

        //Argument sperator
        if (c == ',') {
          if (open - 1 == close) {
            Arguments.Add(Argument.Trim());
            Argument = String.Empty;
            continue;
          }
        }

        Argument += c;
      }

      return Arguments;
    }

    internal static string EvaluateRelativePath(string ProjectPath, string absoluteFilePath) {
      try {
        string[] firstPathParts = ProjectPath.Trim(Path.DirectorySeparatorChar).Split(Path.DirectorySeparatorChar);
        string[] secondPathParts = absoluteFilePath.Trim(Path.DirectorySeparatorChar).Split(Path.DirectorySeparatorChar);

        int sameCounter = 0;
        for (int i = 0; i < Math.Min(firstPathParts.Length,
        secondPathParts.Length); i++) {
          if (
          !firstPathParts[i].ToLower().Equals(secondPathParts[i].ToLower())) {
            break;
          }
          sameCounter++;
        }

        if (sameCounter == 0) {
          return absoluteFilePath;
        }

        string newPath = String.Empty;
        for (int i = sameCounter; i < firstPathParts.Length; i++) {
          if (i > sameCounter) {
            newPath += Path.DirectorySeparatorChar;
          }
          newPath += "..";
        }
        if (newPath.Length == 0) {
          newPath = ".";
        }
        for (int i = sameCounter; i < secondPathParts.Length; i++) {
          newPath += Path.DirectorySeparatorChar;
          newPath += secondPathParts[i];
        }

        // i-sebaf: Think about this again... :)
        // Handels file is in project folder
        if (newPath.Replace(".\\", "").IndexOf("\\") < 0  && !newPath.StartsWith("..\\"))
        {
          return newPath.Replace(".\\", "");
        }        

        return newPath;
      } catch (Exception ex) {
        MessageBox.Show(ex.Message, "Error...");
        return String.Empty;
      }
    }

    internal static Project ActiveProject() {
        DTE2 root = AddInGlobals.DTE;
        Array Projects = root.ActiveSolutionProjects as Array;
        if (Projects.Length == 1) {
          Project VCP = Projects.GetValue(0) as Project;
          return VCP;
        }
        return null;
    }

    internal static CommandBar GetCommandBar(CommandBarName cmdBarName) {
      string barName = String.Empty;

      // Four our self created command bars we use the object lookup and not the name lookup.
      switch (cmdBarName) {
        case CommandBarName.MenuBar:
          barName = "MenuBar";
          break;
        case CommandBarName.View:
          barName = "View";
          break;
        case CommandBarName.CodeWindow:
          barName = "Code Window";
          break;
        case CommandBarName.Project:
          barName = "Project";
          break;
        case CommandBarName.Solution:
          barName = "Solution";
          break;
        case CommandBarName.Item:
          barName = "Item";
          break;
        case CommandBarName.ErrorWindow:
          barName = "Error List";
          break;
        case CommandBarName.VCCMain:
          return AddInGlobals.VCCMainMenu.CommandBar;
        case CommandBarName.VCCRandomSeed:
          return AddInGlobals.VCCRandomSeedMenu.CommandBar;
        case CommandBarName.VCCMoreCommands:
          return AddInGlobals.VCCToolsMenu.CommandBar;
        default:
          break;
      }

      CommandBar cmdBar = ((CommandBars)AddInGlobals.DTE.CommandBars)[barName];

      return cmdBar;
    }

    internal static void InstallNotifyIcon()
    {
      if (NotifyIcon == null)
          NotifyIcon = new NotifyIcon();

      NotifyIcon.Visible = true;
      NotifyIcon.Icon = Resources.ImageResources.VccIcon;
      NotifyIcon.Text = "Vcc " + System.Diagnostics.FileVersionInfo.GetVersionInfo(typeof(Utilities).Assembly.Location).FileVersion;
    }

    internal static void ReportSuccess(string message)
    {
      if (NotifyIcon == null)
        InstallNotifyIcon();
      NotifyIcon.ShowBalloonTip(4000, "Verification succeeded!", String.Format("{0} verified successfully.", message), ToolTipIcon.Info);
    }


    internal static void ReportFailure(string message)
    {
      if (NotifyIcon == null)
        InstallNotifyIcon();

      NotifyIcon.ShowBalloonTip(4000, "Verification failed!", String.Format("{0} did not verify!", message) , ToolTipIcon.Error);
    }



    internal static void RemoveNotifyIcon()
    {
      if (NotifyIcon != null)
        NotifyIcon.Visible = false;
    }
  }


  public enum CommandBarName {
    MenuBar,
    View,
    CodeWindow,
    Project,
    Solution,
    Item,
    ErrorWindow,
    VCCMain,
    VCCRandomSeed,
    VCCMoreCommands
  }

  /// <summary>
  /// Helper class to allow access to the protected static AxHost.GetIPictureDispFromPicture method
  /// </summary>
  internal class ImageConverter : AxHost {

    private ImageConverter () : base(null) { }

    internal static object Convert(System.Drawing.Image image) {
      return AxHost.GetIPictureDispFromPicture(image);
    }
  }

}
