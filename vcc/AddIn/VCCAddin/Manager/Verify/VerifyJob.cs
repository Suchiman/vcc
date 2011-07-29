//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.IO;
using System.Text;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin.Manager.Verify {
  /// <summary>
  /// Represents a single verification job
  /// </summary>
  public class VerifyJob {
    string _functionToVerify = string.Empty;
    string _additionalParameter = string.Empty;

    VerifyManager.VoidAction _AfterExecuteDelegate = delegate { };
    
    #region Properties

    /// <summary>
    /// True when a job is invalid, set via specific jobs like CustomVerify
    /// </summary>
    public bool InvalidJob { get; set; }

    /// <summary>
    /// Delegate is executed after vcc run is finished
    /// </summary>
    public VerifyManager.VoidAction AfterExecuteDelegate {
      get { return _AfterExecuteDelegate; }
      set { _AfterExecuteDelegate = value; }
    }

    /// <summary>
    /// Full filename of document like c:\\sample\sample.c
    /// </summary>
    public string FullFileName { get; set; }

    /// <summary>
    /// Contains the current platform, like x64, win32 ...
    /// </summary>
    public string Platform { get; set; }

    /// <summary>
    /// Current VCCSettings
    /// </summary>
    public VCCSettings Settings { get; set; }

    /// <summary>
    /// Name of the function to verify (uses /f:{0})
    /// </summary>
    public string FunctionToVerify {
      get { return _functionToVerify; }
      set { _functionToVerify = value; }
    }

    /// <summary>
    /// Additional commandline parameters for vcc
    /// </summary>
    public string AdditionalParameter {
      get { return _additionalParameter; }
      set { _additionalParameter = value; }
    }
    #endregion

    #region Events
    /************* JOB DONE ***************************/
    public delegate void JobDoneEventHandler(object sender, JobDoneEventArgs e);
    public event JobDoneEventHandler OnJobDoneHandler;

    protected void OnJobDone(JobDoneEventArgs e) {
      if (OnJobDoneHandler != null)
        OnJobDoneHandler(this, e);
    }
    private void NotifyJobDone(int Result) {
      JobDoneEventArgs e = new JobDoneEventArgs(Result);
      OnJobDone(e);
    }
    #endregion

    public VerifyJob() {
      this.FullFileName = String.Empty;
      this.Settings = null;
      this.FunctionToVerify = String.Empty;
      this.AdditionalParameter = String.Empty;
      this.Platform = String.Empty;
      this.AfterExecuteDelegate = delegate() { };
    }

    public VerifyJob(string FullFileName, VCCSettings Settings, string Platform)
    {
      this.FullFileName = FullFileName;
      this.Settings = Settings;
      this.Platform = Platform;
    }

    public VerifyJob(string FullFileName, VCCSettings Settings, string Platform, string FunctionToVerify)
      : this(FullFileName, Settings, Platform) {
      this.FunctionToVerify = FunctionToVerify;
    }

    public VerifyJob(string FullFileName, VCCSettings Settings, string Platform, string FunctionToVerify, string AdditionalParameter)
      : this(FullFileName, Settings, Platform, FunctionToVerify) {
      this.AdditionalParameter += AdditionalParameter;
    }

    public int Execute() {

      //Cancel execute if job marked as invalid
      if (InvalidJob)
        return 0;
            
      //Build Command-Line       
      StringBuilder cmdline = new StringBuilder();
      string _ForcedIncludeFiles = Settings.ForcedIncludeFiles;
      string _additionalParameterHere = AdditionalParameter;

     

      if (Settings.IgnoreStandardIncludePath) {

      }

      string PreProcessorDefs = Utilities.CheckPreProcessorDefs(Settings.PreprocessorDefinitions);
      string AdditionalIncludes = Utilities.CheckIncludeDirs(Settings.AdditionalIncludeDirectories, "/I");
      string ForcedIncludes = Utilities.CheckIncludeDirs(_ForcedIncludeFiles, "/FI");

      if (PreProcessorDefs.Length > 0) {
        AdditionalIncludes = ";" + AdditionalIncludes;
      }

      if (AdditionalIncludes.Length > 1) {
        ForcedIncludes = ";" + ForcedIncludes;
      }

      if (Settings.IgnoreStandardIncludePath) {
        if (ForcedIncludes.Length > 1)
          ForcedIncludes += ";";
      }

      cmdline.AppendFormat("/p:{0}{1}{2}{3} /stvs /clerrors+ {4} /clpath:\"{5}\"",
                              PreProcessorDefs,
                              AdditionalIncludes,
                              ForcedIncludes,
                              (Settings.IgnoreStandardIncludePath ? "/X" : String.Empty),
                              Path.GetFileName(FullFileName),
                              Utilities.GetCLPath(Platform));

      if (FunctionToVerify != String.Empty) {
        cmdline.AppendFormat(" /f:{0}", FunctionToVerify);
      }

      //Only Boogie is wanted...
      //See also additionalParameterHere, we must remove /a also when we use /t
      if (AdditionalParameter == "/t") {
        cmdline.Replace("/v", "/t");
      }

      //Always Create Modelfile 
      string _fullModelFileName = Path.ChangeExtension(FullFileName, ".vccmodel");
      AddInGlobals.LastestModelFileName = _fullModelFileName;
      cmdline.AppendFormat(" /b:/printModel:1 /b:/printModelToFile:\"{0}\"", Path.GetFileName(_fullModelFileName));


      //Have we additional Parameters from global settings or projectExtender?
      string CmdArgsFromProjectAndGlobalSetting = Utilities.GetVCCCommandLineSwitches();
      if (CmdArgsFromProjectAndGlobalSetting.Length > 0) {
        if (_additionalParameterHere.Length > 0) {
          _additionalParameterHere += " " + CmdArgsFromProjectAndGlobalSetting;
        } else {
          _additionalParameterHere = CmdArgsFromProjectAndGlobalSetting;
        }
      }      

      //Random Seed active?
      if (AddinSettingsManager.RandomSeedEnabled) {
        if (AddinSettingsManager.RandomSeed < 5) {
          _additionalParameterHere += " " + String.Format("/z:/rs:{0}", AddinSettingsManager.RandomSeed);
        }
      }

      if (_additionalParameterHere != String.Empty) {
        // Remove /a if /t is specified and no function is given to apply pruning to.
        if ((_additionalParameterHere.Contains("/a")) && 
            (AdditionalParameter.Contains("/t")) && 
            (FunctionToVerify == String.Empty))
        {
          string[] SplittedArgs = _additionalParameterHere.Split(' ');
          for (int i = 0; i < SplittedArgs.Length; i++)
          {
            if (SplittedArgs[i].Trim().ToLower() == "/a")
            {
              SplittedArgs[i] = string.Empty;
            }
          }
          _additionalParameterHere = String.Join(" ", SplittedArgs);
        }
        cmdline.AppendFormat(" {0}", _additionalParameterHere);
      }

      //Working Directory
      string workdir = Path.GetDirectoryName(FullFileName);

      if (AddinSettingsManager.WarnForHeaderFile) {
        if (!CheckForVCCHeader(workdir, Settings)) {
          VerifyManager.Cancel();
          return 0;
        }
      }

      int exitcode = VCCLauncher.LaunchVCC(cmdline.ToString(), workdir, true, this);
      //Fire Event!
      NotifyJobDone(exitcode);
      return exitcode;
    }

    private static bool CheckForVCCHeader(string workDir, VCCSettings settings) {
      const string vccHeaderFileName = "vcc.h";
      bool vccfound = false;

      FileInfo fi = !workDir.EndsWith("\\") ? new FileInfo(workDir.Replace("\"", "").ToLower() + "\\" + vccHeaderFileName) : new FileInfo(workDir.Replace("\"", "").ToLower() + vccHeaderFileName);
      vccfound |= fi.Exists;

      foreach (string DirName in settings.AdditionalIncludeDirectories.Split(';'))
      {
        fi = !DirName.EndsWith("\\") ? new FileInfo(DirName.Replace("\"", "").ToLower() + "\\" + vccHeaderFileName) : new FileInfo(DirName.Replace("\"", "").ToLower() + vccHeaderFileName);
        vccfound |= fi.Exists;
      }

      foreach (string DirName in settings.ForcedIncludeFiles.Split(';'))
      {
        fi = !DirName.EndsWith("\\") ? new FileInfo(DirName.Replace("\"", "").ToLower() + "\\" + vccHeaderFileName) : new FileInfo(DirName.Replace("\"", "").ToLower() + vccHeaderFileName);
        vccfound |= fi.Exists;
      }

      if (!vccfound) {
        return AbortVerificationQuestion(settings, vccHeaderFileName);
      }
      return true;
    }
    delegate bool AbortVerifivationQuestionDelegate(VCCSettings settings, string vccHeaderFileName);

    private static bool AbortVerificationQuestion(VCCSettings settings, string vccHeaderFileName) {

      if (AddInGlobals.VCCPane.InvokeRequired) {
        return (bool)AddInGlobals.VCCPane.Invoke(new AbortVerifivationQuestionDelegate(AbortVerificationQuestion), settings, vccHeaderFileName);
      } else {
        if (System.Windows.Forms.MessageBox.Show(vccHeaderFileName + " was not found in the following directories:" + Environment.NewLine +
                                             settings.AdditionalIncludeDirectories + Environment.NewLine +
                                             settings.ForcedIncludeFiles + Environment.NewLine +
                                             "Do you want to abort the verification?", vccHeaderFileName + " not found", System.Windows.Forms.MessageBoxButtons.YesNo, System.Windows.Forms.MessageBoxIcon.Warning) == System.Windows.Forms.DialogResult.No)
          return true;
        else
          return false;
      }
    }

    public override string ToString() {
      return FullFileName;
    }
  }
}
