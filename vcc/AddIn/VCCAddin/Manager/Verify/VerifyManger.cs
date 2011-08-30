//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using EnvDTE;
using VerifiedCCompilerAddin.Manager.Settings;

namespace VerifiedCCompilerAddin.Manager.Verify {

   public static class VerifyManager {
    private static volatile List<VerifyJob> JobPipe;
    private static bool bCancel;
    private static bool bRunning;
    private static int activeJobNr;

    #region Events
    /************* PROGRESS UPDATE ********************/
    public delegate void ProgressUpdateEventHandler(ProgressUpdateEventArgs e);
    public static event ProgressUpdateEventHandler OnProgressUpdateHandler;

    private static void OnProgressUpdate(ProgressUpdateEventArgs e) {
      if (OnProgressUpdateHandler != null)
        OnProgressUpdateHandler(e);
    }
    private static void NotifyProgressUpdate(int Progress, string FunctionName) {
      ProgressUpdateEventArgs e = new ProgressUpdateEventArgs(Progress, FunctionName);
      OnProgressUpdate(e);
    }

    /************* PROGRESS UPDATE ********************/
    public delegate void FunctionValidatedEventHandler(FunctionValidatedEventArgs e);
    public static event FunctionValidatedEventHandler OnFunctionValidatedHandler;

    private static void OnFunctionValidated(FunctionValidatedEventArgs e) {
      if (OnFunctionValidatedHandler != null)
        OnFunctionValidatedHandler(e);
    }
    private static void NotifyFunctionValidated(bool State, string FunctionName) {
      FunctionValidatedEventArgs e = new FunctionValidatedEventArgs(State, FunctionName);
      OnFunctionValidated(e);
    }
    
    /************* JOBS DONE ****************************/
    public delegate void JobsDoneEventHandler(JobsDoneEventArgs e);
    public static event JobsDoneEventHandler OnJobsDoneHandler;

    private static void OnJobsDone(JobsDoneEventArgs e) {
      if (OnJobsDoneHandler != null)
        OnJobsDoneHandler(e);
    }
    private static void NotifyJobsDone(int Result) {
      JobsDoneEventArgs e = new JobsDoneEventArgs(Result);
      OnJobsDone(e);
    }

    /************* OnExecute ****************************/
    public delegate void ExecuteEventHandler(JobsExecuteEventArgs e);
    public static event ExecuteEventHandler OnExecuteHandler;

    private static void OnExecute(JobsExecuteEventArgs e) {
      if (OnExecuteHandler != null)
        OnExecuteHandler(e);
    }
    private static void NotifyExecute(List<VerifyJob> JobPipe) {
      JobsExecuteEventArgs e = new JobsExecuteEventArgs(JobPipe);
      OnExecute(e);
    }

    #endregion

    public static bool isRunning { 
      get { return bRunning; } 
    }

    public static void Init() {
      if (JobPipe == null)
        JobPipe = new List<VerifyJob>();
      
      JobPipe.Clear();
      bCancel = false;
    }

    public static void Cancel() {
      //Don't continue.
      bCancel = true;
      //Cancel Running Job
      VCCLauncher.Cancel();
      bRunning = false;
    }

    public static void UpdateProgress(string Data) {

      //Progress Update, percet of function
      if (Data.Contains("%")) {
        int epos = Data.IndexOf('%') - 1;
        int spos = Data.IndexOf("progress") + 9;
        string value = Data.Substring(spos, epos - spos);
        int progress = Convert.ToInt32(Convert.ToDouble(value));

        spos = Data.IndexOf("Verification of") + 16;
        epos = Data.IndexOf(' ', spos);
        string functionName = Data.Substring(spos, epos - spos);

        NotifyProgressUpdate(progress, functionName);
      }

      if (Data.Contains("succeeded.")) {
        int epos = Data.IndexOf(" succeeded.");
        int spos = Data.IndexOf("Verification of ") + 16;
        string FktName = Data.Substring(spos, epos - spos);
        NotifyFunctionValidated(true, FktName);
      }

      if (Data.Contains("failed.")) {
        int epos = Data.IndexOf(" failed.");
        int spos = Data.IndexOf("Verification of ") + 16;
        string FktName = Data.Substring(spos, epos - spos);
        NotifyFunctionValidated(false, FktName);
      }

      if (Data.Contains("%")) {
        Data = Data.Substring(Data.IndexOf("Verification"), Data.Length - Data.IndexOf("Verification"));
        AddInGlobals.DTE.StatusBar.Progress(true, Data + " in " + JobPipe[activeJobNr], activeJobNr + 1, JobPipe.Count);
      }
      
      return;
    }

    public static void AddJob(VerifyJob Job)
    {
      JobPipe.Add(Job);
    }

    public delegate void VoidAction();

    public static void BeforeExecute() {
      AddInGlobals.VCCToolsMenu.Visible = false;
    }

    public static void AfterExecute() {
      AddInGlobals.VCCToolsMenu.Visible = true;
    }

    public static void Execute() {
      BeforeExecute();
      System.Threading.Thread thread = new System.Threading.Thread(delegate() 
        { ExecuteWorker();
          if (JobPipe.Count != 0) { //Happens when documents not saved!
            JobPipe[activeJobNr].AfterExecuteDelegate();
            AfterExecute();            
          }
        }
        );
      thread.Start();
    }
    
    private static void ExecuteWorker() {
      bRunning = true;
      
      bool errorEncountered = false;

      if (!Utilities.SaveAllOpenDocuments()) {
        JobPipe.Clear();
        bRunning = false;
        return;
      }
      
      NotifyExecute(JobPipe);
      AddInGlobals.DTE.StatusBar.Progress(true, "Start Verifying", 0, JobPipe.Count);
      AddInGlobals.DTE.StatusBar.Animate(true, vsStatusAnimation.vsStatusAnimationGeneral);

      AddInGlobals.BuildPane.Clear();
      for (int i = 0; i < JobPipe.Count; i++) {
        activeJobNr = i;
        if (bCancel || MaxErrorsReached())
          break;
        AddInGlobals.DTE.StatusBar.Progress(true, "Verifying: " + JobPipe[i], i + 1, JobPipe.Count);

        //Probe seed 0.1.2.3.4
        if (AddinSettingsManager.RandomSeedEnabled && AddinSettingsManager.RandomSeed == 9) {
          for (int seed = 0; seed < 5; seed++) {
            AddinSettingsManager.RandomSeed = seed;
            int exitCode = JobPipe[i].Execute();
            errorEncountered |= (exitCode != 0);
            if (exitCode != 0) {
              Cancel();
              RandomSeedFaildMessage(seed);
              break;
            } //if
          } //for
          AddinSettingsManager.RandomSeed = 9;
        }
        else {
          int exitCode = JobPipe[i].Execute();
          errorEncountered |= (exitCode != 0);
        }
      }
      try {
        AddInGlobals.DTE.StatusBar.Animate(false, vsStatusAnimation.vsStatusAnimationGeneral);

        AddInGlobals.DTE.StatusBar.Progress(false, "", 100, 100);
      }
      catch
      {}
      AddInGlobals.DTE.StatusBar.Text = errorEncountered ? "Verification failed." : "Verification succeeded.";
      bRunning = false;
      NotifyJobsDone(Convert.ToInt32(errorEncountered));
    }

    delegate void RandomSeedFaildMessageDelegate(int seed);
    private static void RandomSeedFaildMessage(int seed) {
      if (AddInGlobals.VCCPane.InvokeRequired) {
        AddInGlobals.VCCPane.Invoke(new RandomSeedFaildMessageDelegate(RandomSeedFaildMessage), seed);
      } else {
        System.Windows.Forms.MessageBox.Show(String.Format("There is a verification error with random seed {0}. Verification stops here.", seed));
      }
    }

    /// <summary>
    /// Returns true if ErrorList has more than 100 Items.
    /// </summary>
    /// <returns></returns>
    private static bool MaxErrorsReached() {
      if (AddInGlobals.DTE.ToolWindows.ErrorList.ErrorItems.Count > 100) {
        AddInGlobals.BuildPane.OutputString("Verification stopped, too much errors...");
        return true;
      }
      else {
        return false;
      }
    }

  }
}
