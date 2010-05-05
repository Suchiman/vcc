//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;

namespace VerifiedCCompilerAddin.Manager.Verify {
  #region EventArgs

  public class JobDoneEventArgs : EventArgs {
    public JobDoneEventArgs(int Result) {
      this.result = Result;
    }
    public int Result {
      get {
        return (result);
      }
    }
    int result;
  }

  public class JobsDoneEventArgs : JobDoneEventArgs {
    public JobsDoneEventArgs(int Result)
      : base(Result) {
    }
  }

  public class JobsExecuteEventArgs : EventArgs {
    public JobsExecuteEventArgs(List<VerifyJob> JobPipe) {
      this.jobPipe = JobPipe;
    }
    public List<VerifyJob> JobPipe {
      get {
        return jobPipe;
      }
    }
    List<VerifyJob> jobPipe;
  }


  public class ProgressUpdateEventArgs : EventArgs {
    public ProgressUpdateEventArgs(int Progress, string FunctionName) {
      this.progress = Progress;
      this.FktName = FunctionName;
    }
    public int Progress {
      get {
        return (progress);
      }
    }
    public string FunctionName {
      get { return FktName; }
    }

    int progress;
    string FktName;
  }
  public class FunctionValidatedEventArgs : EventArgs {
    public FunctionValidatedEventArgs(bool State, string FunctionName) {
      this.state = State;
      this.FktName = FunctionName;
    }
    public bool State {
      get {
        return (state);
      }
    }
    public string FunctionName {
      get { return FktName; }
    }

    bool state;
    string FktName;
  }
  #endregion
}
