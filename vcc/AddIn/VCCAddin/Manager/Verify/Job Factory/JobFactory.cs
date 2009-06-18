//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using VerifiedCCompilerAddin.Forms;
using System.IO;
using VerifiedCCompilerAddin.Manager.Settings;
using EnvDTE;

namespace VerifiedCCompilerAddin.Manager.Verify {

  public static class JobFactory {

    public static VerifyJob CreateJob(JobType jobType) {

      VerifyJob job = null;
      switch (jobType) {
        case JobType.TypeAdmissiblityCheck:
          job = new TypeAdmissiblityCheck(AddInGlobals.ActiveDocument);
          break;
        case JobType.TypeAndGroupAdmissiblityCheck:
          job = new TypeAndGroupAdmissiblityCheck(AddInGlobals.ActiveDocument);
          break;
        case JobType.CreateBoogie:
          job = new CreateBoogie(AddInGlobals.ActiveDocument);
          break;
        case JobType.CustomVerify:
          job = new CustomVerify(AddInGlobals.ActiveDocument);
          break;
        case JobType.LaunchZ3Visualizer:
          job = new LaunchZ3Visualizer(AddInGlobals.ActiveDocument);
          break;
        case JobType.SingleFileVerify:
          job = new SingleFileVerify(AddInGlobals.ActiveDocument);
          break;
        case JobType.SingleFileFunction:
          job = new SingleFileFuntion(AddInGlobals.ActiveDocument);
          break;
        case JobType.SingleFileInSolution:
          job = new SingleFileInSolution();
          break;
        case JobType.LaunchZ3Inspector:
          job = new LaunchZ3Inspector(AddInGlobals.ActiveDocument);
          break;
      }

      return job;
    }

  }
}
