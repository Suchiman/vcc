//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace VerifiedCCompilerAddin.Manager.Verify {
  
  public enum JobType {
    TypeAdmissiblityCheck,
    TypeAndGroupAdmissiblityCheck,
    CreateBoogie,
    CustomVerify,
    LaunchZ3Visualizer,
    LaunchZ3Inspector,
    SingleFileVerify,
    SingleFileFunction,
    SingleFileInSolution
  }

}