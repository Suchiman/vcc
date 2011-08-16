//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
namespace VerifiedCCompilerAddin.Commands {

  public static class VCCBindings {
    //Base Key ALT+V, followed by a secound Key/-combination
    public static string SingleFile = "Global::ALT+V, ALT+F";
    public static string SingleFileFunction = "Global::ALT+V, F";
    public static string TypeOnlyAdmissibility = "Global::ALT+V, T";
    public static string TypeGroupAdmissibility = "Global::ALT+V, G";
    public static string SingleFileFunctionUpToHere = "Global::ALT+V, CTRL+F";
    public static string Cancel = "Global::ALT+V, ALT+C";
    public static string SingleProject = "Global::ALT+V, ALT+P";
    public static string SingleSolution = "Global::ALT+V, ALT+S";
    public static string CustomSingleFile = "Global::ALT+V, C";
    public static string VCCPane = "Global::ALT+V, W";
    public static string AssertSelection = "Global::ALT+V, A";
    public static string Settings = "Global::ALT+V, S";
    public static string AssertToAssume = "Global::ALT+V, CTRL+A";
    public static string AssumeToAssert = "Global::ALT+V, SHIFT+A";
    public static string CreateBoogie = "Global::ALT+V, B";
    public static string LaunchZ3Viewer = "Global::ALT+V, Z";
    public static string ShowFunctionModel = "Global::ALT+V, M";
    public static string LaunchZ3Inspector = "Global::ALT+V, I";
  }
}