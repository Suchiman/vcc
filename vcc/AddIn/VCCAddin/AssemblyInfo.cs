//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System.Reflection;
using System.Runtime.InteropServices;

//^ [assembly: Microsoft.Contracts.Verify(false)]

[assembly: AssemblyTitle("VerifiedCCompilerAddin")]
[assembly: AssemblyDescription("Visual Studion Add-in for the Microsoft Research VerifiedC language")]
#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif
[assembly: AssemblyTrademark("Microsoft Research VerifiedC")]
[assembly: AssemblyProduct("VerifiedC Compiler")]
[assembly: AssemblyCulture("")]
[assembly: ComVisible(true)]

