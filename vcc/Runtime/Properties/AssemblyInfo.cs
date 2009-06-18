﻿//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: AssemblyTitle("VccRuntime ")]
[assembly: AssemblyDescription("Runtime for the Microsoft Research Vcc language")]
#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif
[assembly: AssemblyTrademark("Microsoft Research Vcc")]
[assembly: AssemblyProduct("Vcc Compiler")]
[assembly: AssemblyCulture("")]
[assembly: ComVisible(false)]

