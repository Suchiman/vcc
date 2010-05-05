//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using Microsoft.Cci;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc {
  class ErrorReporter : ISymbolSyntaxErrorsReporter {
    private ErrorReporter() { }
    internal static readonly ErrorReporter Instance = new ErrorReporter();
  }
}