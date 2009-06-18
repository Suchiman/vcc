//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using Microsoft.Cci.Ast;
using Microsoft.Cci;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc {
  class ErrorReporter : ISymbolSyntaxErrorsReporter {
    private ErrorReporter() { }
    internal static readonly ErrorReporter Instance = new ErrorReporter();
  }
}