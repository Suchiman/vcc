//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using Microsoft.Cci;
using Microsoft.Cci.Ast;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc.Parsing
{

  internal class ParserV2 : Parser
  {
    internal ParserV2(Compilation compilation, ISourceLocation sourceLocation, List<IErrorMessage> scannerAndParserErrors) 
    : base(compilation, sourceLocation, scannerAndParserErrors) {
    }

    private SpecToken CurrentSpecToken {
      get {
        if (this.currentToken == Token.Identifier) {
          switch (this.scanner.GetIdentifierString()) {
            case "ghost":
              return SpecToken.Ghost;
            case "invariant":
              return SpecToken.Invariant;
            default:
              return SpecToken.None;
          }
        }
        return SpecToken.None;
      }
    }

    private enum SpecToken
    {
      None,
      Ghost,
      Invariant,
    }

    private struct SpecTokenSet
    {
      ulong bits;

      public SpecTokenSet(SpecToken t1, SpecToken t2) {
        this.bits = 0;
        this.bits |= (1ul << (int)t1);
        this.bits |= (1ul << (int)t2);
      }

      public SpecTokenSet(SpecToken t1, SpecToken t2, SpecToken t3) {
        this.bits = 0;
        this.bits |= (1ul << (int)t1);
        this.bits |= (1ul << (int)t2);
        this.bits |= (1ul << (int)t3);
      }

      [System.Diagnostics.DebuggerNonUserCode]
      public static SpecTokenSet operator |(SpecTokenSet ts, SpecToken t) {
        SpecTokenSet result = new SpecTokenSet();
        int i = (int)t;
        result.bits = ts.bits | (1ul << i);
        return result;
      }

      [System.Diagnostics.DebuggerNonUserCode]
      public static SpecTokenSet operator |(SpecTokenSet ts1, SpecTokenSet ts2) {
        SpecTokenSet result = new SpecTokenSet();
        result.bits = ts1.bits | ts2.bits;
        return result;
      }

      internal bool this[SpecToken t] {
        get {
          int i = (int)t;
          return (this.bits & (1ul << i)) != 0;
        }
      }
    }


  }

}