//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using Microsoft.Cci;
using Microsoft.Boogie;

namespace Microsoft.Research.Vcc
{
  public static class VisitorHelper
  {
    private class DeferredToken
    {
      private readonly IEnumerable<ILocation> locations;

      public DeferredToken(IEnumerable<ILocation> locations) {
        this.locations = locations;
      }

      public Token GetToken() {
        Token result = Token.NoToken;
        foreach (ILocation loc in locations) {
          IPrimarySourceLocation/*?*/ sloc = loc as IPrimarySourceLocation;
          if (sloc == null) {
            IDerivedSourceLocation/*?*/ dloc = loc as IDerivedSourceLocation;
            if (dloc != null) {
              foreach (IPrimarySourceLocation ploc in dloc.PrimarySourceLocations) {
                sloc = ploc;
                break;
              }
            } 
            else continue;
          }
          if (sloc == null) continue;
          result = new SourceLocationWrapper(sloc);
          break;
        }
        return result;
      }
    }

    public static Token GetTokenFor(IEnumerable<ILocation> locations)
    {
      if (IteratorHelper.EnumerableIsEmpty(locations)) return Token.NoToken;
      return new LazyToken(new DeferredToken(locations).GetToken);
    }

    public static ISourceLocation LocationFromToken(Token tok)
    {
      SourceLocationWrapper wrap = tok as SourceLocationWrapper;
      if (wrap != null) return wrap.sourceLocation;
      ForwardingToken fwd = tok as ForwardingToken;
      if (fwd != null) return LocationFromToken(fwd.tok);
      LazyToken lazyToken = tok as LazyToken;
      if (lazyToken != null) return LocationFromToken(lazyToken.DelayedToken);
      return SourceDummy.SourceLocation;
    }
  }
}
