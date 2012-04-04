﻿using System;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Collections;

namespace Microsoft.Research.Vcc.Cpp
{
  class Program
  {
    static void Main()
    {
      var eel = FSharpList<CAST.Expr>.Empty;
      var eal = FSharpList<CAST.CustomAttr>.Empty;
      var evl = FSharpList<CAST.Variable>.Empty;
      var etvl = FSharpList<CAST.TypeVariable>.Empty;

      var body = FSharpOption<CAST.Expr>.Some(CAST.Expr.NewBlock(CAST.bogusEC, eel, null));
      var parent = FSharpOption<CAST.TypeDecl>.None;

      var fn =
        CAST.Top.NewFunctionDecl(
          new CAST.Function(CAST.bogusToken, CAST.Flags.None, CAST.Type.Void, "foo", parent, evl, etvl, eel, eel,
                            eel, eel, eel, eal, body, false, 0, 0, 0));
      var decls = FSharpList<CAST.Top>.Cons(fn, FSharpList<CAST.Top>.Empty);

      var driver = new CppDriver(new string[] {"dump before begin"});
      driver.WriteToBpl(decls, @"\temp\out.bpl");
      driver.Verify(decls);
    }
  }
}