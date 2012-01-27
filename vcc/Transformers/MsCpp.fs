
//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light


namespace Microsoft.Research.Vcc
 open Microsoft.Research.Vcc
 open Microsoft.Research.Vcc.Util
 open Microsoft.Research.Vcc.TransUtil
 open Microsoft.Research.Vcc.CAST
 
 module MsCpp =
  
  // ============================================================================================================    

  let rewriteExtraMacros self = function
    | Macro(ec, "init", [lhs; rhs]) -> Some(Macro(ec, "=", [self lhs; self rhs]))
    | _ -> None
    

  let init (helper:TransHelper.TransEnv) =

    helper.AddTransformer ("cpp-begin", TransHelper.DoNothing)

    helper.AddTransformer ("cpp-rewrite-macros", TransHelper.Expr rewriteExtraMacros)

    helper.AddTransformer ("cpp-end", TransHelper.DoNothing)


