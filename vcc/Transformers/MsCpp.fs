
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


  let init (helper:TransHelper.TransEnv) =

    let rewriteExtraMacros self = 

      // TODO: handle situations where the location incremented involves a func call, which should not be duplicated
      // this is also wrong in CCI at the moment 
      let handlePrePostIncrDecr e op isPost =
        let (init, tmp) = cache helper "incdec" e VarKind.Local
        let calc = Expr.Prim(e.Common, Op(op, CheckedStatus.Checked), [tmp; IntLiteral(e.Common, one)])
        let assign = Macro({e.Common with Type = Type.Void}, "=", [e; calc])
        if isPost then Expr.MkBlock(init @ [assign]) else Expr.MkBlock(init @ [assign; tmp])

      function
        | Macro(ec, "init", [lhs; rhs]) -> Some(Macro(ec, "=", [self lhs; self rhs]))
        | Macro(_, "()++", [e]) -> Some(handlePrePostIncrDecr e "+" true)
        | Macro(_, "()--", [e]) -> Some(handlePrePostIncrDecr e "-" true)
        | Macro(_, "++()", [e]) -> Some(handlePrePostIncrDecr e "+" false)
        | Macro(_, "--()", [e]) -> Some(handlePrePostIncrDecr e "-" false)

        | _ -> None
    


    helper.AddTransformer ("cpp-begin", TransHelper.DoNothing)

    helper.AddTransformer ("cpp-rewrite-macros", TransHelper.Expr rewriteExtraMacros)

    helper.AddTransformer ("cpp-end", TransHelper.DoNothing)


