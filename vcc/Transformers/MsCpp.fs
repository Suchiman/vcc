
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

  let specialFunctionNames = Set.ofList [
                                          "VCC::Assert"
                                          "VCC::Assume"
                                        ]

  let isSpecialFunction (fn : Function) = Set.contains fn.Name specialFunctionNames

  let specialGlobalNames = Set.ofList [
                                        "VCC::Implies"
                                      ]

  let incrOpTable = Map.ofList [
                                  "()++", ("+", true)
                                  "()--", ("-", true)
                                  "++()", ("+", false)
                                  "--()", ("-", false)
                                ]

  let assignOpTable = Map.ofList [
                                    "+=", ("+", true)
                                    "-=", ("-", true)
                                    "*=", ("*", true)
                                    "/=", ("/", true)
                                    "%=", ("%", true)
                                    "&=", ("&", false)
                                    "|=", ("|", false)
                                    "^=", ("^", false)
                                    "<<=", ("<<", false)
                                    ">>=", (">>", false)
                                  ]

  // ============================================================================================================    

  let init (helper:TransHelper.TransEnv) =

    // ============================================================================================================    

    let rewriteExtraMacros self = 

      // TODO: handle situations where the location incremented involves a func call, which should not be duplicated
      // this is also wrong in CCI at the moment 
      let handlePrePostIncrDecr e op isPost =
        let (init, tmp) = cache helper "incdec" e VarKind.Local
        let calc = Expr.Prim(e.Common, Op(op, CheckedStatus.Checked), [tmp; IntLiteral(e.Common, one)])
        let assign = Macro(e.Common, "=", [e; calc])
        if isPost then Expr.MkBlock(init @ [assign]) else Expr.MkBlock(init @ [assign; tmp])

      let handleAssignOp ec op (e0:Expr) e1 =
        let calc = Expr.Prim(e0.Common, op, [e0; e1])
        Macro({ec with Type = Type.Void}, "=", [e0; calc])        

      function
        | Macro(ec, "init", [lhs; rhs]) -> Some(Macro(ec, "=", [self lhs; self rhs]))
        | Macro(_, incrOp, [e; IntLiteral(_, _one)]) when _one.IsOne && incrOpTable.ContainsKey incrOp -> 
          let (op, isPost) = Map.find incrOp incrOpTable
          Some(handlePrePostIncrDecr (self e) op isPost)
        | Macro(ec, assignOp, [e0; e1]) when assignOpTable.ContainsKey assignOp ->
          let (op, isChecked) = Map.find assignOp assignOpTable
          Some(handleAssignOp ec (Op(op, if isChecked then Checked else Unchecked)) (self e0) (self e1))
        | _ -> None
  
    // ============================================================================================================    

    let rewriteExtraOps self = function
      | Prim(ec, Op("||", cs), _) as disjunction -> 
        // an implication "a ==> b" is rewritten to "a || VCC::Implies || b", which we need to undo here

        let rec collectDisjuncts = function
          | Prim(ec, Op("||", _), [e0; e1]) -> collectDisjuncts e0 @ collectDisjuncts e1
          | expr -> [self expr]
        let rec splitAtImplies lhs = function
          | [] -> (List.rev lhs, [])
          | (Ref(_, {Name = "VCC::Implies"})) :: rhs -> (List.rev lhs, rhs)
          | e :: rhs -> splitAtImplies (e::lhs) rhs
        let rec disjunctListToImplication disjuncts =
          match splitAtImplies []  disjuncts with
              | (lhs, []) -> TransUtil.multiOr lhs
              | (lhs, rhs) -> boolOp "==>" (TransUtil.multiOr lhs) (disjunctListToImplication rhs)

        Some((disjunction |> collectDisjuncts |> disjunctListToImplication).WithCommon(ec))

      | _ -> None

    // ============================================================================================================    

    let retypeOperators self = function
      | Prim(ec, (Op(("<"|"<="|">"|">="|"=="|"!="), _) as op), args) ->
        Some(Prim({ec with Type = Type.Bool}, op, List.map self args))
      | _ -> None
          
    // ============================================================================================================    
    
    let insertBoolConversion self = 

      // insert conversion to bool where this is expected later

      let toBool (expr:Expr) = 
        match expr.Type with
          | Type.Bool -> self expr
          | _ -> Cast({expr.Common with Type = Type.Bool}, CheckedStatus.Unchecked, self expr)

      function
        | Macro(ec, "for", [contr; inits; cond; inc; body]) ->
          Some(Macro(ec, "for", [self contr; self inits; toBool cond; self inc; self body]))

        | If(ec, tc, cond, th, el) -> 
          Some(If(ec, tc, toBool cond, self th, self el))

        | _ -> None
          
    // ============================================================================================================    

    let rewriteSpecialFunctions self = 

      let selfs = List.map self

      function
        | Call(ec, {Name = "VCC::Assert"}, [], [arg]) -> Some(Assert(ec, self arg, []))
        | Call(ec, {Name = "VCC::Assume"}, [], [arg]) -> Some(Assume(ec, self arg))
        | Call(ec, fn, _, _) when isSpecialFunction fn ->
          helper.Oops(ec.Token, "Unhandled special function call to '" + fn.Name + "'")
          None
        | _ -> None

    // ============================================================================================================    

    let removeSpecialDecls decls =

      let filterSpecialDecls = function
        | Top.FunctionDecl(fn) when isSpecialFunction fn -> false
        | Top.Global({Name = name}, _) when Set.contains name specialGlobalNames -> false
        | _ -> true

      List.filter filterSpecialDecls decls

    // ============================================================================================================    

    helper.AddTransformer ("cpp-begin", TransHelper.DoNothing)

    helper.AddTransformer ("cpp-rewrite-functions", TransHelper.Expr rewriteSpecialFunctions)
    helper.AddTransformer ("cpp-rewrite-macros", TransHelper.Expr rewriteExtraMacros)
    helper.AddTransformer ("cpp-rewrite-ops", TransHelper.Expr rewriteExtraOps)
    helper.AddTransformer ("cpp-remove-decls", TransHelper.Decl removeSpecialDecls)
    //helper.AddTransformer ("cpp-retype-ops", TransHelper.Expr retypeOperators)
    //helper.AddTransformer ("cpp-bool-conversion", TransHelper.Expr insertBoolConversion)

    helper.AddTransformer ("cpp-end", TransHelper.DoNothing)
