
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

  let contractFunctionNames = Set.ofList [
                                            "VCC::Requires"
                                            "VCC::Ensures"
                                            "VCC::Reads"
                                            "VCC::Writes"
                                            "VCC::Decreases"
                                         ]

  let isContractFunction (fn : Function) = Set.contains fn.Name contractFunctionNames

  let specialGlobalNames = Set.ofList [
                                        "VCC::IMPLIES"
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

    let rewriteLiterals self = function
      | IntLiteral(ec, i)  when ec.Type = Type.Bool -> Some(BoolLiteral(ec, i.IsOne))
      | _ -> None

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

        | Macro(ec, "dot", [e0; UserData(_, field)]) ->
          match field with
            | :? Field as f -> 
              Some(Expr.Deref(ec, Dot({ec with Type = Type.MkPtr(ec.Type, false)}, 
                                      Macro({e0.Common with Type = Type.MkPtrToStruct(f.Parent)}, "&", [self e0]),
                                      f)))
            | _ -> helper.Oops(ec.Token, "unexpected UserData type in 'dot'"); None

        | Macro(_, "implicit_cast", [Cast(ec, cs, expr) as cast]) -> 
          match cs with
            | Checked -> 
              if not (Type.ConversionIsLossless(expr.Type, ec.Type)) then
                helper.Error(ec.Token, 37, "Cannot implicitly convert type '" + expr.Type.ToString() + "' to '" + ec.Type.ToString() + "'")
              Some(self cast)
            | _ -> Some(self cast)

        | _ -> None
  
    // ============================================================================================================    

    let rewriteExtraOps self = function
      | Prim(ec, Op("||", cs), _) as disjunction -> 
        // an implication "a ==> b" is rewritten to "a || VCC::IMPLIES || b", which we need to undo here

        let rec collectDisjuncts = function
          | Prim(ec, Op("||", _), [e0; e1]) -> collectDisjuncts e0 @ collectDisjuncts e1
          | expr -> [self expr]
        let rec splitAtImplies lhs = function
          | [] -> (List.rev lhs, [])
          | (Ref(_, {Name = "VCC::IMPLIES"})) :: rhs -> (List.rev lhs, rhs)
          | e :: rhs -> splitAtImplies (e::lhs) rhs
        let rec disjunctListToImplication disjuncts =
          match splitAtImplies []  disjuncts with
              | (lhs, []) -> TransUtil.multiOr lhs
              | (lhs, rhs) -> boolOp "==>" (TransUtil.multiOr lhs) (disjunctListToImplication rhs)

        Some((disjunction |> collectDisjuncts |> disjunctListToImplication).WithCommon(ec))

      | _ -> None

    // ============================================================================================================    
    
    let insertBoolConversion self = 

      // insert conversion to bool where this is expected later

      let toBool (expr:Expr) = 
        match expr.Type with
          | Type.Bool -> self expr
          | _ -> Cast({expr.Common with Type = Type.Bool}, CheckedStatus.Unchecked, self expr)

      function 
        | Prim(ec, (Op(("!"|"||"|"&&"), _) as op), args) ->
          Some(Prim(ec, op, args |> List.map self |> List.map toBool))
        | _ -> None
          
    // ============================================================================================================    

    let reportErrors self = 
      
      let rejectFloats (expr : Expr) =
        if expr.Type.IsFloat then 
          helper.Error(expr.Token, 9801, "floating point types are currently not supported")
        None

      rejectFloats

    // ============================================================================================================    

    let collectContracts decls =

      // move contracts from list of statements to the surrounding block or function

      let findContracts stmts =
        let findContracts' (bc:BlockContract) = function
          | Call(ec, fn , [], [arg]) when isContractFunction fn ->
            match fn.Name with
              | "VCC::Requires" ->  { bc with Requires = arg :: bc.Requires }
              | "VCC::Ensures" ->   { bc with Ensures = arg :: bc.Ensures }
              | "VCC::Reads" ->     { bc with Reads = arg :: bc.Reads }
              | "VCC::Writes" ->    { bc with Writes = arg :: bc.Writes }
              | "VCC::Decreases" -> { bc with Decreases = arg :: bc.Decreases }
              | _ -> die()
          | Call(ec, fn, _, _) when isContractFunction fn ->
            helper.Oops(ec.Token, "unhandled occurrence of contract function")
            bc
          | _ -> bc

        let found = List.fold findContracts' BlockContract.Empty stmts
        { Requires = List.rev found.Requires
          Ensures = List.rev found.Ensures
          Reads = List.rev found.Reads
          Writes = List.rev found.Writes
          Decreases = List.rev found.Decreases
          IsPureBlock = found.IsPureBlock }

      let removeContracts = 
        let isNoContract = function
          | Call(ec, fn , [], [arg]) when isContractFunction fn -> false
          | _ -> true
        List.filter isNoContract

      let pullOutContracts self = function
        | Block(ec, stmts, None) ->
          let bc = findContracts stmts
          if bc.IsEmpty then None else Some(Block(ec, stmts |> removeContracts |> List.map self, Some bc))
        | _ -> None

      for d in decls do
        match d with
          | Top.FunctionDecl({Body = Some body} as fn) ->
            let body' = body.SelfMap pullOutContracts
            match body' with
              | Block(ec, stmts, Some bc) ->
                fn.Requires <- bc.Requires
                fn.Ensures <- bc.Ensures
                fn.Reads <- bc.Reads
                fn.Writes <- bc.Writes
                fn.Variants <- bc.Decreases
                fn.Body <- Some(Block(ec, stmts, None))
              | _ -> ()
          | _ -> ()

      decls

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
        | Top.FunctionDecl(fn) when isSpecialFunction fn || isContractFunction fn -> false
        | Top.Global({Name = name}, _) when Set.contains name specialGlobalNames -> false
        | _ -> true

      List.filter filterSpecialDecls decls

    // ============================================================================================================    

    helper.AddTransformer ("cpp-begin", TransHelper.DoNothing)

    helper.AddTransformer ("cpp-errors", TransHelper.Expr reportErrors)
    helper.AddTransformer ("cpp-rewrite-literals", TransHelper.Expr rewriteLiterals)
    helper.AddTransformer ("cpp-rewrite-functions", TransHelper.Expr rewriteSpecialFunctions)
    helper.AddTransformer ("cpp-rewrite-macros", TransHelper.Expr rewriteExtraMacros)
    helper.AddTransformer ("cpp-bool-conversion", TransHelper.Expr insertBoolConversion)
    helper.AddTransformer ("cpp-rewrite-ops", TransHelper.Expr rewriteExtraOps)
    helper.AddTransformer ("cpp-remove-decls", TransHelper.Decl removeSpecialDecls)
    helper.AddTransformer ("cpp-contracts", TransHelper.Decl collectContracts)

    helper.AddTransformer ("cpp-end", TransHelper.DoNothing)
