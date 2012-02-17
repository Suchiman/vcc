
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

  let specialFunctionMap = Map.ofList [
                                                    "VCC::Activeclaim",   "_vcc_active_claim"
                                                    "VCC::Claimcount",    "_vcc_ref_cnt"
                                                    "VCC::Claims",        "_vcc_claims"
                                                    "VCC::Closed",        "_vcc_closed"
                                                    "VCC::Extent",        "_vcc_extent"
                                                    "VCC::Fresh",         "_vcc_is_fresh"
                                                    "VCC::Mallocroot",    "_vcc_is_malloc_root"
                                                    "VCC::Mutable",       "_vcc_mutable"
                                                    "VCC::Owner",         "_vcc_owner"
                                                    "VCC::Owns",          "_vcc_owns"
                                                    "VCC::Span",          "_vcc_span"
                                                    "VCC::Threadlocal",   "_vcc_thread_local2"
                                                    "VCC::Valid",         "_vcc_typed2"
                                                    "VCC::Wrapped",       "_vcc_wrapped"
                                      ]

  let specialFunctionWithContractMap = Map.ofList [
                                                    "VCC::Unwrap",        "_vcc_unwrap"
                                                    "VCC::Wrap",          "_vcc_wrap"
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

  let (|StartsWith|_|) prefix (s:string) = if s.StartsWith(prefix, System.StringComparison.Ordinal) then Some () else None

  let nongeneric (name:string) = 
    let endpos = name.IndexOf('<')
    if endpos = -1 then name else name.Substring(0, endpos)

  let (|SpecialCallTo|_|) name = Map.tryFind (nongeneric name) specialFunctionMap
          
  //============================================================================================================    


  let init (helper:TransHelper.TransEnv) =

    // ============================================================================================================    

    let rewriteLiterals self = function
      | IntLiteral(ec, i)  when ec.Type = Type.Bool -> Some(BoolLiteral(ec, i.IsOne))
      | Macro(_, "implicit_cast", [Cast(ec, cs, IntLiteral(_, n))]) 
      | Cast(ec, cs, IntLiteral(_, n)) when TransUtil.intInRange ec.Type n ->
          Some(IntLiteral(ec, n))
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

        | Macro(ec, "init", [arr; Macro(_, "array_init", args)]) ->         
          let assignIdx idx (arg:Expr) = 
            Expr.Macro({ec with Type = Type.Void}, "=", [Expr.Deref({arr.Common with Type = arg.Type}, Index(arr.Common, arr, mkInt idx)); arg])
          Some(Expr.MkBlock(List.mapi assignIdx (List.map self args)))

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
            | _ -> ()
          Some(self cast)
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
          | (Ref(_, {Name = "VCC::Implies"})) :: rhs -> (List.rev lhs, rhs)
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

    let handleStackArrays decls = 
    
      let varSubst = new Dict<_,_>()
      let typeSubst = new Dict<_,_>()

      let replaceDeclsAndAllocate self = function
        | VarDecl(ec, ({Type = Array(t, n)} as v), attr) as decl ->
          let ptrType = Type.PhysPtr(t) // TODO: could be spec ptr
          let vptr = Variable.CreateUnique v.Name ptrType v.Kind
          let vptrRef = Expr.Ref({ec with Type=v.Type}, vptr)
          let stackAlloc = Expr.Macro({ ec with Type = ptrType }, "stack_allocate_array", [mkInt n; Expr.False])
          let assign = Expr.Macro(ec, "=", [vptrRef; stackAlloc])
          varSubst.Add(v, vptr)
          typeSubst.[v.Type] <- ptrType
          Some(Expr.Macro(ec, "fake_block", [VarDecl(ec, vptr, attr); assign]))
        | _ -> None

      let typeSubst t = 
        match typeSubst.TryGetValue(t) with
          | true, t' -> Some t'
          | _ -> None

      decls |> deepMapExpressions replaceDeclsAndAllocate |> mapExpressions (fun _ (expr:Expr) -> expr.SubstType(typeSubst, varSubst))

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
          | CallMacro(ec, "VCC::Requires", [], [arg])         ->      { bc with Requires = arg :: bc.Requires }
          | CallMacro(ec, "VCC::Ensures", [], [arg])          ->      { bc with Ensures = arg :: bc.Ensures }
          | CallMacro(ec, StartsWith "VCC::Reads", [], args) ->      { bc with Reads = args @ bc.Reads }
          | CallMacro(ec, StartsWith "VCC::Writes", [], args) ->     { bc with Writes = args @ bc.Writes }
          | CallMacro(ec, StartsWith "VCC::Decreases", [], args) ->  { bc with Decreases = args @ bc.Decreases }
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
          | CallMacro(ec, "VCC::Requires", [], [_])         
          | CallMacro(ec, "VCC::Ensures", [], [_])          
          | CallMacro(ec, StartsWith "VCC::Reads", [], _) 
          | CallMacro(ec, StartsWith "VCC::Writes", [], _)
          | CallMacro(ec, StartsWith "VCC::Decreases", [], _) ->  false
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

    let rewriteSpecialFunctionsWithContracts decls =
      
      // for VCC functions with contracts (e.g. VCC::Wrap), pick a single representative from the
      // possible many generic instantiations, rename it to the internal name and
      // rewrite all calls to these functions into calls to this representative

      let representatives = new Dict<_,_>()

      let pickRepresentatives = function
        | Top.FunctionDecl(fn) ->
          let name = nongeneric fn.Name
          match Map.tryFind name specialFunctionWithContractMap with
            | None -> true
            | Some name' ->
              if representatives.ContainsKey(name) then false 
              else
                fn.Body <- None // we should have picked up the contracts by now
                fn.Name <- name'
                representatives.Add(name, fn)
                true
        | _ -> true

      let decls' = List.filter pickRepresentatives decls

      let replaceCalls self = function
        | Call(ec, fn, [], args) ->
          match representatives.TryGetValue (nongeneric fn.Name) with
            | true, fn' -> Some(Call(ec, fn', [], List.map self args))
            | false, _ -> None
        | _ -> None

      decls' |> deepMapExpressions replaceCalls

    // ============================================================================================================    

    let rewriteSpecialFunctions self = 

      let selfs = List.map self

      function
        | CallMacro(ec, "VCC::Assert", [], [arg]) -> 
          Some(Assert(ec, self arg, []))
        | CallMacro(ec, "VCC::Assume", [], [arg]) -> 
          Some(Assume(ec, self arg))
        | CallMacro(ec, StartsWith "VCC::Result", [], []) ->
          Some(Result(ec))
        | CallMacro(ec, StartsWith "VCC::Old", [], [arg]) ->
          Some(Old(ec, Macro({ec with Type = Type.MathState}, "prestate", []), self arg))
        | CallMacro(ec, SpecialCallTo(tgt), [], args)  ->
          Some(Macro(ec, tgt, List.map self args))
        | _ -> None

    // ============================================================================================================    

    let rewriteGhostParameters decls = 

      decls

    // ============================================================================================================    

    let rewriteBlockDecorators self = 

      // associate a call to a construct that applies to the following block (like VCC::Unwrapping or VCC::Atomic) 
      // with the following stmt (block) and convert it into the format expected by later processing

      let rec rewriteBlockDecorators' acc = function
        | [] -> List.rev acc
        | CallMacro(ec, StartsWith "VCC::Unwrapping", [], args) :: stmts ->
          match rewriteBlockDecorators' [] stmts with
            | [] -> 
              helper.Warning(ec.Token, 9127, "_(unwrapping ...) without following statements is ignored")
              List.rev acc
            | stmt :: stmts -> 
              List.rev acc @ [Macro(ec, "unwrapping", stmt :: args)] @ stmts
        | CallMacro(ec, StartsWith "VCC::Atomic", [], args) :: stmts ->
          match rewriteBlockDecorators' [] stmts with
            | [] -> 
              helper.Warning(ec.Token, 9127, "_(atomic ...) without following statements is ignored")
              List.rev acc
            | stmt :: stmts -> 
              List.rev acc @ [Atomic(ec, args, stmt)] @ stmts
        | stmt :: stmts -> rewriteBlockDecorators' ((self stmt)::acc) stmts
          
      function
        | Block(ec, stmts, bc) -> Some(Block(ec, rewriteBlockDecorators' [] stmts, bc))
        | _ -> None
      
    // ============================================================================================================    

    let removeSpecialDecls decls =

      let filterSpecialDecls = function
        | Top.FunctionDecl({Name = StartsWith "VCC::"}) -> false
        | Top.Global({Name = StartsWith "VCC::"}, _) -> false
        | _ -> true

      List.filter filterSpecialDecls decls

    // ============================================================================================================    

    helper.AddTransformer ("cpp-begin", TransHelper.DoNothing)

    helper.AddTransformer ("cpp-errors", TransHelper.Expr reportErrors)
    helper.AddTransformer ("cpp-contracts", TransHelper.Decl collectContracts)
    helper.AddTransformer ("cpp-ghost-params", TransHelper.Decl rewriteGhostParameters)
    helper.AddTransformer ("cpp-rewrite-literals", TransHelper.Expr rewriteLiterals)
    helper.AddTransformer ("cpp-stack-arrays", TransHelper.Decl handleStackArrays)
    helper.AddTransformer ("cpp-rewrite-functions", TransHelper.Expr rewriteSpecialFunctions)
    helper.AddTransformer ("cpp-rewrite-block-decorators", TransHelper.Expr rewriteBlockDecorators)
    helper.AddTransformer ("cpp-rewrite-functions-with-contracts", TransHelper.Decl rewriteSpecialFunctionsWithContracts)
    helper.AddTransformer ("cpp-rewrite-macros", TransHelper.Expr rewriteExtraMacros)
    helper.AddTransformer ("cpp-bool-conversion", TransHelper.Expr insertBoolConversion)
    helper.AddTransformer ("cpp-rewrite-ops", TransHelper.Expr rewriteExtraOps)
    helper.AddTransformer ("cpp-remove-decls", TransHelper.Decl removeSpecialDecls)

    helper.AddTransformer ("cpp-end", TransHelper.DoNothing)
