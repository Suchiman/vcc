
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
                                                    "VCC::Activeclaim",         "_vcc_active_claim"
                                                    "VCC::Approves",            "_vcc_approves"
                                                    "VCC::Array",               "_vcc_as_array"
                                                    "VCC::Arrayrange",          "_vcc_array_range"
                                                    "VCC::Claimcount",          "_vcc_ref_cnt"
                                                    "VCC::Claims",              "_vcc_claims"
                                                    "VCC::Closed",              "_vcc_closed"
                                                    "VCC::Depends",             "_vcc_depends"
                                                    "VCC::Extent",              "_vcc_extent"
                                                    "VCC::Extentmutable",       "_vcc_extent_mutable"
                                                    "VCC::Fresh",               "_vcc_is_fresh"
                                                    "VCC::Mallocroot",          "_vcc_is_malloc_root"
                                                    "VCC::Mutable",             "_vcc_mutable"
                                                    "VCC::Mutablearray",        "_vcc_mutable_array"
                                                    "VCC::Notshared",           "_vcc_not_shared"
                                                    "VCC::Objectroot",          "_vcc_object_root"
                                                    "VCC::Owner",               "_vcc_owner"
                                                    "VCC::Owns",                "_vcc_owns"
                                                    "VCC::Programentrypoint",   "_vcc_program_entry_point"
                                                    "VCC::Span",                "_vcc_span"
                                                    "VCC::Threadlocal",         "_vcc_thread_local2"
                                                    "VCC::Valid",               "_vcc_typed2"
                                                    "VCC::Wrapped",             "_vcc_wrapped"
                                      ]

  let specialFunctionWithContractMap = Map.ofList [
                                                    "VCC::Unwrap",              "_vcc_unwrap"
                                                    "VCC::Wrap",                "_vcc_wrap"
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

  let sanitizeFullName (fullName : string) =
    let s = new System.Text.StringBuilder(fullName)
    s.Replace(' ', '#') |> ignore
    s.Replace('(', '$') |> ignore
    s.Replace(')', '$') |> ignore
    s.Replace('<', '$') |> ignore
    s.Replace('>', '$') |> ignore
    s.Replace('*', '^') |> ignore
    s.Replace(',', '`') |> ignore
    s.ToString()

  // ============================================================================================================    

  let (|StartsWith|_|) prefix (s:string) = if s.StartsWith(prefix, System.StringComparison.Ordinal) then Some () else None

  let (|Contains|_|) substr (s:string) = if s.Contains(substr) then Some() else None

  let nongeneric (name:string) = 
    let endpos = name.IndexOf('<')
    if endpos = -1 then name else name.Substring(0, endpos)

  let (|SpecialCallTo|_|) name = Map.tryFind (nongeneric name) specialFunctionMap
          
  //============================================================================================================    


  let init (helper:TransHelper.TransEnv) =

    // ============================================================================================================    

    let rewriteCasts self = 

      // perform some normalization on literals and casts
      // remove the 'implicit' marker on cast where we find them to be safe at compile time.

      let bfcil (t:Type) = function // _b_it_f_ield _c_onversion _i_s _l_ossless
        | { Offset = BitField(_,_,sSize) } as field ->
          let tIsSigned = t.IsSignedInteger
          let tSize = t.SizeOf * 8
          let sIsSigned = field.Type.IsSignedInteger
          if (tIsSigned = sIsSigned) then
            sSize <= tSize
          else if tIsSigned then
            sSize < tSize
          else false
        | _ -> die()

      let rec icil t = function // _i_nteger _c_onversion _i_s _l_ossless
        | IntLiteral(_, n) -> TransUtil.intInRange t n
        | Macro(_, "implicit_cast", [Cast(ec, _, e)])
        | Cast(ec, _, e) -> Type.ConversionIsLossless(ec.Type, t) || icil t e
        | Prim(_, Op(("+"|"~"), _), [e0]) -> icil t e0
        | Prim(_, Op(("+"|"-"|"*"|"|"|"&"|"^"),_), [e0; e1]) -> icil t e0 && icil t e1
        | Prim(_, Op(("/"|"%"|"<<"|">>"), _), [e0; _]) -> icil t e0
        | Macro(_, "ite", [_; _then; _else]) -> icil t _then && icil t _else
        | Macro(_, "dot",  [_; UserData(_, field)]) -> 
          match field with
            | :? Field as f when f.IsBitField -> bfcil t f
            | _ -> die()
        | Deref(_, Dot(_,_, f)) when f.IsBitField -> bfcil t f
        | expr -> Type.ConversionIsLossless(expr.Type, t)

      function
        | IntLiteral(ec, i) when ec.Type = Type.Bool -> Some(BoolLiteral(ec, i.IsOne))
        | IntLiteral(ec, i) when ec.Type._IsPtr -> Some(Macro(ec, "null", []))
        | Macro(ic, "implicit_cast", [Cast(ec, cs, e)]) when ec.Type._IsInteger ->
          let e' = self e
          if icil ec.Type e' then 
            Some(Cast(ec, cs, e')) 
          else 
            Some(Macro(ic, "implicit_cast", [Cast(ec, cs, e')]))

        | _ -> None

    // ============================================================================================================    

    let rewriteAssignOps self = 

      // rewrite '+=' and '++' like operations
    
      let handlePrePostIncrDecr e op isPost =
        let (init, tmp) = cache helper "incdec" e VarKind.Local
        let calc = Expr.Prim(e.Common, Op(op, CheckedStatus.Checked), [tmp; IntLiteral(e.Common, one)])
        let assign = Macro(e.Common, "=", [e; calc])
        if isPost then Expr.MkBlock(init @ [assign]) else Expr.MkBlock(init @ [assign; tmp])

      let handleAssignOp ec op (e0:Expr) e1 =
        let calc = Expr.Prim(e0.Common, op, [e0; e1])
        Macro(ec, "=", [e0; calc])        

      function

        | Macro(_, incrOp, [e; _]) when incrOpTable.ContainsKey incrOp -> 
          let (op, isPost) = Map.find incrOp incrOpTable
          Some(handlePrePostIncrDecr (self e) op isPost)
        
        | Macro(ec, assignOp, [e0; e1]) when assignOpTable.ContainsKey assignOp ->
          let (op, isChecked) = Map.find assignOp assignOpTable
          Some(handleAssignOp ec (Op(op, if isChecked then Checked else Unchecked)) (self e0) (self e1))

        | _ -> None

    // ============================================================================================================    

    let rewriteExtraMacros self = 

      // TODO: handle situations where the location incremented involves a func call, which should not be duplicated
      // this is also wrong in CCI at the moment 
      function

        | Macro(ec, "init", [arr; Macro(_, "array_init", args)]) ->         
          let assignIdx idx (arg:Expr) = 
            Expr.Macro({ec with Type = Type.Void}, "=", [Expr.Deref({arr.Common with Type = arg.Type}, Index(arr.Common, arr, mkInt idx)); arg])
          Some(Expr.MkBlock(List.mapi assignIdx (List.map self args)))

        | Macro(ec, "init", [lhs; rhs]) -> Some(Macro(ec, "=", [self lhs; self rhs]))
        
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

    let specialArgumentHandling decls = 

      let filterArguments args = 
        let hasVarArgs = ref false
        let rec loop acc = function
          | [] -> List.rev acc
          | (arg : Variable) :: args when arg.Type = Type.Void -> loop acc args
          | arg :: args when arg.Name = "..." -> hasVarArgs := true; loop acc args
          | arg :: args -> loop (arg::acc) args
        let args' = loop [] args
        (args', !hasVarArgs)

     
      for d in decls do
        match d with
          | Top.FunctionDecl(fn) -> 
            let (args, hasVarArgs) = filterArguments fn.Parameters  
            fn.Parameters <- args
            if hasVarArgs then fn.Flags <- fn.Flags ||| Flags.AcceptsExtraArguments
            
          | _ -> ()

      decls

    // ============================================================================================================    

    let collectContracts decls =

      // move contracts from list of statements to the surrounding block or function
      

      let findContracts stmts =

        let findContracts' (bc:BlockContract, invs) = function
          | Call(ec, {FriendlyName = StartsWith "VCC::Requires"}, [], [arg]) ->  { bc with Requires = arg :: bc.Requires }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Ensures"}, [], [arg]) ->   { bc with Ensures = arg :: bc.Ensures }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Reads"}, [], args) ->      { bc with Reads = args @ bc.Reads }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Writes"}, [], args) ->     { bc with Writes = args @ bc.Writes }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Decreases"}, [], args) ->  { bc with Decreases = args @ bc.Decreases }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Pure"}, [], []) ->         { bc with IsPureBlock = true }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Invariant"}, [], [arg]) -> bc, (arg :: invs)
          | _ -> bc, invs

        let (found, invs) = List.fold findContracts' (BlockContract.Empty, []) stmts
        { Requires = List.rev found.Requires
          Ensures = List.rev found.Ensures
          Reads = List.rev found.Reads
          Writes = List.rev found.Writes
          Decreases = List.rev found.Decreases
          IsPureBlock = found.IsPureBlock }, List.rev invs

      let removeContracts = 
        let isNoContract = function
          | Call(ec, {FriendlyName = StartsWith "VCC::Requires"}, [], [_])         
          | Call(ec, {FriendlyName = StartsWith "VCC::Ensures"}, [], [_])          
          | Call(ec, {FriendlyName = StartsWith "VCC::Reads"}, [], _) 
          | Call(ec, {FriendlyName = StartsWith "VCC::Writes"}, [], _)
          | Call(ec, {FriendlyName = StartsWith "VCC::Decreases"}, [], _) 
          | Call(ec, {FriendlyName = StartsWith "VCC::Pure"}, [], [])
          | Call(ec, {FriendlyName = StartsWith "VCC::Invariant"}, [], [_])
            ->  false
          | _ -> true
        List.filter isNoContract

      let pullOutContracts self = function
        | Macro(ec, "for", [ Macro(lec, "loop_contract", loopContract); init; cond; incr; Block(bec, stmts, None) ]) ->
          let (bc, invs) = findContracts stmts
          let body' = Block(bec, stmts |> removeContracts |> List.map self, None)
          let invs' = List.map Expr.MkAssert invs
          Some(Macro(ec, "for", [ Macro(lec, "loop_contract", invs' @ loopContract); init; cond; incr; body']))
        | Macro(ec, "for", _) -> 
          helper.Oops(ec.Token, "unexpected for loop structure")
          None
        | Block(ec, stmts, None) ->
          let (bc, invs) = findContracts stmts
          if not (List.isEmpty invs) then helper.Oops(invs.Head.Token, "invariant outside of loop")
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
                fn.CustomAttr <- (if bc.IsPureBlock then [VccAttr(AttrIsPure, "")] else []) @ fn.CustomAttr
                fn.Body <- Some(Block(ec, stmts, None))
              | _ -> fn.Body <- Some body'
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
          let name = nongeneric fn.FriendlyName
          match Map.tryFind name specialFunctionWithContractMap with
            | None -> true
            | Some name' ->
              if representatives.ContainsKey(name) then false 
              else
                fn.Body <- None // we should have picked up the contracts by now
                fn.Name <- name'
                fn.FriendlyName <- name'
                representatives.Add(name, fn)
                true
        | _ -> true

      let decls' = List.filter pickRepresentatives decls

      let replaceCalls self = function
        | Call(ec, fn, [], args) ->
          match representatives.TryGetValue (nongeneric fn.FriendlyName) with
            | true, fn' -> Some(Call(ec, fn', [], List.map self args))
            | false, _ -> None
        | _ -> None

      decls' |> deepMapExpressions replaceCalls

    // ============================================================================================================    

    let rewriteQuantifiers decls =

      // rewrites quantifier expressions, which have been turned into lambdas by the rewriter, back into
      // their original form

      let (|Quantifier|_|) = function
        | Call(ec, {FriendlyName = (StartsWith "VCC::Exists" | StartsWith "VCC::ForAll" as friendlyName)}, [], 
                   [Deref(_, Call(_, closureCtor, [], _this :: captures))])
          -> Some(ec, closureCtor, captures, if friendlyName.StartsWith("VCC::Exists") then QuantKind.Exists else QuantKind.Forall)
        | _ -> None

      let closures = new HashSet<_>()
      let lambdas = new Dict<_,_>()

      let findClosures self = function
        | Quantifier(_, {Parent = Some(closure)}, _, _) -> closures.Add(closure) |> ignore; true
        | _ -> true
               
      let findLambdas = function
        | Top.FunctionDecl({Parent = Some parent; FriendlyName = fname; Body = Some(body)} as lambda) 
          when closures.Contains(parent) && fname.EndsWith("::operator()") ->
          lambdas.Add(parent, (lambda, body))
        | _ -> ()

      let buildArgsToActualsMap args actuals = 
        let f map arg = function
          | Macro(_, "&", [actual]) -> Map.add arg actual map
          | _ -> die()
        List.fold2 f Map.empty args actuals

      let buildFieldsToActuals ctorBody argsToActuals =
        // build a structure that maps the fields in the closure to the locals that they are meant to capture

        let f2a = new Dict<_,_>()
        let f = function
          | Macro(_, "=", [Deref(_, Dot(_, Ref(_, {Name = "this"}), f)); Macro(_, "&", [Deref(_, Ref(_, v))])]) ->
            f2a.Add(f, (Map.find v argsToActuals))
          | Return(_, None) -> ()
          | expr -> helper.Oops(expr.Common.Token, "unexpected statement in closure constructor")

        match ctorBody with
          | Some(Block(_, assigns, _)) ->
            List.iter f assigns
            f2a
          | Some(expr) -> 
            helper.Oops(expr.Common.Token, "unexpected closure constructor structure when processing quantifiers")
            f2a
          | None -> f2a

      let makeArgsQuantifierBound args = 
        let v2v = new Dict<_,_>()
        let f (v:Variable) = 
          let v' = { v.UniqueCopy() with Kind = QuantBound }
          v2v.Add(v,v')
          v'
        (List.map f args), v2v

      let replaceClosureFieldsAndBoundVariables (f2a : Dict<_,_>) (v2v : Dict<_,_>) self = function
        | Deref(_, Deref(_, Dot(_, Ref(_, {Name = "this"}), f))) ->
          match f2a.TryGetValue(f) with
            | true, actual -> Some(actual)
            | false, _ -> None
        | Ref(ec, v) ->
          match v2v.TryGetValue(v) with
            | true, v' -> Some(Ref(ec, v'))
            | false, _ -> None
        | _ -> None

      let splitBody = function
        | Block(ec, stmts, None) ->
          let rec loop triggers = function
            | [Return(_, Some expr)] -> 
              expr, List.rev triggers
            | Call(ec, {FriendlyName = "VCC::Trigger"}, [], args) :: ((_ :: _) as stmts) ->
              loop (args::triggers) stmts
            | _ ->
              helper.Oops(ec.Token, "unexpected lambda body structure for quantifier")
              Expr.Bogus, []
          loop [] stmts
        | expr ->               
          helper.Oops(expr.Token, "unexpected lambda body structure for quantifier")
          Expr.Bogus, []

      let rewriteQuantifiers' self = function
        | Quantifier(ec, ({Parent = Some(closure)} as closureCtor), captures, kind) -> 
          match lambdas.TryGetValue(closure) with
            | true, (lambda, body) ->
              let expr, triggers = splitBody body
              let (expr':Expr) = self expr
              let argsToActuals = buildArgsToActualsMap closureCtor.Parameters.Tail captures
              let fieldsToActuals = buildFieldsToActuals closureCtor.Body argsToActuals
              let (qvars, varsToVars) = makeArgsQuantifierBound (lambda.Parameters.Tail)
              let quant = Quant(ec, { Kind = kind; Variables = qvars; Triggers = triggers; Condition = None; Body = expr'; Weight = "" })              
              Some(quant.SelfMap(replaceClosureFieldsAndBoundVariables fieldsToActuals varsToVars))
            | false, _ ->
              helper.Oops(ec.Token, "no lambda expression found for quantifier")
              None             
        | _ -> None

      let isNotLambdaClosure = function
        | Top.FunctionDecl({Parent = Some parent}) when closures.Contains(parent) -> false
        | Top.TypeDecl(td) when closures.Contains(td) -> false
        | _ -> true

      decls |> deepVisitExpressions findClosures 
      decls |> List.iter findLambdas
      decls |> List.filter isNotLambdaClosure |> deepMapExpressions rewriteQuantifiers' 

    // ============================================================================================================    

    let rewriteSpecialFunctions self = 

      let selfs = List.map self

      function
        | Call(ec, {FriendlyName = "VCC::Assert"}, [], [arg]) -> 
          Some(Assert(ec, self arg, []))
        | Call(ec, {FriendlyName = "VCC::Assume"}, [], [arg]) -> 
          Some(Assume(ec, self arg))
        | Call(ec, {FriendlyName = StartsWith "VCC::Result"}, [], []) ->
          Some(Result(ec))
        | Call(ec, {FriendlyName = StartsWith "VCC::Old"}, [], [arg]) ->
          Some(Old(ec, Macro({ec with Type = Type.MathState}, "prestate", []), self arg))
        | Call(ec, {FriendlyName = SpecialCallTo(tgt)}, [], args)  ->
          Some(Macro(ec, tgt, List.map self args))
        | _ -> None

    // ============================================================================================================    

    let normalizeBlocks self = 

      // remove empty blocks and blocks with just a single statement

      let rec loop acc = function
        | [] -> List.rev acc
        | stmt :: stmts ->
          match self stmt with
            | Block(_, [], _) -> loop acc stmts
            | block' -> loop (block' :: acc) stmts

      function
      | Block(ec, [Block(_, _, _) as block], None) -> Some(self block)
      | Block(ec, stmts, bc) -> Some(Block(ec, loop [] stmts, bc))
      | _ -> None
    
    // ============================================================================================================    

    let rewriteFieldAttributes decls =

      // pick up special annotation fields like VCCBackingMember and turn them into attributes on the following
      // fields

      let rewriteFieldAttributes' tok fields = 
        let rec loop attrs acc = function
          | [] ->
            if not (List.isEmpty attrs) then helper.Oops(tok, "extra attributes at end of type")
            List.rev acc
          | {Name = StartsWith "VCCBackingMember" } : Field :: fields ->
            loop (VccAttr(AttrBackingMember, "true")::attrs) acc fields
          | fld :: fields ->
            fld.CustomAttr <- attrs @ fld.CustomAttr
            loop [] (fld::acc) fields
        loop [] [] fields

      for d in decls do
        match d with 
          | Top.TypeDecl(td) ->
            td.Fields <- rewriteFieldAttributes' td.Token td.Fields
          | _ -> ()

      decls

    // ============================================================================================================    

    let rewriteGhost decls = 

      // rewrite the various forms of how _(ghost ...) annotations are rewritte back into their intended form

      // spec fields in classes/structures are marked by an initial static field named VCCBeginGhost and
      // and a closing VCCEndGhost; remove the marker fields and make everything between such fields
      // 'spec'

      let collectGhostFields = function
        | Top.TypeDecl(td) -> 
          let rec loop inSpec acc = function
            | ([] : Field list) -> 
              if inSpec then helper.Oops(td.Token, "unbalanced VCCBeginGhost and VCCEndGhost in type declaration")
              List.rev acc
            | { Name = StartsWith "VCCBeginGhost" } :: fields ->
              if inSpec then helper.Oops(td.Token, "nested VCCBeginGhost and VCCEndGhost in type declaration")
              loop true acc fields
            | { Name = StartsWith "VCCEndGhost" } :: fields ->
              if not inSpec then helper.Oops(td.Token, "unbalanced VCCBeginGhost and VCCEndGhost in type declaration")
              loop true acc fields
            | f :: fields ->
              if inSpec && not f.IsStatic then helper.Oops(f.Token, "spec field should have been marked 'static'")
              if inSpec then f.Flags <- (f.Flags ||| Flags.Spec) &&& ~~~ Flags.Static
              loop inSpec (f::acc) fields
          td.Fields <- loop false [] td.Fields
          Top.TypeDecl(td)
        | d -> d
        
      // spec blocks in code are marked by an initial local variable decl for a variable named VCCBeginGhost and
      // ended by a similar declaration of a local with name VCCEndGhost; take everything between two such
      // declarations and make them 'spec'

      let localsMap = new Dict<_,_>()

      let mkSpec = function
        | VarDecl(ec, ({Kind = Local} as v), attr) ->
          let v' = { v.UniqueCopy() with Kind = SpecLocal }
          localsMap.Add(v, v')
          VarDecl(ec, v', attr)
        | expr -> Expr.SpecCode expr

      let collectGhostStmts self = 
        let rec loop acc = function
          | [] -> List.rev acc, []
          | VarDecl(_, {Name = Contains "::VCCBeginGhost"}, []) :: stmts ->
            let specStmts, rest = loop [] stmts
            let specStmts' = List.map mkSpec specStmts
            loop (specStmts' @ acc) rest
          | VarDecl(_, {Name = Contains "::VCCEndGhost"}, []) :: stmts ->
            acc, stmts
          | stmt :: stmts ->
            loop ((self stmt) :: acc) stmts
      
        function
          | Block(ec, stmts, bc) -> 
            match loop [] stmts with
              | stmts', [] -> Some(Block(ec, stmts', bc))
              | _, _ -> helper.Oops(ec.Token, "unbalanced VCCBeginGhost and VCCEndGhost"); None
          | _ -> None

      let substVars self = function
        | Expr.Ref(ec, v) ->
          match localsMap.TryGetValue(v) with
            | true, v' -> Some(Ref(ec, v'))
            | false, _ -> None
        | _ -> None

      decls 
      |> List.map collectGhostFields
      |> deepMapExpressions collectGhostStmts 
      |> deepMapExpressions substVars

    // ============================================================================================================    

    let rewriteBlockDecorators self = 

      // associate a call to a construct that applies to the following block (like VCC::Unwrapping or VCC::Atomic) 
      // with the following stmt (block) and convert it into the format expected by later processing

      let rec rewriteBlockDecorators' acc = function
        | [] -> List.rev acc
        | Call(ec, {FriendlyName = StartsWith "VCC::Unwrapping"}, [], args) :: stmts ->
          match rewriteBlockDecorators' [] stmts with
            | [] -> 
              helper.Warning(ec.Token, 9127, "_(unwrapping ...) without following statements is ignored")
              List.rev acc
            | stmt :: stmts -> 
              List.rev acc @ [Macro(ec, "unwrapping", stmt :: args)] @ stmts
        | Call(ec, {FriendlyName = StartsWith "VCC::Atomic"}, [], args) :: stmts ->
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

    let sanitizeNames decls =
      for d in decls do
        match d with 
          | Top.FunctionDecl(fn) -> fn.Name <- sanitizeFullName fn.Name
          | _ -> ()
      decls

    // ============================================================================================================    

    let removeSpecialDecls decls =

      let filterSpecialDecls = function
        | Top.FunctionDecl({FriendlyName = StartsWith "VCC::"}) -> false
        | Top.Global({Name = StartsWith "VCC::"}, _) -> false
        | _ -> true

      List.filter filterSpecialDecls decls

    // ============================================================================================================    

    helper.AddTransformer ("cpp-begin", TransHelper.DoNothing)

    helper.AddTransformer ("cpp-errors", TransHelper.Expr reportErrors)
    helper.AddTransformer ("cpp-special-args", TransHelper.Decl specialArgumentHandling)
    helper.AddTransformer ("cpp-blocks", TransHelper.Expr normalizeBlocks)
    helper.AddTransformer ("cpp-contracts", TransHelper.Decl collectContracts)
    helper.AddTransformer ("cpp-field-attributes", TransHelper.Decl rewriteFieldAttributes)
    helper.AddTransformer ("cpp-ghost", TransHelper.Decl rewriteGhost)
    helper.AddTransformer ("cpp-assign-ops", TransHelper.Expr rewriteAssignOps)
    helper.AddTransformer ("cpp-rewrite-casts", TransHelper.Expr rewriteCasts)
    helper.AddTransformer ("cpp-quantifiers", TransHelper.Decl rewriteQuantifiers)
    helper.AddTransformer ("cpp-stack-arrays", TransHelper.Decl handleStackArrays)
    helper.AddTransformer ("cpp-rewrite-functions", TransHelper.Expr rewriteSpecialFunctions)
    helper.AddTransformer ("cpp-rewrite-block-decorators", TransHelper.Expr rewriteBlockDecorators)
    helper.AddTransformer ("cpp-rewrite-functions-with-contracts", TransHelper.Decl rewriteSpecialFunctionsWithContracts)
    helper.AddTransformer ("cpp-rewrite-macros", TransHelper.Expr rewriteExtraMacros)
    helper.AddTransformer ("cpp-bool-conversion", TransHelper.Expr insertBoolConversion)
    helper.AddTransformer ("cpp-rewrite-ops", TransHelper.Expr rewriteExtraOps)
    helper.AddTransformer ("cpp-sanitize-names", TransHelper.Decl sanitizeNames)
    helper.AddTransformer ("cpp-remove-decls", TransHelper.Decl removeSpecialDecls)

    helper.AddTransformer ("cpp-end", TransHelper.DoNothing)
