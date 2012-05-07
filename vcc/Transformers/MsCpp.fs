
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

  let functionsReturningsClassValues = Set.ofList [
                                                    "VCC::Arrayrange"
                                                    "VCC::Domain"
                                                    "VCC::Extent"
                                                    "VCC::Inter"
                                                    "VCC::Now"
                                                    "VCC::Owns"
                                                    "VCC::Span"
                                                    "VCC::Union"
                                                    "VCC::Universe"
                                                  ]

  let functionsWithVarargs = Set.ofList [
                                          "VCC::Atomic"
                                          "VCC::AtomicOp"
                                          "VCC::AtomicRead"
                                          "VCC::Mine"
                                          "VCC::Reads"
                                          "VCC::Trigger"
                                          "VCC::Unwrapping"
                                          "VCC::Writes"
                                        ]

  let specialFunctionMap = Map.ofList [
                                                    "VCC::Activeclaim",         "_vcc_valid_claim"
                                                    "VCC::Addr",                "_vcc_addr"
                                                    "VCC::Addreq",              "_vcc_addr_eq"
                                                    "VCC::Approves",            "_vcc_approves"
                                                    "VCC::Array",               "_vcc_as_array"
                                                    "VCC::Arraymembers",        "_vcc_array_members"
                                                    "VCC::Arrayrange",          "_vcc_array_range"
                                                    "VCC::AtomicObject",        "_vcc_is_atomic_obj"
                                                    "VCC::Claimable",           "_vcc_is_claimable"
                                                    "VCC::Claimcount",          "_vcc_ref_cnt"
                                                    "VCC::Claims",              "_vcc_claims"
                                                    "VCC::Closed",              "_vcc_closed"
                                                    "VCC::Depends",             "_vcc_depends"
                                                    "VCC::Destroyclaim",        "_vcc_unclaim"
                                                    "VCC::Diff",                "_vcc_set_difference"
                                                    "VCC::Disjoint",            "_vcc_set_disjoint"
                                                    "VCC::Extent",              "_vcc_extent"
                                                    "VCC::Extentfresh",         "_vcc_extent_is_fresh"
                                                    "VCC::Extentmutable",       "_vcc_extent_mutable"
                                                    "VCC::Fresh",               "_vcc_is_fresh"
                                                    "VCC::In",                  "_vcc_set_in"
                                                    "VCC::In0",                 "_vcc_set_in0"
                                                    "VCC::Inrangephysptr",      "_vcc_in_range_phys_ptr"
                                                    "VCC::Inter",               "_vcc_set_intersection"
                                                    "VCC::Isghost",             "_vcc_is_ghost_ptr"
                                                    "VCC::Mallocroot",          "_vcc_is_malloc_root"
                                                    "VCC::Makeclaim",           "_vcc_claim"
                                                    "VCC::Mine",                "_vcc_keeps"
                                                    "VCC::Mutable",             "_vcc_mutable"
                                                    "VCC::Mutablearray",        "_vcc_is_mutable_array"
                                                    "VCC::Notshared",           "_vcc_not_shared"
                                                    "VCC::Now",                 "_vcc_current_state"
                                                    "VCC::Objectroot",          "_vcc_is_object_root"
                                                    "VCC::Owner",               "_vcc_owner"
                                                    "VCC::Owns",                "_vcc_owns"
                                                    "VCC::Programentrypoint",   "_vcc_program_entry_point"
                                                    "VCC::ReadsHavoc",          "_vcc_reads_havoc"
                                                    "VCC::Span",                "_vcc_span"
                                                    "VCC::Starthere",           "_vcc_start_here"
                                                    "VCC::Threadlocal",         "_vcc_thread_local2"
                                                    "VCC::Threadlocalarray",    "_vcc_is_thread_local_array"
                                                    "VCC::Union",               "_vcc_set_union"
                                                    "VCC::Universe",            "_vcc_set_universe"
                                                    "VCC::Upgradeclaim",        "_vcc_upgrade_claim"
                                                    "VCC::Valid",               "_vcc_typed2"
                                                    "VCC::Wrapped",             "_vcc_wrapped"
                                                    "VCC::Writable",            "_vcc_writable"
                                      ]

  let specialFunctionWithContractMap = Map.ofList [
                                                    "VCC::Alloc",               "_vcc_spec_alloc"
                                                    "VCC::Allocarray",          "_vcc_spec_alloc_array"
                                                    "VCC::Free",                "_vcc_free"
                                                    "VCC::GiveupClosedOwner",   "_vcc_giveup_closed_owner"
                                                    "VCC::Matchlong",           "_vcc_match_long"
                                                    "VCC::Matchulong",          "_vcc_match_ulong"
                                                    "VCC::SetClosedOwner",      "_vcc_set_closed_owner"
                                                    "VCC::SetOwns",             "_vcc_set_owns"
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


  let specialTypesMap = Map.ofList  [
                                      "VCC::ClaimT",    Type.Claim
                                      "VCC::Integer",   Type.MathInteger MathIntKind.Signed
                                      "VCC::Natural",   Type.MathInteger MathIntKind.Unsigned
                                      "VCC::Object",    Type.ObjectT
                                      "VCC::State",     Type.MathState
                                      "VCC::ThreadT",   Type.ThreadIdT
                                    ] 

  let specialTypeNames = specialTypesMap |> Map.toSeq |> Seq.map fst |> Set.ofSeq 

  let specialTypeCtors = Set.ofList [
                                      "VCC::Claim::Claim"
                                      "VCC::Integer::Integer"
                                      "VCC::Natural::Natural"
                                      "VCC::Object::Object"
                                      "VCC::Set::Set"
                                      "VCC::State::State"
                                    ]

  let customAttrs = Map.ofList  [
                                  "VCC::Pure",          CAST.AttrIsPure
                                  "VCC::FrameAxiom",    CAST.AttrFrameaxiom
                                  "VCC::NoReadsCheck",  CAST.AttrNoReadsCheck
                                  "VCC::AtomicInline",  CAST.AttrAtomicInline
                                ]

  let (|IsCustomAttr|_|) s = Map.tryFind s customAttrs



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

  let (|EndsWith|_|) postfix (s:string) = if s.EndsWith(postfix, System.StringComparison.Ordinal) then Some () else None

  let nongeneric (name:string) = 
    let endpos = name.IndexOf('<')
    if endpos = -1 then name else name.Substring(0, endpos)

  let basename (name:string) =
    let startPos = name.LastIndexOf("::")
    if startPos = -1 then name else name.Substring(startPos + 2)

  let (|NonGenericIs|_|) name0 name1 = if nongeneric name1 = name0 then Some() else None

  let (|SpecialCallTo|_|) name = Map.tryFind (nongeneric name) specialFunctionMap

  let (|IsSpecialType|_|) name = Map.tryFind name specialTypesMap
  
  let (|AddrOf|_|) = function
    | Macro(ec, "&", [arg]) -> Some(ec, arg)
    | _ -> None
          
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
        | IntLiteral(ec, i) when ec.Type._IsPtr && i.IsZero -> Some(Macro(ec, "null", []))
        | Macro(ic, "implicit_cast", [Cast(ec, cs, e)]) when ec.Type._IsInteger ->
          let e' = self e
          if icil ec.Type e' then 
            Some(Cast(ec, cs, e')) 
          else 
            Some(Macro(ic, "implicit_cast", [Cast(ec, cs, e')]))

        | _ -> None

    // ============================================================================================================    

    let rewriteMaps self =

      // rewrite map accesses, assignments, and initialization

      let (|MapGet|_|) = function
        | Deref(ec, Call(_, { FriendlyName = n }, [], [Macro(_, "&", [m]); idx])) 
            when n.StartsWith("VCC::Map") && n.EndsWith("operator[]") ->          
          Some(ec, m, idx)
        | _ -> None

      function 
        | MapGet(ec, m, idx) -> Some(Macro(ec, "map_get", [self m; self idx]))
        | Deref(ec, Call(_, { FriendlyName = fname }, [], [Macro(_, "&", [lhs]); Macro(_, "&", [rhs])])) 
            when fname.StartsWith("VCC::Map") && fname.EndsWith("operator=") ->
          Some(Macro(ec, "=", [self lhs; self rhs]))
        | _ -> None

    // ============================================================================================================        

    let rewriteSets self = 

      // rewrite set creation and equality

      let (|CreateSet|_|) = function
        | Deref(_, Macro(_, "comma", [Deref(_, Call(ec, {FriendlyName = "VCC::Set::Set"}, [], (AddrOf(_, Ref(_, v))) :: elems )); AddrOf(_, Ref(_, v'))])) when v.Name = v'.Name ->
          Some(ec, elems)
        | Deref(_, Call(ec, {FriendlyName = "VCC::Set::Set"}, [], _ :: elems)) -> Some(ec, elems)
        | _ -> None

      function
        | CreateSet(ec, elems)
        | AddrOf(_, CreateSet(ec, elems)) ->
          Some(Macro(ec, "set", List.map self elems))
        | Call(ec, { FriendlyName = "VCC::Set::operator==" }, [], [arg0; arg1]) ->
          Some(Macro(ec, "_vcc_set_eq", [self arg0; self arg1]))
        | _ -> None

    // ============================================================================================================    

    let rewriteAssignOps self = 

      // rewrite '+=' and '++' like operations
      
      // TODO: handle situations where the location incremented involves a func call, which should not be duplicated
      // this is also wrong in CCI at the moment 
    
      let handlePrePostIncrDecr (e:Expr) op isPost t =
        let (init, tmp) = 
          if isPost then
            let tmp = getTmp helper "incdec" e.Type VarKind.Local
            [VarDecl(voidBogusEC(), tmp, []); Macro(voidBogusEC(), "=", [mkRef tmp; e])], [mkRef tmp]
          else 
            [], [e]
        
        let calc = Expr.Prim(e.Common, Op(op, CheckedStatus.Checked), tmp @ [IntLiteral({e.Common with Type = t}, one)])
        let assign = Macro(e.Common, "=", [e; calc])
        Expr.MkBlock(init @ [assign] @ tmp) 

      let handleAssignOp ec op (e0:Expr) e1 =
        let calc = Expr.Prim(e0.Common, op, [e0; e1])
        Macro(ec, "=", [e0; calc])        

      function

        | Macro(_, incrOp, [e; arg]) when incrOpTable.ContainsKey incrOp -> 
          let (op, isPost) = Map.find incrOp incrOpTable
          Some(handlePrePostIncrDecr (self e) op isPost arg.Type)
        
        | Macro(ec, assignOp, [e0; e1]) when assignOpTable.ContainsKey assignOp ->
          let (op, isChecked) = Map.find assignOp assignOpTable
          Some(handleAssignOp ec (Op(op, if isChecked then Checked else Unchecked)) (self e0) (self e1))

        | _ -> None

    // ============================================================================================================    

    let rewritePtrArithmetic self = function
      | Prim(ec, Op((("+"|"-") as op), cs), [ptr; e1]) when ptr.Type.IsPtr && e1.Type._IsInteger ->       
        let rhs = Prim(e1.Common, Op("*", Checked), [self e1; IntLiteral(e1.Common, new bigint(ptr.Type.Deref.SizeOf))])
        let rhs' = if op = "-" then Expr.Prim (rhs.Common, Op("-", Checked), [rhs]) else rhs
        Some(Macro(ec, "ptr_addition", [self ptr; rhs']))
      | _ -> None
        
    // ============================================================================================================    

    let removeObjectCopyOperations self = function

      // For some VCC-builtin functions, the C++ compiler will introduce extra object copy operations
      // which we do not need and get rid of here

      // also, when passing structs, extra temporary variables are introduced, which we als remove here

      | AddrOf(_, Deref(ec, Call(_, fn, [], args)))
      | Deref(ec, Call(_, fn, [], args))
      | Macro(_, "implicit_cast", [Cast(_, _, Deref(ec, Call(_, fn, [], args)))]) 
        when Set.contains (nongeneric fn.FriendlyName) functionsReturningsClassValues ->
        Some(Call(ec, fn, [], List.map self args))

      | Macro(_, "comma", [Macro(_, "=", [Ref(_, v); expr]); Ref(_, v')]) when v.Name = v'.Name ->
        // TODO: compare variable directly and not by name once the AST convertor re-uses variables for ALLOTEMPS
        Some(self expr)

      // remove the structure that is created for conversion and copy constructors for built-in verification types

      | AddrOf(ec, Deref(_, Call(_, fn, [], [Macro(_, "currentobject", []); expr]))) 
          when fn.IsCtor && (Set.contains (fn.FriendlyName) specialTypeCtors || nongeneric fn.FriendlyName = "VCC::Map") ->
        match expr with
          | AddrOf(ecExpr, expr') when ec.Type = ecExpr.Type -> Some(self expr')
          | _ -> Some(self expr)

      | _ -> None

    // ============================================================================================================    

    let addDeclarationsForParameters decls = 

      // later processing requires that parameters are also declared at the beginning of a methods body
      // add these declarations

      for d in decls do
        match d with
          | Top.FunctionDecl({Body = Some (Block(ec, stmts, bc))} as fn) ->
            let toDecl (v:Variable) = VarDecl(voidBogusEC(), v, [])
            fn.Body <- Some(Block(ec, (List.map toDecl fn.Parameters) @ stmts, bc))
          | _ -> ()
      
      decls

    // ============================================================================================================    

    let retypeMacros self = 

      let macrosWithSpecialTypes = Map.ofList [
                                                "_vcc_as_array", Type.ObjectT
                                              ]
      function
        | Macro(ec, m, args) ->
          match Map.tryFind m macrosWithSpecialTypes with
            | None -> None
            | Some t -> Some(Macro({ec with Type = t}, m, List.map self args))
        | _ -> None
          
    // ============================================================================================================    

    let rewriteExtraMacros self = 

      function

        | Macro(ec, "init", [lhs; Macro(_, "array_init", Skip(_) :: args)]) ->         
          match lhs.Type with
            | Type.Array(_,_) -> 
              let assignIdx idx (arg:Expr) = 
                Expr.Macro({ec with Type = Type.Void}, "=", [Expr.Deref({lhs.Common with Type = arg.Type}, Index(lhs.Common, lhs, mkInt idx)); arg])
              Some(Expr.MkBlock(List.mapi assignIdx (List.map self args)))
            | Type.Ref(td) ->
              Some(Expr.MkBlock(List.map self args))
            | _ -> 
              helper.Oops(lhs.Token, "unexpected type '" + lhs.Type.ToString() + "' for initialized object")
              None

        | Macro(ec, "init", [_; Skip(_)]) -> Some(Skip(ec))

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
                helper.Error(ec.Token, 37, "Cannot implicitly convert expression '" + expr.Token.Value + "' of type '" + expr.Type.ToString() + "' to '" + ec.Type.ToString() + "'")
            | _ -> ()
          Some(self cast)

        | Macro(ec, "comma", args) -> Some(Macro(ec, "fake_block", List.map self args))

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
      // turn "do" (while) into "doUntil"

      let toBool (expr:Expr) = 
        match expr.Type with
          | Type.Bool -> self expr
          | _ -> Cast({expr.Common with Type = Type.Bool}, CheckedStatus.Unchecked, self expr)

      function 
        | Prim(ec, (Op(("!"|"||"|"&&"), _) as op), args) ->
          Some(Prim(ec, op, args |> List.map toBool))
        | Macro(ec, "=", [lhs; rhs]) when lhs.Type = Type.Bool -> 
          Some(Macro(ec, "=", [self lhs; toBool rhs]))
        | Macro(ec, "for", [contr; init; cond; incr; body]) ->
          Some(Macro(ec, "for", [self contr; self init; toBool cond; self incr; self body]))
        | Macro(ec, "while", [contr; cond; body]) ->
          Some(Macro(ec, "while", [self contr; toBool cond; self body]))
        | Macro(ec, "do", [contr; body; cond]) ->
          Some(Macro(ec, "doUntil", [self contr; self body; Prim({cond.Common with Type = Type.Bool}, Op("!", Processed), [toBool cond])]))
        | If(ec, tc, cond, _then, _else) ->
          Some(If(ec, Option.map self tc, toBool cond, self _then, self _else))
        | Macro(ec, "ite", [cond; _then; _else]) ->
          Some(Macro(ec, "ite", [toBool cond; self _then; self _else]))
        | _ -> None
          
    // ============================================================================================================    

    let handleStackArrays decls = 
    
      let varSubst = new Dict<_,_>()

      let replaceDeclsAndAllocate self = function
        | VarDecl(ec, ({Type = Array(t, n)} as v), attr) as decl ->
          let ptrType = Type.PhysPtr(t) // TODO: could be spec ptr
          let vptr = Variable.CreateUnique v.Name ptrType v.Kind
          let vptrRef = Expr.Ref({ec with Type=v.Type}, vptr)
          let stackAlloc = Expr.Macro({ ec with Type = ptrType }, "stack_allocate_array", [mkInt n; Expr.False])
          let assign = Expr.Macro(ec, "=", [vptrRef; stackAlloc])
          varSubst.Add(v, mkRef vptr)
          Some(Expr.Macro(ec, "fake_block", [VarDecl(ec, vptr, attr); assign]))
        | _ -> None

      decls 
      |> deepMapExpressions replaceDeclsAndAllocate
      |> mapExpressions (fun _ (e:Expr) -> e.Subst(varSubst))

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

    let collectTypeContracts decls =

      // move attribute markers (like VCCDynamicOwns) and invariants, both of which are 
      // represented as member functions to their types
      // filter out those entities as they are moved

      let rewriteThis td self = function
        | Call(ec, ({ FriendlyName = "VCC::Mine" } as fn), [], args) -> 
          Some(Call(ec, fn, [], This({ec with Type = Type.MkPtrToStruct(td)}) :: (List.map self args)))
        | Ref(ec, { Name = "this" }) -> 
          Some(This(ec))
        | _ -> None

      let collectTypeContracts' = function
        | Top.FunctionDecl({FriendlyName = EndsWith("::VCCDynamicOwns"); Parent = Some(td)}) ->
          td.CustomAttr <- VccAttr(CAST.AttrDynamicOwns, "") :: td.CustomAttr
          false
        | Top.FunctionDecl({FriendlyName = EndsWith("::VCCVolatileOwns"); Parent = Some(td)}) ->
          td.CustomAttr <- VccAttr(CAST.AttrVolatileOwns, "") :: td.CustomAttr
          false
        | Top.FunctionDecl({FriendlyName = EndsWith("::VCCClaimable"); Parent = Some(td)}) ->
          td.CustomAttr <- VccAttr(CAST.AttrClaimable, "") :: td.CustomAttr
          false
        | Top.FunctionDecl({FriendlyName = EndsWith("::VCCPrimitive"); Parent = Some(td)}) ->
          td.CustomAttr <- VccAttr(CAST.AttrPrimitive, "") :: td.CustomAttr
          false
        | Top.FunctionDecl({FriendlyName = Contains("::VCCInvariant"); Parent = Some(td); Body = body}) as fn ->
          match body with
            | Some(Block(_, [Return(_, Some(inv))], None)) ->
              td.Invariants <- inv.SelfMap(rewriteThis td) :: td.Invariants
            | Some(expr) ->
              helper.Oops(expr.Token, "unexpected invariant function structure")
            | _ -> helper.Oops(fn.Token, "invariant function without body")
          false
        | _ -> true

      let reverseInvariants = function
        | Top.TypeDecl(td) as d -> td.Invariants <- List.rev td.Invariants; d
        | d -> d

      decls 
      |> List.filter collectTypeContracts' 
      |> List.map reverseInvariants

    // ============================================================================================================    

    let collectContracts decls =

      // 1) move contracts from list of statements to the surrounding block or function
      // 2) and move contracts from contract functions to their target functions

      // Step 1

      let findContracts stmts =

        let findContracts' (bc:BlockContract, invs) = function
          | Call(ec, {FriendlyName = StartsWith "VCC::Requires"}, [], [arg]) ->  { bc with Requires = arg :: bc.Requires }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Ensures"}, [], [arg]) ->   { bc with Ensures = arg :: bc.Ensures }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Returns"}, [], [arg]) as returns ->   { bc with Ensures = returns :: bc.Ensures}, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Reads"}, [], args) ->      { bc with Reads = args @ bc.Reads }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Writes"}, [], args) ->     { bc with Writes = args @ bc.Writes }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Decreases"}, [], args) ->  { bc with Decreases = args @ bc.Decreases }, invs
          | Call(ec, {FriendlyName = StartsWith "VCC::Invariant"}, [], [arg]) -> bc, (arg :: invs)
          | Call(ec, {FriendlyName = IsCustomAttr(attr) }, [], []) ->            { bc with CustomAttr = VccAttr(attr, "") :: bc.CustomAttr}, invs
          | _ -> bc, invs

        let (found, invs) = List.fold findContracts' (BlockContract.Empty, []) stmts
        { Requires = List.rev found.Requires
          Ensures = List.rev found.Ensures
          Reads = List.rev found.Reads
          Writes = List.rev found.Writes
          Decreases = List.rev found.Decreases
          CustomAttr = List.rev found.CustomAttr}, List.rev invs

      let removeContracts = 
        let isNoContract = function
          | Call(ec, {FriendlyName = StartsWith "VCC::Requires"}, [], [_])         
          | Call(ec, {FriendlyName = StartsWith "VCC::Ensures"}, [], [_])          
          | Call(ec, {FriendlyName = StartsWith "VCC::Returns"}, [], [_])
          | Call(ec, {FriendlyName = StartsWith "VCC::Reads"}, [], _) 
          | Call(ec, {FriendlyName = StartsWith "VCC::Writes"}, [], _)
          | Call(ec, {FriendlyName = StartsWith "VCC::Decreases"}, [], _) 
          | Call(ec, {FriendlyName = StartsWith "VCC::Invariant"}, [], [_])
          | Call(ec, {FriendlyName = IsCustomAttr(_) }, [], [])
            ->  false
          | _ -> true
        List.filter isNoContract

      let toLoopContract (bc : BlockContract, invs) =
        let mkLoopContract kind (expr:Expr) = 
          Expr.MkAssert (Macro ({ expr.Common with Type = Type.Bool }, kind, [expr]))
        
        List.map Expr.MkAssert invs @
        List.map (mkLoopContract "loop_variant") bc.Decreases @ 
        List.map (mkLoopContract "loop_writes") bc.Writes

      let pullOutContracts self = function
        | Macro(ec, "for", [ Macro(lec, "loop_contract", []); init; cond; incr; Block(bec, stmts, None) ]) ->
          let (bc, invs) = findContracts stmts
          let body' = Block(bec, stmts |> removeContracts |> List.map self, None)
          let loopContracts = toLoopContract (bc, invs)
          Some(Macro(ec, "for", [ Macro(lec, "loop_contract", loopContracts); init; cond; incr; body']))
        | Macro(ec, "do", [ Macro(lec, "loop_contract", []); Block(bec, stmts, None); cond]) ->
          let (bc, invs) = findContracts stmts
          let body' = Block(bec, stmts |> removeContracts |> List.map self, None)
          let loopContracts = toLoopContract (bc, invs)
          Some(Macro(ec, "do", [Macro(lec, "loop_contract", loopContracts); body'; cond]))
        | Macro(ec, "while", [ Macro(lec, "loop_contract", []); cond; Block(bec, stmts, None)]) ->
          let (bc, invs) = findContracts stmts
          let body' = Block(bec, stmts |> removeContracts |> List.map self, None)
          let loopContracts = toLoopContract (bc, invs)
          Some(Macro(ec, "while", [Macro(lec, "loop_contract", loopContracts); cond; body']))
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
                fn.CustomAttr <- bc.CustomAttr @ fn.CustomAttr
                fn.Body <- Some(Block(ec, stmts, None))
              | _ -> fn.Body <- Some body'
          | _ -> ()

      // Step 2)

      let (|IsOobContractFor|_|) = function
        | Block(_, Call(ec, { FriendlyName = StartsWith "VCC::ContractFor" }, [], fptr) :: _, _) -> 
          match fptr with
            | [Cast(_, _, Call(_, fn, _, _))] -> Some(fn)
            | _ -> 
              helper.Oops(ec.Token, "unexpected function reference structure in VCC::ContractFor")
              None
        | _ -> None

      let oobContracts = new Dict<Function,Function>()

      for d in decls do
        match d with
          | Top.FunctionDecl({Body = Some (IsOobContractFor fn)} as fnContr) -> 
            match oobContracts.TryGetValue(fn) with
              | true, fn' -> helper.Error(fnContr.Token, 9802, "multiple out-of-band contracts for '" + fn.FriendlyName + "'", Some(fn'.Token))
              | false, _ -> oobContracts.Add(fn, fnContr)
          | _ -> ()

      for d in decls do
        match d with
          | Top.FunctionDecl(fn) -> 
            match oobContracts.TryGetValue(fn) with
              | true, fnContr ->
                let notEmpty l = not (List.isEmpty l)
                if notEmpty fn.CustomAttr || notEmpty fn.Reads || notEmpty fn.Writes || notEmpty fn.Requires || notEmpty fn.Ensures || notEmpty fn.Variants then
                  helper.Error(fn.Token, 9803, "function '" + fn.FriendlyName + "' cannot have both contracts and out-of-band contracts", Some(fnContr.Token))
                else
                  // substitute the parameters from the contract with their counterpart from the function's declaration
                  let parameterSubst = new Dict<_,_>()
                  List.iter2 (fun p0 p1 -> parameterSubst.Add(p0, mkRef p1)) fnContr.Parameters fn.Parameters
                  let subst = List.map (fun (expr:Expr) -> expr.Subst parameterSubst)

                  fn.Reads <- fnContr.Reads |> subst
                  fn.Writes <- fnContr.Writes |> subst
                  fn.Requires <- fnContr.Requires |> subst
                  fn.Ensures <- fnContr.Ensures |> subst
                  fn.Variants <- fnContr.Variants |> subst
                  fn.CustomAttr <- fnContr.CustomAttr
              | false, _ -> ()
          | _ -> ()

      // the contract functions will later be filtered out by name, no need to filter here

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
                fn.Flags <- fn.Flags ||| Flags.Spec
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
        | Deref(ec, Call(_, {FriendlyName = (StartsWith "VCC::Lambda" as friendlyName)}, [], 
                   [Deref(_, Call(_, closureCtor, [], _this :: captures))]))
        | Call(ec, {FriendlyName = (StartsWith "VCC::Exists" | StartsWith "VCC::ForAll" | StartsWith "VCC::Lambda" as friendlyName)}, [], 
                   [Deref(_, Call(_, closureCtor, [], _this :: captures))]) -> 
          let kind = 
            match friendlyName with
            | StartsWith "VCC::Exists" -> QuantKind.Exists
            | StartsWith "VCC::ForAll" -> QuantKind.Forall
            | StartsWith "VCC::Lambda" -> QuantKind.Lambda
            | _ -> die()
          Some(ec, closureCtor, captures, kind)
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
          | AddrOf(_, actual) -> Map.add arg actual map
          | _ -> die()
        List.fold2 f Map.empty args actuals

      let buildFieldsToActuals ctorBody argsToActuals =
        // build a structure that maps the fields in the closure to the locals that they are meant to capture

        let f2a = new Dict<_,_>()
        let f = function
          | Macro(_, "=", [Deref(_, Dot(_, Ref(_, {Name = "this"}), f)); AddrOf(_, Deref(_, Ref(_, v)))]) ->
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
            | Return(_, Some expr) :: _ -> 
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

      let toUnchecked self = function
        | Cast(ec, Checked, arg) -> Some(Cast(ec, Unchecked, self arg))
        | Prim(ec, Op(op, Checked), args) -> Some(Prim(ec, Op(op, Unchecked), List.map self args))
        | _ -> None

      let (|StringLiteral|_|) = function
        | Macro(_, "string", [UserData(_, (:? string as str))])
        | Cast(_, _, Macro(_, "string", [UserData(_, (:? string as str))]))
        | Macro(_, "implicit_cast", [Cast(_, _, Macro(_, "string", [UserData(_, (:? string as str))]))]) -> Some str
        | _ -> None

      function
        | Call(ec, {FriendlyName = SpecialCallTo(tgt)}, [], args)  ->
          Some(Macro(ec, tgt, List.map self args))
        | Call(ec, {FriendlyName = "VCC::Assert"}, [], [arg]) -> 
          Some(Assert(ec, self arg, []))
        | Call(ec, {FriendlyName = "VCC::Assume"}, [], [arg]) -> 
          Some(Assume(ec, self arg))
        | Call(ec, {FriendlyName = StartsWith "VCC::Result"}, [], []) ->
          Some(Result(ec))
        | Call(ec, {FriendlyName = StartsWith "VCC::Returns"}, [], [expr]) ->
          Some(Expr.Prim({ec with Type = Type.Bool}, Op("==", CheckedStatus.Unchecked), [Result(ec); self expr]))
        | Call(ec, {FriendlyName = StartsWith "VCC::Old"}, [], [arg]) ->
          Some(Old(ec, Macro({ec with Type = Type.MathState}, "prestate", []), self arg))
        | Call(ec, {FriendlyName = NonGenericIs "VCC::At" }, [], [state; expr]) ->
          Some(Old(ec, self state, self expr))
        | Call(ec, {FriendlyName = StartsWith "VCC::Unchecked"}, [], [arg]) ->
          Some((self arg).SelfMap(toUnchecked))
        | Call(ec, { FriendlyName = StartsWith "VCC::Labeled"}, [], [StringLiteral(str); expr]) ->
          Some(Macro(ec, "lbl_" + str, [Expr.Bogus; self expr]))
        | Call(ec, ({ FriendlyName = StartsWith "VCC::Is"} as fn), [], [arg]) ->
          match fn.Body with 
            | Some (Block(_, VarDecl(_, v, _) :: _, _)) -> 
              Some(Macro(ec, "_vcc_is", [self arg; typeExpr v.Type.Deref]))
            | _ -> 
              helper.Oops(ec.Token, "Unexpected body structure of '" + fn.Name + "'")
              None
        | Ref(ec, {Name = "VCC::Me"}) -> Some(Macro(ec, "_vcc_me", []))
        | _ -> None

    // ============================================================================================================    


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
      | Return(ec, Some(Skip(_))) -> Some(Return(ec, None))
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
            loop (VccAttr(AttrBackingMember, "")::attrs) acc fields
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
              loop false acc fields
            | f :: fields ->
              if inSpec && not f.IsStatic then helper.Oops(f.Token, "spec field should have been marked 'static'")
              if inSpec then f.Flags <- (f.Flags ||| Flags.Spec) &&& ~~~ Flags.Static
              loop inSpec (f::acc) fields
          td.Fields <- loop false [] td.Fields
          Top.TypeDecl(td)
        | d -> d
        
      // references to spec fields in invariants without qualifying 'this' are represented as references to 
      // a global variable; here, we rewrite them into the proper field access

      let fixupReferencesInInvariants = function
        | Top.TypeDecl(td) -> 
          let fixupReference _ = function
            | Ref(ec, ({ Kind = VarKind.Global } as v)) ->
              let idx = v.Name.LastIndexOf("::")
              if idx > 0 && not (v.Name.StartsWith("VCC::")) then
                let fieldName = v.Name.Substring(idx + 2)
                match List.tryFind (fun (f : Field) -> f.Name = fieldName) td.Fields with
                  | Some fld ->
                    if (not fld.IsSpec) then 
                      helper.Oops(ec.Token, "unexpected reference to static field '" + v.Name + "' in invariant")
                    Some(Deref(ec, Dot({ec with Type = Type.MkPtr(fld.Type, true)}, This({ec with Type = Type.MkPtrToStruct(td)}), fld)))
                  | None ->
                    helper.Oops(ec.Token, "unexpected reference to unknown field '" + v.Name + "' in invariant")
                    None
              else None
            | _ -> None
          td.Invariants <- List.map (fun (e:Expr) -> e.SelfMap(fixupReference)) td.Invariants
          Top.TypeDecl(td)
        | d -> d

      // spec blocks in code are marked by an initial local variable decl for a variable named VCCBeginGhost and
      // ended by a similar declaration of a local with name VCCEndGhost; take everything between two such
      // declarations and make them 'spec'

      let specVarMap = new Dict<_,_>()

      let mkSpec = function
        | VarDecl(ec, ({Kind = Local} as v), attr) ->
          let v' = { v.UniqueCopy() with Kind = SpecLocal }
          specVarMap.Add(v, v')
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
          match specVarMap.TryGetValue(v) with
            | true, v' -> Some(Ref(ec, v'))
            | false, _ -> None
        | VarDecl(ec, v, attrs) ->
          match specVarMap.TryGetValue(v) with
            | true, v' -> Some(VarDecl(ec, v', attrs))
            | false, _ -> None
        | _ -> None

      // ghost and out arguments are preceded by a marker parameter of type VCC::Ghost and VCC::GhostOut respectively

      let (|GhostMarkerType|_|) = function
        | Type.Ref({Name = "VCC::Ghost"}) -> Some(VarKind.SpecParameter)
        | Type.Ref({Name = "VCC::GhostOut"}) -> Some(VarKind.OutParameter)
        | _ -> None
     
      let ghostToSpecPars = 
        let ghostToSpecPar (v:Variable) kind =
          match kind with
            | VarKind.Parameter -> v
            | _ ->
              let v' = { v.UniqueCopy() with Kind = kind }
              specVarMap.Add(v, v')
              v'

        let rec rewritePars acc kind = function
          | [] -> List.rev acc
          | (p:Variable) :: ps ->
            match p.Type with
              | GhostMarkerType(kind) -> rewritePars acc kind ps
              | _ -> rewritePars ((ghostToSpecPar p kind)::acc) kind ps

        function 
          | Top.FunctionDecl(fn) -> fn.Parameters <- rewritePars [] VarKind.Parameter fn.Parameters;
          | _ -> ()
        
      decls |> List.iter ghostToSpecPars

      // to make the calls to functions with ghost and out parameters work, we add dummy instances for the markers
      // we remove those here

      let removeHelperInstances self = function
        | Call(ec, fn, t, args) -> 
          let rec loop acc seenOut = function
            | [] -> List.rev acc
            | (Call(_, {FriendlyName = "VCC::GhostOut::Instance"}, [], []))::args -> loop acc true args
            | (Call(_, {FriendlyName = "VCC::Ghost::Instance"}, [], []))::args -> loop acc false args
            | arg :: args -> 
              let arg' = if seenOut then Macro(arg.Common, "out", [self arg]) else self arg
              loop (arg'::acc) seenOut args
          Some(Call(ec, fn, t, loop [] false args))

        | _ -> None

      decls 
      |> List.map collectGhostFields
      |> List.map fixupReferencesInInvariants
      |> deepMapExpressions collectGhostStmts 
      |> deepMapExpressions substVars
      |> deepMapExpressions removeHelperInstances

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

    let checkAstStructure decls =

      let checkLoopStructure self = function
        | Macro(ec, "for", [ Macro(lec, "loop_contract", loopContract); init; cond; incr; Block(bec, stmts, None) ]) -> true
        | Macro(ec, "for", _) -> helper.Oops(ec.Token, "unexpected for loop structure"); false
        | Macro(ec, "do", [ Macro(lec, "loop_contract", loopContract); Block(bec, stmts, None); cond]) -> true
        | Macro(ec, "do", _) -> helper.Oops(ec.Token, "unexpected do-while loop structure"); false
        | Macro(ec, "while", [ Macro(lec, "loop_contract", loopContract); cond; Block(bec, stmts, None)]) -> true
        | Macro(ec, "while", _) -> helper.Oops(ec.Token, "unexpected while loop structure"); false
        | _ -> true

      decls |> deepVisitExpressions checkLoopStructure

      decls

    // ============================================================================================================    

    let removeTemps decls =

      // replace expressions of the form  @=(tmp, rhs) by rhs when tmp is never referenced elsewhere

      let singleUseVars = new HashSet<_>()
      let mutipleUseVars = new HashSet<_>()
      
      let findTemps self = function
        | Ref(_, v) when v.Kind = VarKind.Local ->
          if not (mutipleUseVars.Contains(v.Name)) then
            if not (singleUseVars.Add(v.Name)) then
              singleUseVars.Remove(v.Name) |> ignore
              mutipleUseVars.Add(v.Name) |> ignore
          false
        | _ -> true

      let removeSingleUseTemps self = function
        | Macro(_, "=", [Ref(_, v); rhs]) when singleUseVars.Contains(v.Name) ->
          Some(self rhs)
        | _ -> None

      decls |> deepVisitExpressions findTemps
      decls |> deepMapExpressions removeSingleUseTemps

    // ============================================================================================================    

    let sanitizeNames decls =
      for d in decls do
        match d with 
          | Top.FunctionDecl(fn) -> fn.Name <- sanitizeFullName fn.Name
          | _ -> ()
      decls

    // ============================================================================================================    

    let rewriteSpecialTypes decls = 

      // turn types from the VCC:: namespace into their CAST counterparts

      let typeSubst self = function
        | Type.Ref({Name = IsSpecialType(t')}) ->
          Some(t')
        | Type.Ref({Name = "VCC::Set"})
        | Ptr(Type.Ref({Name = "VCC::Set"})) ->
          Some(Type.PtrSet)
        | Type.Ref(td) when td.Name.StartsWith("VCC::Map") ->
          match td.Fields with
            | [ { Name = "_from_member_"; Type = Ptr(fromType) }; { Name = "_to_member_"; Type = Ptr(toType) } ] -> Some(Type.Map(self fromType, self toType))
            | _ -> helper.Oops(td.Token, "unexpected map type structure"); None
        | _ -> None

      decls |> deepRetype typeSubst

    // ============================================================================================================    

    let makeAssignmentsIntoStmts self = 

      // assignments that occur on statement have their type changed to 'void'

      let handleAssignmentStmts = function
        | Macro(ec, "=", args) -> Macro({ec with Type = Type.Void}, "=", args)
        | Block(ec, stmts, bc) -> Block(ec, List.map self stmts, bc)
        | expr -> expr

      function
        | Block(ec, stmts, bc) -> Some(Block(ec, List.map handleAssignmentStmts stmts, bc))
        | _ -> None

    // ============================================================================================================    

    let rewriteGlobalsInitialization = 
      
      // rewrite Global(v, Some(skip)) to Global(v, None))

      let removeSkipInit = function
        | Top.Global(v, Some(Skip(_))) -> Top.Global(v, None)
        | t -> t

      List.map removeSkipInit

    // ============================================================================================================    

    let removeSpecialDecls decls =

      let filterSpecialDecls = function
        | Top.TypeDecl({Name = StartsWith "VCC::"}) -> false
        | Top.FunctionDecl({FriendlyName = StartsWith "VCC::"}) -> false
        | Top.FunctionDecl(fn) when fn.Token.Filename.EndsWith("stdlib.h") -> false
        | Top.Global({Name = StartsWith "VCC::"}, _) -> false
        | _ -> true

      List.filter filterSpecialDecls decls

    // ============================================================================================================    

    let despecializeAlloc decls = 

      let (|IsSpecAlloc|_|) = function
        | "_vcc_spec_alloc"
        | "_vcc_spec_alloc_array"
        | "_vcc_alloc" -> Some()
        | _ -> None

      let despecializeCall self = function
        | Call(ec, ({Name = IsSpecAlloc() } as alloc), [], args) ->
          match ec.Type with
            | Ptr(t) -> Some(Call(ec, alloc, [t], List.map self args))
            | _ -> helper.Oops(ec.Token, "unexpected return type in call to _vcc_alloc"); None
        | _ -> None

      let despecializeDecl = function
        | Top.FunctionDecl({Name = IsSpecAlloc()} as alloc) ->
          let tv = { Name = "T" } : TypeVariable
          alloc.RetType <- Type.SpecPtr(TypeVar(tv))
          alloc.TypeParameters <- [tv]
          alloc.Flags <- alloc.Flags ||| Flags.Spec
        | _ -> ()

      decls |> List.iter despecializeDecl
      decls |> deepMapExpressions despecializeCall

    // ============================================================================================================    

    let removeVarargCasts self = 

      // for functions with variable argument lists (...), the compiler introduces some implicit casts
      // which we get rid of here

      let stripInitialImplicitCast = 
          let sic = function
              | Macro(_, "implicit_cast", [Cast(_, _, expr)]) -> expr
              | expr -> expr
          List.map sic

      function
        | Call(ec, ({FriendlyName = name} as fn), [], args) when Set.contains name functionsWithVarargs ->
          Some(Call(ec, fn, [], args |> stripInitialImplicitCast |> List.map self))
        | _ -> None
      
    // ============================================================================================================    

    let removeInitOrAssignOfBuiltinTypes self = 

      let (|IsBuiltinTypeCtor|_|) = function
        | Call(_, fn, [], args) when fn.IsCtor && (Set.contains (fn.FriendlyName) specialTypeCtors || nongeneric fn.FriendlyName = "VCC::Map") -> Some args
        | _ -> None
        
      function
        | Macro(ec, "init", [lhs; AddrOf(_, Deref(_, IsBuiltinTypeCtor(args)))]) ->
          match args with
            | [] -> die()
            | [_] -> Some(Skip(ec))
            | [_; AddrOf(_, rhs)] -> Some(Macro(ec, "init", [lhs; self rhs]))
            | _ -> None
        | Deref(ec, Call(_, {FriendlyName = EndsWith "operator="; Parent = Some td}, [], [AddrOf(_, lhs); rhs])) 
            when Set.contains td.Name specialTypeNames ->
          Some(Macro(ec, "=", [self lhs; self rhs]))
        | _ -> None

    // ============================================================================================================    

    let normalizeSignatures self =
      let selfs = List.map self
      function
        | Macro(ec, "_vcc_claim", [Macro(es, "set", []); prop]) -> Some(Macro(ec, "_vcc_claim", [Macro(es, "_vcc_set_empty", []); self prop]))
        | Macro(ec, "_vcc_claim", [Macro(_, "set", elems); prop]) -> Some(Macro(ec, "_vcc_claim", selfs elems @ [prop]))
        | Macro(ec, "_vcc_unclaim", [cl; Macro(_, "set", elems)]) -> Some(Macro(ec, "_vcc_unclaim", selfs (cl :: elems)))
        | Macro(ec, "_vcc_unclaim", [Macro(es, "set", []); prop]) -> Some(Macro(ec, "_vcc_unclaim", [Macro(es, "_vcc_set_empty", []); self prop]))
        | Macro(ec, "_vcc_upgrade_claim", [Macro(_, "set", claimsSet); prop]) -> Some(Macro(ec, "_vcc_upgrade_claim", selfs (claimsSet @ [prop])))
        | Macro(ec, "_vcc_is_claimable", [e]) -> Some(Macro(ec, "_vcc_is_claimable", [Macro({e.Common with Type = Type.TypeIdT}, "_vcc_typeof", [self e])]))
//        | Macro(ec, "\\havoc_others", [e]) -> 
//          let e' = self e
//          Some(Macro(ec, "_vcc_havoc_others", [e'; Macro({e'.Common with Type = Type.TypeIdT}, "_vcc_typeof", [e'])]))
        | _ -> None
      

    // ============================================================================================================    

    helper.AddTransformer ("cpp-begin", TransHelper.DoNothing)

    helper.AddTransformer ("cpp-check-ast-structure", TransHelper.Decl checkAstStructure)
    helper.AddTransformer ("cpp-errors", TransHelper.Expr reportErrors)
    helper.AddTransformer ("cpp-remove-temps", TransHelper.Decl removeTemps)
    helper.AddTransformer ("cpp-globals-init", TransHelper.Decl rewriteGlobalsInitialization)
    helper.AddTransformer ("cpp-special-args", TransHelper.Decl specialArgumentHandling)
    helper.AddTransformer ("cpp-builtin-type-init", TransHelper.Expr removeInitOrAssignOfBuiltinTypes)
    helper.AddTransformer ("cpp-vararg-casts", TransHelper.Expr removeVarargCasts)
    helper.AddTransformer ("cpp-special-types", TransHelper.Decl rewriteSpecialTypes)
    helper.AddTransformer ("cpp-set", TransHelper.Expr rewriteSets)
    helper.AddTransformer ("cpp-map", TransHelper.Expr rewriteMaps)
    helper.AddTransformer ("cpp-remove-object-copying", TransHelper.Expr removeObjectCopyOperations)
    helper.AddTransformer ("cpp-blocks", TransHelper.Expr normalizeBlocks)
    helper.AddTransformer ("cpp-contracts", TransHelper.Decl collectContracts)
    helper.AddTransformer ("cpp-type-contracts", TransHelper.Decl collectTypeContracts)
    helper.AddTransformer ("cpp-field-attributes", TransHelper.Decl rewriteFieldAttributes)
    helper.AddTransformer ("cpp-ghost", TransHelper.Decl rewriteGhost)
    helper.AddTransformer ("cpp-assign-ops", TransHelper.Expr rewriteAssignOps)
    helper.AddTransformer ("cpp-rewrite-casts", TransHelper.Expr rewriteCasts)
    helper.AddTransformer ("cpp-ptr-arithmetic", TransHelper.Expr rewritePtrArithmetic)
    helper.AddTransformer ("cpp-quantifiers", TransHelper.Decl rewriteQuantifiers)
    helper.AddTransformer ("cpp-stack-arrays", TransHelper.Decl handleStackArrays)
    helper.AddTransformer ("cpp-rewrite-functions", TransHelper.Expr rewriteSpecialFunctions)
    helper.AddTransformer ("cpp-rewrite-block-decorators", TransHelper.Expr rewriteBlockDecorators)
    helper.AddTransformer ("cpp-rewrite-functions-with-contracts", TransHelper.Decl rewriteSpecialFunctionsWithContracts)
    helper.AddTransformer ("cpp-rewrite-macros", TransHelper.Expr rewriteExtraMacros)
    helper.AddTransformer ("cpp-retype-macros", TransHelper.Expr retypeMacros)
    helper.AddTransformer ("cpp-bool-conversion", TransHelper.Expr insertBoolConversion)
    helper.AddTransformer ("cpp-rewrite-ops", TransHelper.Expr rewriteExtraOps)
    helper.AddTransformer ("cpp-sanitize-names", TransHelper.Decl sanitizeNames)
    helper.AddTransformer ("cpp-remove-decls", TransHelper.Decl removeSpecialDecls)
    helper.AddTransformer ("cpp-despecialize-alloc", TransHelper.Decl despecializeAlloc)
    helper.AddTransformer ("cpp-assignment-to-stmts", TransHelper.Expr makeAssignmentsIntoStmts)
    helper.AddTransformer ("cpp-signatures", TransHelper.Expr normalizeSignatures)
    helper.AddTransformer ("cpp-declare-parameters", TransHelper.Decl addDeclarationsForParameters)

    helper.AddTransformer ("cpp-end", TransHelper.DoNothing)
