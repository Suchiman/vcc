//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
 open Microsoft.FSharp.Math
 open Microsoft.Research.Vcc
 open Microsoft.Research.Vcc.Util
 open Microsoft.Research.Vcc.TransUtil
 open Microsoft.Research.Vcc.CAST
 
 module Simplifier =
 
  let alwaysPureCalls =
    let dict = new Dict<string,string>()
    List.iter (fun (s, v) -> dict.Add ("_vcc_" + s, v)) 
                [ 
                  // signature letter:
                  //   t - typed pointer (will add encoding of type)
                  //   p - typed pointer (nothing gets added)
                  //   S - $s
                  //   s - old($s)
                  //   a - ptrset
                  //   i - _vcc_size_t
                  //   . - just pass whatever was there
                  "admissibility_pre",      "Sp";
                  "good_for_post_admissibility", "S";
                  "array_members",          "ti";
                  "array_range",            "Sti";
                  "array",                  ".i";
                  "as_array",               "ti";
                  "byte_ptr_subtraction",   "pp";
                  "closed",                 "Sp";
                  "nested",                 "Sp";
                  "claims_obj",             "pp";
                  "claims",                 "p.";
                  "current_state",          "S";
                  "is_claimable",           ".";
                  "union_active",           "pp";
                  "not_shared",             "Sp";
                  "ref_cnt",                "Sp";
                  "valid_claim",            "Sp";
                  "skip_wf",                "";
                  "containing_struct",      "pp";
                  "depends",                "sSpp";
                  "dont_instantiate",       "p";
                  "dont_instantiate_int",   "i";
                  "emb",                    "Sp";
                  "extent",                 "Sp";
                  "full_extent",            "p";
                  "get_fnptr",              "i.";
                  "get_memory_allocator",   "";
                  "get_string_literal",     "ii";
                  "in_array",               "pti";
                  "in_domain",              "Spp";
                  "in_vdomain",             "Spp";
                  "in_claim_domain",        "pp";
                  "domain",                 "Sp";
                  "imply_inv",              "St";
                  "inlined_array",          "t";
                  "inv",                    "St";
                  "inv2",                   "sSt";
                  "inv2_when_closed",       "sSt";
                  "i1_to_ptr",              ".";
                  "i2_to_ptr",              ".";
                  "i4_to_ptr",              ".";
                  "i8_to_ptr",              ".";
                  "is",                     "p.";
                  "is_array",               "Sti";
                  "is_array_emb",           "Stip";
                  "is_fresh",               "sSp";
                  "is_global",              "t";
                  "is_global_array",        "ti";
                  "is_malloc_root",         "Sp";
                  "is_mutable_array",       "Sti";
                  "is_object",              "p";
                  "is_object_root",         "Sp";
                  "is_thread_local_array",  "Sti";
                  "is_thread",              "p";
                  "me",                     "";
                  "mutable",                "Sp";
                  "thread_owned",           "Sp";
                  "non_null_array_range",   "ti";
                  "non_null_extent",        "Sp";
                  "non_null_set_singleton", "p";
                  "owner",                  "Sp";
                  "owns",                   "Sp";
                  "obj_eq",                 "pp";
                  "obj_neq",                "pp";
                  "wrapped",                "St";
                  "ptr_eq",                 "pp";
                  "ptr_neq",                "pp";
                  "ptr_to_i1",              "p";
                  "ptr_to_i2",              "p";
                  "ptr_to_i4",              "p";
                  "ptr_to_i8",              "p";
                  "ptr_to_u1",              "p";
                  "ptr_to_u2",              "p";
                  "ptr_to_u4",              "p";
                  "ptr_to_u8",              "p";
                  "thread_local",           "Sp";
                  "thread_local2",          "St";
                  "set_cardinality",        "a";
                  "set_disjoint",           "aa";
                  "set_difference",         "aa";
                  "set_empty",              "";
                  "set_eq",                 "aa";
                  "set_subset",             "aa";
                  "set_in",                 "pa";
                  "set_in0",                "pa";
                  "set_in2",                "pa";
                  "set_intersection",       "aa";
                  "set_singleton",          "p";
                  "set_union",              "aa";
                  "set_universe",           "";
                  "sk_hack",                ".";
                  "span",                   "p";
                  "volatile_span",          "Sp";
                  "typed",                  "Sp";
                  "typed2",                 "St";
                  "typeof",                 "p";
                  "u1_to_ptr",              ".";
                  "u2_to_ptr",              ".";
                  "u4_to_ptr",              ".";
                  "u8_to_ptr",              ".";                  
                  "vs_ctor",                "Sp";
                  "when_claimed",           "";
                  "mutable_increases",      "sS";
                  "meta_eq",                "sS";
                  "program_entry_point",    "S";
                  "always_by_claim",        "pp";
                  "reads_check_pre",        "S";
                  "reads_check_post",       "S";
                  "gemb",                   "p";
                  "start_here",             "";
                  "full_stop",              "S";
                  "pre_wrap",               "S";
                  "pre_unwrap",             "S";
                  "pre_static_wrap",        "S";
                  "pre_static_unwrap",      "S";
                  "unwrap_check_pre",       "Sp";
                  "good_for_post_can_unwrap","S";
                  "unwrap_post",            "..pp";
                  "unwrap_post_claimable",  "..pp";
                  "wrap_post",              "..pp";
                  "take_over",              ".pp";
                  "release",                "..pp";
                  "expect_unreachable",     "";
                  "bv_lemma",               ".";
                  "is_non_primitive_ptr",   "p";
                  "extent_mutable",         "Sp";
                  "extent_zero",            "Sp";
                  "extent_is_fresh",        "sSp";
                  "inv_is_approved_by",     "sSp..";
                  "inv_is_owner_approved",  "sSp.";
                  "is_approved_by",         "...";
                  "is_owner_approved",      "..";
                  "updated_only_values",    "sS.";
                  "updated_only_domains",   "sS.";
                  "domain_updated_at",      "sS..";
                  "claims_claim",           "..";
                  "stuttering_pre","S.";
                  "is_admissibility_check", "";
                  "is_unwrap_check", "";
                  "is_stuttering_check", "";
                  "new_ownees", "S..";
                  "rec_eq", "..";
                  "account_claim", "Spp";
                ]
    dict

  let isRecord (td : TypeDecl) = hasBoolAttr "record" td.CustomAttr
    
  let exprDependsOnSpecExpr (expr : Expr) = 
    let (specFound : string option ref) = ref None
    let continueIfNotFound() = Option.isNone !specFound
    let hasSpec' self = function
      | Dot(_, _, f) when f.IsSpec -> specFound := Some("field '" + f.Name + "'"); false
      | Ref(_, ({Kind = SpecLocal} as v)) -> specFound := Some("variable '" + v.Name + "'"); false
      | Ref(_, ({Kind = SpecParameter|OutParameter} as v)) -> specFound := Some("parameter '" + v.Name + "'"); false
      | CallMacro(_, ("_vcc_alloc" | "_vcc_stack_alloc"), _, _) -> false
      | Call(_, ({Name = "_vcc_containing_struct"}), _, [addr; df]) ->  self addr; self df; false
      | Macro(_, "by_claim", [_; obj; ptr]) -> self obj; self ptr; false
      | Call(_, ({IsSpec = true} as f), _, _) -> specFound := Some("function '" + f.Name + "'"); false
      | Call(_, fn, _, args) ->
        let checkNonSpecPar (p : Variable) (e : Expr) =
          if p.Kind <> VarKind.SpecParameter && p.Kind <> VarKind.OutParameter then self e
        List.iter2 checkNonSpecPar fn.Parameters args
        false
      | Old(_, (CallMacro(_, "_vcc_by_claim", _, _)), expr) -> self expr; false
      | Atomic(_, _, expr) -> self expr; false
      | Block(_, exprs) ->
        let rec checkLastExpr = function
          | [] -> ()
          | [x] -> self x
          | _ :: xs -> checkLastExpr xs
        checkLastExpr exprs; false
      | _ -> continueIfNotFound()
    expr.SelfVisit hasSpec'
    !specFound
    

  let inRangeBvExtract (expr:Expr) numOfBits =
    let ec = expr.Common
    let (lower, upper) =
      let mkBigInt (n:int32) = new Math.BigInt(n)
      let two = mkBigInt 2
      let sub bi1 bi2 = bi1 - bi2
      if ec.Type.IsSignedInteger then
        let x = Math.BigInt.Pow(two, (mkBigInt (numOfBits - 1)))
        (sub zero x, sub x one)
      else
        (zero, sub (Math.BigInt.Pow(two, (mkBigInt numOfBits))) one)
    let mkCheck args = Expr.Prim({ec with Type = Type.Bool }, Op("<=", Processed), args )
    let lowerCheck = mkCheck [IntLiteral(ec, lower); expr]
    let upperCheck = mkCheck [expr; IntLiteral(ec, upper)]
    let assertEc = {(afmte 8503 (lower.ToString() + " <= {0} && {0} <= " + upper.ToString() + " in bitfield assignment") [expr]) with Type = Type.Bool; }
    Expr.Prim(assertEc, Op("&&", Processed), [lowerCheck; upperCheck])

  let init (helper:Helper.Env) =
  
    // ============================================================================================================
    
    let desugarLambdas decls =
      let defs = ref []
      
      // n-ary lambdas are represented as nested quantifiers. We need to make sure that we place an in_lambda for every quantifier
      // also, we need to adjust the types. Originally, all nested quantifiers have the same type, which is the type of the
      // entire lambda. We need to shave of map-type for map-type as we walk down the nested quantifiers
      // for this, we pass in the extra optional type argument. Initially, the argument in None, which corresponds to the
      // fact that we have found the outermost quantifier
      let rec addNestedInLambdas mapType self = function
        | Quant (_, ({ Kind = Lambda; Condition = None; Body = Cast({Type = Type.Bool}, _, Quant(qc, ({Kind = Lambda})))})) as outer when Option.isNone mapType ->
          Some(outer.SelfMap(addNestedInLambdas (Some(qc.Type))))
        | Quant (c, ({ Kind = Lambda; Condition = None; Body = Cast(({Type = Type.Bool} as bc), _, Quant(qc, ({Kind = Lambda} as nestedQData)))} as q)) -> 
          let nestedType = match mapType with | Some(Type.Map(_, t)) -> t | _ -> die()
          let nestedQ = Quant({qc with Type = nestedType }, nestedQData)
          let nestedQ = nestedQ.SelfMap(addNestedInLambdas (Some(nestedType)))
          Some ((Quant (c, { q with Body = Macro(bc, "in_lambda", [Expr.True; nestedQ]) })))
        | _ -> None

      let expand self = function
        | Quant (c, ({ Kind = Lambda; Condition = None; Body = Macro (_, "in_lambda", [cond; expr]) } as q)) ->
          Some (self (Quant (c, { q with Condition = Some cond; Body = (self expr) })))
          
        | Quant (c, ({ Kind = Lambda } as q)) ->
          let (domain, range) =
            match c.Type with
              | Type.Map (d, r) -> (d, r)
              | _ -> helper.Die()
          
          let isPure = ref true
          
          let rec hasQVar vars expr =
            let argHasIt args =
              let argsHaveIt = List.map (hasQVar vars) args // ensure to really visit all arguments because of the side effects
              List.exists (fun x -> x) argsHaveIt
              
            let hasIt = ref false
            let check self = function
              | Expr.Ref (_, v) when _list_mem v vars ->
                hasIt := true
                false
              | Deref(_, Dot(_,e,f)) when hasBoolAttr "record" f.Parent.CustomAttr ->
                self e; false
              | Deref _ ->
                isPure := false
                true
              | Call(_, fn, _, args) ->
                if argHasIt args then
                  hasIt := true
                  if not fn.IsStateless then isPure := false
                false
              | _ -> true
            (expr:Expr).SelfVisit check
            !hasIt
            
          let parms = ref []
          
          let repl = 
          
            let rec repl' vars prestate self = 
              let turnExpressionIntoParameter (expr : Expr) = 
                let pname = "#l" + (List.length !parms).ToString()
                let var = { Name = pname; Type = expr.Type; Kind = QuantBound } : Variable
                let expr = 
                  match prestate with
                    | None -> expr
                    | Some prestate -> Old(expr.Common, prestate, expr)
                parms := (expr, var) :: !parms
                Some (Expr.Ref (expr.Common, var))

              function
                | Old(ec, prestate, expr) -> Some(expr.SelfMap (repl' vars (Some(prestate))))
                | Quant(ec, qd) as quant when hasQVar vars quant ->
                  let self = fun (e : Expr) -> e.SelfMap(repl' (qd.Variables @ vars) prestate)
                  Some(Quant(ec, {qd with Body = self qd.Body}))
                | Macro(ec, "vs_updated", [Dot(dc, e1, f); e2]) when hasQVar vars e1 || hasQVar vars e2 ->
                  //dbgBreak()
                  let e1' = self e1
                  let e2' = self e2
                  Some(Macro(ec, "vs_updated", [Dot(dc, e1', f); e2']))
                  // special handling because the field and assignment are split into two separate arguments but must be handled
                  // together
                | expr when hasQVar vars expr -> None
                | IntLiteral _
                | BoolLiteral _
                | Macro (_, "null", [])
                | CallMacro(_, "_vcc_typeof", _, _)
                | CallMacro(_, "vs_zero", _, [])
                | Cast (_, _, Macro (_, "null", [])) -> None
                | expr -> turnExpressionIntoParameter expr
            repl' q.Variables None
          
          let cond = 
            match q.Condition with
              | Some c -> c.SelfMap repl
              | None -> Expr.True
          let body = q.Body.SelfMap repl
          
          let fn =
            { Token           = c.Token
              IsSpec          = true
              RetType         = c.Type
              OrigRetType     = c.Type
              Name            = "lambda#" + (helper.UniqueId()).ToString()
              Parameters      = [for (_, var) in !parms -> { var with Kind = Parameter }]
              TypeParameters  = []
              Requires        = []
              Ensures         = []
              Writes          = []
              Reads           = if !isPure then [] else [Expr.Macro ({ bogusEC with Type = Type.PtrSet }, "_vcc_set_universe", [])]
              CustomAttr      = [VccAttr ("is_pure", "")]
              Body            = None
              IsProcessed     = true
            } : Function                  
                
          let axcall = Call ({ bogusEC with Type = fn.RetType }, fn, [], List.map (snd >> mkRef) !parms)
          let idxvar = match q.Variables with [x] -> x | _ -> die()
          let idx = Macro ({ bogusEC with Type = range }, "map_get", [axcall; mkRef idxvar])
          let axiom = Quant ({ c with Type = Bool }, 
                             { Kind = Forall
                               Variables = idxvar :: List.map snd !parms
                               Triggers = [[idx]]
                               Condition = Some cond
                               Body = Prim ({ c with Type = Bool }, Op ("==", Processed), [idx; body])
                             })
          
          defs := Top.FunctionDecl fn :: Top.GeneratedAxiom(axiom, Top.FunctionDecl(fn)) :: !defs
          Some (Call (c, fn, [], List.map (fst >> self) !parms))
          
        | _ -> None   
                   
      let decls = decls |> deepMapExpressions (addNestedInLambdas None) |> deepMapExpressions expand 
      decls @ !defs    
    
    // ============================================================================================================
    
     
    /// Get rid of &&, || -- operators that alter control flow.
    /// Actually FELT translates them all to "ite" (IConditional) nodes.
    let rec doRemoveLazyOps inSpecBlock ctx self = function
      | Expr.Macro (c, "ite", [cond; th; el]) when not ctx.IsPure ->
        let varKind =
          match exprDependsOnSpecExpr th, exprDependsOnSpecExpr el with
            | None, None when not inSpecBlock  -> VarKind.Local
            | _,_ -> VarKind.SpecLocal
        
        let tmp = getTmp helper "ite" c.Type varKind
        let c' = { c with Type = Void }
        let tmpRef = Expr.Ref (c, tmp)
        let write = Expr.If (c', cond,
                             Macro (c', "=", [tmpRef; th]),
                             Macro (c', "=", [tmpRef; el]))
        addStmtsOpt [VarDecl (c', tmp); self write] tmpRef
      | Macro(ec, "spec", args) -> Some(Macro(ec, "spec", List.map (fun (e:Expr) -> e.SelfCtxMap(true, doRemoveLazyOps true)) args))
      | _ -> None
    
    let removeLazyOps = deepMapExpressionsCtx (doRemoveLazyOps false)
    
    // ============================================================================================================

    /// Rename locals if their names clashes with parameters or locals from other scopes   
    let doRemoveNestedLocals (f:Function)=
      let subst = new Dict<Variable, Variable>()
      let seenNames = new Dict<string,bool>()    
      let addVarToSeen (v : Variable) = seenNames.[v.Name] <- true
      let varDecls = ref []

      let renameVar (v : Variable) =
        if seenNames.ContainsKey(v.Name) then
          let renamedVar = { v with Name = v.Name + "#" + subst.Count.ToString()}
          if v.Kind <> Local && v.Kind <> SpecLocal then die()
          subst.[v] <- renamedVar
          renamedVar
        else
          addVarToSeen v
          v 

      let rnExpr self = function
        | Expr.VarDecl (ce, var) ->
          varDecls := Expr.VarDecl (ce, renameVar var) :: !varDecls
          Some (Expr.Comment(ce, "__vcc__ removed decl"))
        | Expr.Ref(ce, var) ->
          match subst.TryGetValue(var) with
            | true, substName -> Some(Expr.Ref(ce, substName)) 
            | false, _ -> None
        | VarWrite _ -> die()
        | _ -> None
        
      f.Body <- 
        match f.Body with
          | Some body -> 
            let body' =  body.SelfMap(rnExpr)
            Some(Expr.MkBlock((List.rev !varDecls) @ [body']))
          | None -> None
      f
       
    let removeNestedLocals = mapFunctions doRemoveNestedLocals    
    
    // ============================================================================================================
    
    let singleFieldStruct tok tp_name field_name (t:Type) =
      let td =
          { Token = tok
            Name = tp_name
            Fields = []
            Invariants = []
            CustomAttr = []
            SizeOf = t.SizeOf
            Kind = Struct
            IsNestedAnon = false
            GenerateEquality = NoEq
            GenerateFieldOffsetAxioms = false
            Parent = None
            IsVolatile = false 
            IsSpec = false }
      let (t, vol) = match t with | Volatile(t) -> (t, true) | t -> (t, false)
      let singleField =
        { Name = field_name
          Token = tok
          Type = t
          Parent = td
          IsSpec = false
          Offset = Normal 0
          IsVolatile = vol
          CustomAttr = [] }
      td.Fields <- [singleField]
      (td, singleField)
          
    (*
       Turn a global:
       
         int x;
         
       into:
       
         struct {
           int data;
         } x;
         
       and then all "x" into "x.data". This allows to own globals (with emb(&x) at the C level).
     *)
    let wrapPrimitiveGlobals decls =
      // TODO: Ptr kind - deal also with spec globals, where all ptrs are spec ptrs
      let globalSubst = new Dict<_,_>()
      let varSubst = new Dict<_,_>()
      let rec isPrimitive = function
        | Type.Volatile(t) -> isPrimitive(t)
        | t -> not t.IsComposite
      let handle = function
        | Top.Global (v, init) when isPrimitive v.Type ->
          let (td, fld) = singleFieldStruct bogusToken ("swrap#" + v.Name) "data" v.Type
          let approvesInv =
            match v.Type with 
              | Type.Volatile _ -> 
                let ecObjT = { bogusEC with Type = Type.ObjectT }
                let this = Macro( { bogusEC with Type = Type.PhysPtr(Type.Ref td) }, "this", [])
                [Macro (boolBogusEC(), "approves", 
                  [ Macro( ecObjT, "_vcc_owner", [ this ]); 
                    Deref ({bogusEC with Type = v.Type}, 
                           Expr.MkDot (this, fld))])]
              | _ -> []
          td.Invariants <- approvesInv
          let v' = { Name = "wrap#" + v.Name; Type = Type.Ref td; Kind = VarKind.Global } : Variable
          let repl ec =
            let inner = Macro ({ ec with Type = Type.MkPtrToStruct(td) }, "&", [Expr.Ref ({ ec with Type = Type.Ref td }, v')])
            match v.Type with
              | Array (t, _) ->
                Deref ({ ec with Type = t }, Dot ({ ec with Type = PhysPtr t }, inner, fld))
              | _ ->
                Deref (ec, Dot ({ ec with Type = PhysPtr ec.Type }, inner, fld))
          globalSubst.Add (v, repl)
          varSubst.Add(v, v')
          [Top.TypeDecl td; Top.Global (v', init)]
        | d -> [d]
        
      let repl self = function
        | Expr.Ref (ec, v) ->
          match globalSubst.TryGetValue v with
            | true, f -> Some (f ec)
            | _ -> None
        | _ -> None
        
      let replaceVarsInGeneratedAxioms = function
        | Top.GeneratedAxiom(ax, Top.Global(v, _)) as ga ->
          match varSubst.TryGetValue(v) with
            | true, v' -> Top.GeneratedAxiom(ax, Top.Global(v', None))
            | _ -> ga
        | t -> t
  
      decls |> List.map handle |> List.concat |> List.map replaceVarsInGeneratedAxioms |> deepMapExpressions repl 
    
    // ============================================================================================================

    let cleanupAddrDeref =
      let cleanupAddDeref' self = function
        | Macro (ec, "&", [expr]) -> 
          match self expr with 
            | Deref (_, e) -> Some (e)
            | expr' -> Some(Macro(ec, "&", [expr']))
        | Cast (ec, _, e) when ec.Type = e.Type -> Some (self e)
        | _ -> None
      deepMapExpressions cleanupAddDeref'

    
    let expandByClaim =
      let pushOldIn self = function
        | Deref (_, Macro (_, "&", [e])) -> Some (self e)
        | Old (ec, state, expr) ->
          match self expr with
            | Macro (_, ("bv_extract_signed" | "bv_extract_unsigned" as name), [e; i; j; k]) ->
              Some (Macro (ec, name, [self (Old (ec, state, e)); i; j; k]))
            | _ -> None
        | _ -> None    
            
      let doByClaim self = function
        | Expr.Old (ec, Call (_, { Name = "_vcc_by_claim" }, _, [c]), e) ->
          match e with
            | Expr.Deref (_, (Dot (_, obj, f) as ptr))
            | Expr.Deref (_, (Index (_, Dot (_, obj, f), _) as ptr)) ->
              if f.IsVolatile then
                helper.Error (ec.Token, 9629, "by_claim(...) can only refer to a non-volatile field", Some(f.Token))
              Some (Expr.Macro (ec, "by_claim", [self c; self obj; self ptr]))
            | _ ->
              helper.Error (ec.Token, 9628, "by_claim(...) expects field or embedded array reference as a second parameter", None)
              None            
        | _ -> None
      deepMapExpressions pushOldIn >> deepMapExpressions doByClaim
    
    
    // ============================================================================================================
    
    let handleApprovers decls =
      let generated = gdict()
      let res expr =
        generated.[expr] <- true
        Some expr
      let approvers = gdict()
      let selfApproved = gdict()
      let doApproves self = function
        | Macro (ec, "approves", [approver; Deref (_, Dot (_, (This as th), subject))]) ->        
          match approver with
            | CallMacro (_, "_vcc_owner", _, [This]) ->
              res (Macro (ec, "_vcc_inv_is_owner_approved", [th; mkFieldRef subject]))
            | Deref (_, Dot (_, This, approver)) ->
              if approver = subject then
                selfApproved.[approver] <- true
              approvers.[approver] <- ec.Token
              res (Macro (ec, "_vcc_inv_is_approved_by", [th; mkFieldRef approver; mkFieldRef subject]))
            | expr ->
              helper.Error (ec.Token, 9670, "approves(...) needs owner(this) or this->field as the first parameter", None)
              None
        | Macro (ec, "approves", _) as expr ->
          helper.Error (ec.Token, 9669, "approves(...) needs this->field as the second parameter", None)
          None
        | expr ->
          None
    
      let checkForAxiom axioms = function
        | Macro (ec, "_vcc_inv_is_owner_approved", [th; subject]) as expr ->
          generated.Remove expr |> ignore
          Macro (ec, "_vcc_is_owner_approved", [typeExpr th.Type.Deref; subject]) :: axioms
        | Macro (ec, "_vcc_inv_is_approved_by", [th; approver; subject]) as expr ->
          generated.Remove expr |> ignore
          Macro (ec, "_vcc_is_approved_by", [typeExpr th.Type.Deref; approver; subject]) :: axioms
        | _ -> axioms
        
      let doDecl = function
        | Top.TypeDecl (td) as d ->
          generated.Clear()
          approvers.Clear()
          selfApproved.Clear()
          td.Invariants <- td.Invariants |> List.map (fun e -> e.SelfMap doApproves)
          let axioms = 
            List.map TransUtil.splitConjunction td.Invariants |> 
              List.concat |> 
              List.fold checkForAxiom [] |>
              List.map Top.Axiom
          for e in generated.Keys do
            helper.Error (e.Token, 9671, "approves(...) can only be used as a top-level invariant")
          for f in approvers.Keys do
            match f.Type with
              | ObjectT -> ()
              | _ ->
                helper.Error (approvers.[f], 9673, "approver field '" + f.Name + "' should have obj_t type, it has '" + f.Type.ToString() + "'")
            if not (selfApproved.ContainsKey f) && f.IsVolatile then
              helper.Error (approvers.[f], 9672, "volatile field '" + f.Name + "' is an approver, but not a self-approver")
          d :: axioms
        | d -> [d]
        
      List.map doDecl decls |> List.concat
        
    // ============================================================================================================    
    
    /// Remove operators like +=, -=, pre++ ...
    /// Also handle assignments to bitfields (as they also require precomputation).
      (* Require precomputation for example:
           ... *(f()) += 3 ...
         should go into:
           tmp = f();
           ... *tmp = *tmp + 3 ... 
         
         TODO: FELT does the desugaring itself, but does it wrongly. Investigate.
       *)        

    let cacheAssignTarget self = function
      | Expr.Deref (_, Dot(_, _, f)) as e when isRecord f.Parent -> (self, e)
      | Expr.Deref (c, ptr) ->
        let cacheVarKind =
          match exprDependsOnSpecExpr ptr with
            | Some _ -> VarKind.SpecLocal
            | None -> VarKind.Local
        let (inits, tmp) = cache helper "assignOp" ptr cacheVarKind
        let addInits e = Expr.MkBlock (List.map self (inits @ [e]))
        addInits, Expr.Deref (c, tmp)
      | _ as e -> (self, e)


    let removeAssignOps self = function
    
        | Expr.Macro (c, "map_set", [map; idx; value]) ->
          let (inits, map) = cacheAssignTarget self map
          Some (inits (Expr.Macro (c, "=", [map; Expr.Macro (map.Common, "map_updated", [map; idx; value])])))
        | Expr.Macro (c, "=", [Expr.Macro(c1, "map_get", [map; idx]); expr]) ->
          Some(self(Expr.Macro(c, "map_set", [map; idx; expr])))
        // here we assume that bit extraction is always padded; when we write, we just ignore the padding
        | Expr.Macro (c, "=", [ Expr.Macro (c', ("bv_extract_signed" | "bv_extract_unsigned"), 
                                            [e1; 
                                             Expr.IntLiteral (_, total);
                                             Expr.IntLiteral (_, beg); 
                                             Expr.IntLiteral (_, end_)]); e2]) ->
                                             
          let beg = bigint.ToInt32 beg
          let end_ = bigint.ToInt32 end_
          let total = bigint.ToInt32 total          
          let rec stripUnchecked = function
            | Expr.Macro(_, name, [e]) when name.StartsWith("unchecked_") -> stripUnchecked e
            | e -> e
          let (inits1, bv1) = cacheAssignTarget self (self (stripUnchecked e1)) // the unchecked ops were only meaningful for reading
          let (inits2, e2) = cache helper "assignSrc" (self e2) (VarKind.Local)
          let concat = Macro ({c' with Type = c'.Type.Deref}, "bv_update", [bv1; mkInt total; mkInt beg; mkInt end_; e2])
          let rangeAssertForRhs = Expr.MkAssert(inRangeBvExtract (ignoreEffects e2) (end_ - beg) )
          Some (inits1 (Expr.MkBlock (inits2 @ [rangeAssertForRhs; Expr.Macro (c, "=", [bv1; concat])])))
          
        | Expr.Macro (c, "=", [ Expr.Macro (_, ("_vcc_ptr_to_i4" | "_vcc_ptr_to_i8" | "_vcc_ptr_to_u4" | "_vcc_ptr_to_u8"), [e1]); e2 ]) -> 
          let (inits, target) = cacheAssignTarget self (self e1)
          Some (inits (Expr.Macro(c, "=", [target; Expr.Macro({c with Type = e1.Type}, intToPtrFunction e2.Type, [e2])])))
        | Expr.Macro (c, "=", [ Expr.Macro (_, ("_vcc_i4_to_ptr" | "_vcc_i8_to_ptr" | "_vcc_u4_to_ptr" | "_vcc_u8_to_ptr"), [e1]); e2 ]) -> 
          let (inits, target) = cacheAssignTarget self (self e1)
          Some (inits (Expr.Macro(c, "=", [target; Expr.Macro({c with Type = e1.Type}, ptrToIntFunction e1.Type, [e2])])))
        | Expr.Macro (c, "=", [ Expr.Macro (_, name, [e1]); e2 ]) when name.StartsWith("unchecked_") ->
          let (inits, target) = cacheAssignTarget self (self e1)
          Some (inits (Expr.Macro(c, "=", [target; uncheckedSignConversion e2]))) 
        | Expr.Macro (c, "=", [ Expr.Cast(_,_, e1); e2]) ->
          Some(self(Expr.Macro(c, "=", [e1; e2])))
        // fix type of remaining occurrences of the transformer functions         
        | Expr.Macro(c, name, args) when name.StartsWith("bv_extract_") && c.Type._IsPtr -> 
          Some(Expr.Macro({c with Type = c.Type.Deref}, name, List.map self args))
        | Expr.Macro(c, name, args) when name.StartsWith("unchecked_") ->
          match c.Type with
            | Ptr (Integer _ as i) -> Some(Expr.Macro({c with Type = i}, name, List.map self args)) 
            | _ -> None       
        | _ -> None
        
    // ============================================================================================================
    
    
    /// Remember which locals have the address of them taken, and then for each of them
    /// turn them into heap-allocated pointers. Same goes for local structs.
    /// TODO: add free(...) somewhere
    (* void f()
       {
          int x, y;
          struct S s;
          
          x = 7;
          g(&x);
          s.y = 12;          
       }
       
       void f()
       {
         int *x_;
         int y;
         struct S *s_;
         
         x_ = alloc(int);
         s_ = alloc(struct S);
         
         *x_ = 7;
         g(&( *x_));
         ( *s_).y = 12;
       }
     *)
    let replaceWithPointers (subst:Dict<_,_>) _ = function
      | Expr.Macro (c, "&", [Expr.Ref (_, v)]) ->
        match subst.TryGetValue v with
          | true, (v', _) -> Some (Expr.Ref (c, v'))
          | _ -> None
      | Expr.Ref (c, v) ->
        match subst.TryGetValue v with
          | true, (v', _) -> Some (Expr.Deref (c, Expr.Ref ({ c with Type = v'.Type }, v')))
          | _ -> None
      | Expr.VarDecl (_, v) ->
        match subst.TryGetValue v with
          | true, (_, decl) -> Some(decl)
          | false, _ -> None
      | _ -> None

    let heapifyAddressedLocals decls =
      let addressableLocals = new Dict<_,_>()
      let fnTok = ref bogusEC
      let fakeEC t = { !fnTok with Type = t }
      let pointernize comm v =
        let mkEc t = { comm with Type = t } : ExprCommon
        if not (addressableLocals.ContainsKey v) then
          let v' = { v with Type = Type.MkPtr(v.Type, v.IsSpec); Name = "addr." + v.Name; Kind = VarKind.Local }
          let vRef = Expr.Ref({forwardingToken (comm.Token) None (fun () -> "&" + v.Name) with Type = v'.Type} , v')
          let alloc = Expr.Call (fakeEC v'.Type, internalFunction helper "stack_alloc", [v.Type], [Macro(bogusEC, "stackframe", [])])
          let assign = Expr.Macro (fakeEC Void, "=", [vRef; alloc])
          let init =
            if v.Kind = VarKind.Parameter || v.Kind = VarKind.SpecParameter || v.Kind = VarKind.OutParameter then
              [Expr.Macro (fakeEC Void, "=", [Expr.Deref (fakeEC v.Type, Expr.Ref ({ comm with Type = v'.Type }, v')); mkRef v])]
            else []
          let def = VarDecl (fakeEC Void, v') :: assign :: init
          addressableLocals.[v] <- (v', Expr.MkBlock def)
          
      let pointsToStruct = function
        | Ptr(Type.Ref({Kind = (TypeKind.Struct|TypeKind.Union)})) -> true
        | _ -> false
          
      let findThem inBody self = function
         | Expr.Deref (_, dot) as expr ->
           let rec aux = function
             | Expr.Index (_, e, idx) -> 
               self idx
               aux e
             | Expr.Dot (_, e, _) -> aux e
             | Expr.Macro (_, "&", [Expr.Ref _]) -> ()
             | e -> self e
           aux dot
           false // don't recurse
         | Expr.Macro (_, "=", [Expr.Deref(_, e1); Expr.Deref(_, e2)]) when pointsToStruct e2.Type -> self e1; self e2; false            
         | Expr.Macro (_, "&", [Expr.Ref (c, ({ Kind = (VarKind.Local|VarKind.Parameter|VarKind.SpecLocal|VarKind.SpecParameter|VarKind.OutParameter) } as v))]) when inBody ->
           pointernize c v
           true
         | Expr.Macro (cmn, "&", [Expr.Ref (c, ({ Kind = (VarKind.Parameter|VarKind.SpecParameter|VarKind.OutParameter) } as v))]) when not inBody ->
           helper.Error(cmn.Token, 9666, "Cannot take an parameter's address inside of function contracts")
           true
         | _ -> true
            
      let doFunction (d:Function) =
        match d.Body with
          | Some b ->
            fnTok := { !fnTok with Token = d.Token }
            List.iter (fun (e:Expr) -> e.SelfVisit (findThem false)) (d.Reads @ d.Writes @ d.Requires @ d.Ensures)
            b.SelfVisit (findThem true)
            let b = b.SelfMap (replaceWithPointers addressableLocals)
            d.Body <- Some b
          | None -> ()
        d
        
      decls |> mapFunctions doFunction
    
    // ============================================================================================================
    
    let handleGlobals decls =
      // TODO Ptr kind for globals
      let globalSubst = new Dict<_,_>()
      let handle = function
        | Top.Global ({ Kind = VarKind.Global|VarKind.ConstGlobal; Type = Array (t, sz) } as v, init) ->
          let v' = { v with Type = PhysPtr t; Kind = VarKind.ConstGlobal }
          globalSubst.[v] <- (v', Expr.MkBlock [])
          let is_global = Expr.Macro (boolBogusEC (), "_vcc_is_global_array", 
                                      [mkRef v'; mkInt sz])
          [Top.Global (v', init); Top.GeneratedAxiom(is_global, Top.Global(v', None))]
        | Top.Global ({ Kind = VarKind.Global|VarKind.ConstGlobal } as v, init) ->
          let v' = { v with Type = PhysPtr v.Type; Kind = VarKind.ConstGlobal }
          globalSubst.[v] <- (v', Expr.MkBlock [])
          let is_global = Expr.Macro (boolBogusEC (), "_vcc_is_global", 
                                      [mkRef v'])
          [Top.Global (v', init); Top.GeneratedAxiom(is_global, Top.Global(v',None))]
        | d -> [d]
        
      let replaceVarsInGeneratedAxioms = function
        | Top.GeneratedAxiom(ax, Top.Global(v, _)) as ga ->
          match globalSubst.TryGetValue(v) with
            | true, (v', _) -> Top.GeneratedAxiom(ax, Top.Global(v', None))
            | _ -> ga
        | t -> t
        
      decls |> List.map handle |> List.concat |> List.map replaceVarsInGeneratedAxioms |> deepMapExpressions (replaceWithPointers globalSubst)  

   
    // ============================================================================================================
    
    /// Get rid of while(){}, do{}while(), for(;;){}, break and continue
    let rec loopAndSwitchDesugaring labels self s =
      let generateUnique = helper.UniqueId
      let doLoop (loopEc:ExprCommon) contract =
        let unique = generateUnique().ToString()
        let break_lbl = { Name = "#break_" + unique } : LabelId
        let continue_lbl = { Name = "#continue_" + unique } : LabelId
        let rec aux invs writes = function
          | Assert (_, Expr.Macro (_, "loop_writes", [e])) :: rest -> aux invs (e :: writes) rest
          | Assert (_, e) :: rest -> aux (e :: invs) writes rest
          | [] -> (List.rev invs, List.rev writes)
          | _ -> die()
        let (invs, writes) = aux [] [] contract
        let mkLoop stmts =
          let loop = Loop ({ voidBogusEC() with Token = loopEc.Token }, invs, writes, Expr.MkBlock stmts)
          Some (Expr.MkBlock [loop; Label (loopEc, break_lbl)])
        let inBody (body:Expr) =
          body.SelfMap (loopAndSwitchDesugaring (Some (break_lbl, continue_lbl)))
        (mkLoop, inBody, break_lbl, continue_lbl)
      
      let doSwitch ctx stmts =
        let (expr, cases) = 
          match stmts with
          | expr :: cases -> (expr, cases)
          | _ -> die()
        let condCopy, condExpr = cache helper "switch" expr VarKind.Local
        let end_switch_lbl = { Name = "#end_of_switch_" + generateUnique().ToString() } : LabelId
        let updatedCtx =
          match ctx with
          | Some(_, continue_lbl) -> Some(end_switch_lbl, continue_lbl)
          | None -> Some(end_switch_lbl, ({ Name = "dummy_label"} : LabelId))
        let doCaseBody (stmts : Expr list) = [ for s in stmts -> s.SelfMap (loopAndSwitchDesugaring updatedCtx) ]
        let doCase case =
          let case_lbl = { Name = "#switch_case_" + generateUnique().ToString() } : LabelId
          let (token, caseExpr, caseBody) =
            match case with
            | Expr.Macro(token, "case", caseExpr :: caseBody) -> (token, Some(caseExpr), caseBody)
            | Expr.Macro(token, "default", caseBody) -> (token, None, caseBody)
            | _ -> die()
          let caseDispatch = 
            let gotoCase = Goto(token, case_lbl)
            match caseExpr with
            | None -> gotoCase
            | Some(caseExpr) -> If(voidBogusEC(), Prim({condExpr.Common with Type = Bool}, Op("==", Processed), [condExpr; caseExpr]), gotoCase, Expr.MkBlock([]))
          let doneCaseBody = [Label(token, case_lbl)] @ (doCaseBody caseBody)
          (caseDispatch, doneCaseBody)
          
        let (dispatch, bodies) = cases |> List.map doCase |> List.unzip
        let ec' = {expr.Common with Type = Void}
        Expr.MkBlock
         (condCopy @ dispatch @ [Goto(ec', end_switch_lbl)] @ (List.concat bodies) @ [Label(ec', end_switch_lbl)])
                  
      match s with
        | Macro (wtok, "while", [Macro (_, "loop_contract", conds); cond; body]) ->
          let (mkLoop, inBody, break_lbl, continue_lbl) = doLoop wtok conds
          mkLoop [ If (wtok, cond, 
                          inBody body, 
                          Goto (wtok, break_lbl)); 
                   Label (wtok, continue_lbl)]
     
        | Macro (wtok, "doUntil", [Macro (_, "loop_contract", conds); body; cond]) ->
          let (mkLoop, inBody, break_lbl, continue_lbl) = doLoop wtok conds
          let usedBreak = ref false
          let check _ = function
            | Expr.Label (_, l) ->
              if l.Name = break_lbl.Name || l.Name = continue_lbl.Name then
                usedBreak := true
              false
            | _ -> not !usedBreak // keep looking
          let body = inBody body
          let res () =
            mkLoop [ body;
                     Label (wtok, continue_lbl); 
                     If (wtok, cond, 
                          Goto (wtok, break_lbl), 
                          Expr.MkBlock [])]
          match cond with
            | Expr.BoolLiteral (_, true) ->
              body.SelfVisit check
              if !usedBreak then res ()
              else Some body
            | _ -> res ()
        
        | Macro (wtok, "for", [Macro (_, "loop_contract", conds); init; cond; incr; body]) ->
          let (mkLoop, inBody, break_lbl, continue_lbl) = doLoop wtok conds
          let loop =
            mkLoop [If (wtok, cond,
                         inBody body,
                         Goto (wtok, break_lbl));
                    Label (wtok, continue_lbl);
                    inBody incr ]
          Some (Expr.MkBlock [init; loop.Value])
        
        | Macro (token, "switch", condAndBody) -> Some (doSwitch labels condAndBody)
        | Macro (token, "break", []) ->
          match labels with
            | Some (l, _) -> Some (Goto (token, l))
            | None -> None
        | Macro (token, "continue", []) ->
          match labels with
            | Some (_, l) -> Some (Goto (token, l))
            | None -> die()
        | _ -> None
        
    // ============================================================================================================\


    let checkSpecCodeAndRemoveSpecMark decls =

      let rec isPhysicalLocation = function
        | Dot(_, _, f) when f.IsSpec -> false
        | Dot(_, ptr, _) -> isPhysicalLocation ptr
        | Index(_, ptr, _) -> isPhysicalLocation ptr
        | Ref(_, {Kind = SpecLocal|SpecParameter|OutParameter}) -> false        
        | Ref(_, {Type = Type.Ref td }) when hasBoolAttr "record" td.CustomAttr -> false
        | Deref(_, expr) -> isPhysicalLocation expr //TODO: this is actually not true - how to we guarantee that the pointer is not pointing to physical memory
        | _ -> true

      let rec checkNoWritesToPhysicalFromSpec withinSpec self = function
       | Macro(cmn, "=", [location ; expr]) when isPhysicalLocation location ->
         match exprDependsOnSpecExpr expr with
           | None when withinSpec ->
              helper.GraveWarning(cmn.Token, 9300, "assignment to physical location from specification code")
              false
           | None -> false
           | Some specField -> 
            helper.GraveWarning(cmn.Token, 9300, "assignment to physical location from specification " + specField)
            false
       | Macro(cmn, "out", [outPar]) when isPhysicalLocation outPar ->
         helper.GraveWarning(cmn.Token, 9304, "physical location passed as out parameter")
         false
       | CallMacro(_, "spec", _, args) -> List.iter (fun (e:Expr) -> e.SelfVisit(checkNoWritesToPhysicalFromSpec true)) args; false
       | _ -> true

      let rec checkAccessToSpecFields ctx self = function
        | CallMacro(_, "spec", _, _) 
        | Call(_, {IsSpec = true}, _, _) 
        | CallMacro(_, "_vcc_unwrap", _, _)
        | CallMacro(_, "_vcc_wrap", _, _)
        | CallMacro(_, "_vcc_wrap_non_owns", _, _)
        | CallMacro(_, "unclaim", _, _) -> false
        | CallMacro(_, "by_claim", _, [_; obj; ptr]) ->
          obj.SelfCtxVisit(ctx.IsPure, checkAccessToSpecFields)
          ptr.SelfCtxVisit(ctx.IsPure, checkAccessToSpecFields)
          false
        | Macro(_, "=", [tgt; src]) as assign ->
          match exprDependsOnSpecExpr tgt with
            | Some _ -> false
            | _ -> true
        | Call(_, f, _, args) ->
          let checkIfNotSpecPar (v : Variable) (expr : Expr) =
            if v.Kind <> VarKind.SpecParameter && v.Kind <> VarKind.OutParameter then expr.SelfCtxVisit(ctx.IsPure, checkAccessToSpecFields)
          List.iter2 checkIfNotSpecPar f.Parameters args
          false
        | Dot(cmn, _, f) when not ctx.IsPure && f.IsSpec ->
          helper.GraveWarning(cmn.Token, 9301, "access to specification field '" + f.Name + "' within non-specification code")
          true
        | Ref(cmn, ({Kind = SpecLocal} as v)) when not ctx.IsPure ->
          helper.GraveWarning(cmn.Token, 9301, "access to specification variable '" + v.Name + "' within non-specification code")
          true
        | Ref(cmn, ({Kind = SpecParameter|OutParameter} as v)) when not ctx.IsPure ->
          helper.GraveWarning(cmn.Token, 9301, "access to specification parameter '" + v.Name + "' within non-specification code")
          true
        | Call(cmn, ({IsSpec = true} as fn), _, args) when not ctx.IsPure ->
          helper.GraveWarning(cmn.Token, 9301, "access to specification function '" + fn.Name + "' within non-specification code")
          true
        | _ -> true
        
      let errorForSecondPhysicalAccess (expr : Expr) =
        let count = ref 0
        let rec isHeapAllocatedParOrLocal = function
          | Macro(_, "&", [Ref(_, {Kind = VarKind.Local|VarKind.Parameter|VarKind.SpecLocal|VarKind.SpecParameter|VarKind.OutParameter})]) -> true
          | Dot(_, ptr, _) -> isHeapAllocatedParOrLocal ptr
          | _ -> false
        
        let incrAndReportError token =
          incr count
          if !count > 1 then
            helper.GraveWarning(token, 9302, "more then one access to physical memory in atomic block; extra accesses might be due to bitfield operations")
       
        let countPhysicalAccesses' ctx self = function
          | Deref(_, ptr) when not ctx.IsPure && isHeapAllocatedParOrLocal ptr -> true
          | Deref(cmn, ptr) when not ctx.IsPure && isPhysicalLocation ptr -> incrAndReportError cmn.Token; true
          | CallMacro(cmn, "inlined_atomic", _, _) -> incrAndReportError cmn.Token; false
          | _ -> true
        expr.SelfCtxVisit(false, countPhysicalAccesses')
        
      let checkAtMostOnePhysicalAccessInAtomic self = function
        | Atomic(_, _, body) as _atomic ->
          errorForSecondPhysicalAccess body
          false
        | _ -> true
      
        
      let removeSpecMarker self = function
        | CallMacro(_, "spec", _, [body]) -> Some(self(body))
        | _ -> None
        
      for d in decls do
        match d with 
          | Top.FunctionDecl({IsSpec = true}) -> ()
          | _ -> [d] |> deepVisitExpressionsCtx checkAccessToSpecFields 
                 [d] |> deepVisitExpressions (checkNoWritesToPhysicalFromSpec false)
                 [d] |> deepVisitExpressions checkAtMostOnePhysicalAccessInAtomic
        
        
      decls |> deepMapExpressions removeSpecMarker

    // ============================================================================================================\
    
    helper.AddTransformer ("desugar-begin", Helper.DoNothing)
    
    helper.AddTransformer ("desugar-lambdas", Helper.Decl desugarLambdas)
    helper.AddTransformer ("desugar-ite", Helper.Decl removeLazyOps)
    helper.AddTransformer ("norm-nested-locals", Helper.Decl removeNestedLocals)
    helper.AddTransformer ("norm-primitive-globals", Helper.Decl wrapPrimitiveGlobals)
    helper.AddTransformer ("desugar-addr-deref", Helper.Decl cleanupAddrDeref)
    helper.AddTransformer ("desugar-by-claim", Helper.Decl expandByClaim)
    helper.AddTransformer ("desugar-approvers", Helper.Decl handleApprovers)
    helper.AddTransformer ("desugar-assign-ops", Helper.Expr removeAssignOps)
    helper.AddTransformer ("check-spec-code", Helper.Decl checkSpecCodeAndRemoveSpecMark)
    helper.AddTransformer ("desugar-addressable-locals", Helper.Decl heapifyAddressedLocals)
    helper.AddTransformer ("desugar-globals", Helper.Decl handleGlobals)
    helper.AddTransformer ("desugar-loops", Helper.Expr (loopAndSwitchDesugaring None))
    
    helper.AddTransformer ("desugar-end", Helper.DoNothing)