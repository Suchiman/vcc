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
 
 module AddChecks =
   
  let invariantsOf (td:TypeDecl) =
    let stripLabels = function
      | Macro(_, "labeled_invariant", [_; i]) -> i
      | i -> i
    td.Invariants |> List.map splitConjunction |> List.concat
    
  let invariantCheck (helper:Helper.Env) cond errno suffix prestate (this:Expr) =
    match this.Type with
      | Ptr (Type.Ref td) ->
        let replaceThisOld self = function 
          | Expr.Macro(c, "this", []) -> Some this
          | Expr.Old (c, Macro (_, "prestate", []), e) ->
            Some (Expr.Old (c, prestate, self e))
          | _ -> None

        let fmt = "invariant({0}) of " + td.Name + " " + suffix
        let mkAssertFromInvariant (expr : Expr) =
          let primaryToken = if this.Token = bogusEC.Token then expr.Token else this.Token
          Expr.Macro (afmter errno fmt primaryToken (Some(new ForwardingToken(expr.Token, fun () -> "location of the invariant") :> Token)) [expr], "inv_check", [expr])
        [ for inv in invariantsOf td do
            if cond inv then yield mkAssertFromInvariant (inv.SelfMap replaceThisOld) 
            else yield! [] ]
      | Ptr (TypeVar _)
      | ObjectT
      | Array (_, _) -> 
        let nowstate = Expr.Macro ({ bogusEC with Type = Type.MathState }, "_vcc_current_state", [])
        [Expr.Macro (afmte errno ("invariant of {0} " + suffix) [this], "_vcc_inv2", [prestate; nowstate; this])]
      | t ->
        helper.Panic ("wrong ref (" + this.ToString() +  ":" + t.ToString() + 
                      ") supplied to invariantCheck(...), expecting something of pointer-to-struct type")
    
 
  let isOnUnwrap = function
    | BoolOp (_, "==>", BoolOp (_, "&&", 
                                COld (_, CallMacro (_, "_vcc_closed", _, [_; This])),
                                Prim (_, Op ("!", _), [CallMacro (_, "_vcc_closed", _, [_; This])])), _) -> true
    | _ -> false
  
  let saveAndCheckInvariant helper cond errno suffix this =
    let this = ignoreEffects this
    let prestate = getTmp helper "prestate" Type.MathState VarKind.SpecLocal
    let nowstate = Expr.Macro ({ bogusEC with Type = Type.MathState }, "_vcc_current_state", [])
    let saveState = [VarDecl (bogusEC, prestate); VarWrite (bogusEC, [prestate], nowstate)]
    let check = invariantCheck helper cond errno suffix (mkRef prestate) this
    (saveState, List.map Expr.MkAssert check)  
    
  
  let init (helper:Helper.Env) =
  
  
    // ============================================================================================================
    
    let expectUnreach = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_expect_unreachable", []))
    
    let bogusSet = { bogusEC with Type = Type.PtrSet }
    let bogusState = { bogusEC with Type = Type.MathState }
    let pureEx (e:Expr) = Pure (e.Common, e)
    let extractKeeps uncond checks =
      let updateFor = function
        | Macro (_, "keeps", [_; obj]) ->
          uncond obj
        | _ -> []
      let rec addToOwns acc = function
        | Assert (_, e) -> addToOwns acc e
        | Macro (_, "inv_check", [e]) -> addToOwns acc e
        | Prim (_, Op (("<==>"|"=="), _), [cond; keeps]) ->
          let body = splitConjunction keeps |> List.map updateFor |> List.concat
          if body.IsEmpty then acc
          else
            If (bogusEC, pureEx cond, Expr.MkBlock (body @ [expectUnreach]), Expr.MkBlock [expectUnreach]) :: acc
        | e -> updateFor e @ acc
      List.fold addToOwns [] checks
    
    // TODO this could use some refactoring
    /// Handle things like _vcc_unwrap(...), _vcc_set_owns(...)
        (* It seems there is not much to handle here, the only special handling is adding
           writes checks, and those are handled by the declarations of those functions
           in vcc.h.
        *)
    let handleSpecialCalls self = function
      | Stmt (stmtComm, CallMacro (callComm, (("_vcc_wrap"|"_vcc_wrap_non_owns") as wrapName), _, [this])) as expr ->
        match this.Type with
          | Ptr (Type.Ref td) when staticOwns td ->
            let tmpowns = getTmp helper "owns" Type.PtrSet VarKind.SpecLocal
            let curstate = getTmp helper "staticWrapState" Type.MathState VarKind.SpecLocal
            
            let (save, checks) = saveAndCheckInvariant helper (fun e -> not (isOnUnwrap e)) 8014 "fails on wrap" this 
            
            let myAssert id msg name p =
              let p = ignoreEffects p
              Expr.MkAssert (Expr.Macro (afmte id msg [this; p], name, [p]))
            
            let updateFor obj =
              let single = Macro (bogusSet, "_vcc_set_singleton", [obj])
              
              [myAssert 8018 "'{1}' is not wrapped before wrapping '{0}' (its owner)" "_vcc_wrapped" obj;
               myAssert 8019 "'{1}' is not writable before wrapping '{0}' (its owner)" "writes_check" obj;
               VarWrite (bogusEC, [tmpowns], 
                pureEx (Macro (bogusSet, "_vcc_set_union", [mkRef tmpowns; single])));
               VarWrite (bogusEC, [curstate], 
                pureEx (Macro (bogusState, "_vcc_take_over", [mkRef curstate; this; obj])))]
            let addOwnees = extractKeeps updateFor checks
            let ownSave =
                  [VarDecl (bogusEC, tmpowns); 
                   VarDecl (bogusEC, curstate); 
                   VarWrite (bogusEC, [tmpowns], Macro (bogusSet, "_vcc_set_empty", []));
                   VarWrite (bogusEC, [curstate], Macro (bogusState, "_vcc_current_state", [])); 
                   ]
            let pre = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_pre_static_wrap", []))
            let assume = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_full_stop", []))
            let staticWrap = 
              if wrapName.Contains "non_owns" then
                Macro (callComm, "_vcc_static_wrap_non_owns", [pureEx this; mkRef curstate])
              else
                Macro (callComm, "_vcc_static_wrap", [pureEx this; mkRef curstate; mkRef tmpowns])
            let checkWr = propAssert 8020 "'{0}' is not writable before wrapping it" "writes_check" this
            Some (Expr.MkBlock (save @ ownSave @ [checkWr] @ addOwnees @ [pre; staticWrap] @ checks @ [assume]))
               
          | _ ->
            match this.Type with
              | ObjectT
              | Ptr (Type.Ref _) 
              | Ptr (TypeVar _)
              | Array (_, _) -> 
                let (save, check) = saveAndCheckInvariant helper (fun e -> not (isOnUnwrap e)) 8014 "fails on wrap" this
                let pre = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_pre_wrap", []))
                let assume = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_full_stop", []))
                let expr =
                  match expr with
                    | Stmt (stmtComm, Macro (callComm, "_vcc_wrap_non_owns", args)) ->
                      Stmt (stmtComm, Macro (callComm, "_vcc_wrap", args)) 
                    | _ -> expr
                Some (Expr.MkBlock (save @ [pre; expr] @ check @ [assume]))
              | t -> 
                helper.Error (expr.Token, 9621, "call to wrap(...) with an improper type: " + t.ToString(), None)
                None
      | Stmt (_, CallMacro (callComm, "_vcc_unwrap", _, [this])) as expr ->
        match this.Type with
          | Ptr (Type.Ref td) when staticOwns td ->
            let (save, check) = saveAndCheckInvariant helper isOnUnwrap 8015 "fails on unwrap" this
            let checkWrap = propAssert 8016 "'{0}' is not wrapped before unwrap" "_vcc_wrapped" this
            let checkWr = propAssert 8021 "'{0}' is not writable before unwrapping it" "writes_check" this
            let assumeInv = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_inv", [ignoreEffects this]))
            let assume = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_full_stop", []))
            let pre = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_pre_static_unwrap", []))
            
            match saveAndCheckInvariant helper (fun e -> not (isOnUnwrap e)) 0 "OOPS" this with
              | (VarDecl (_, curstate) :: _) as save2, props -> 
                let now = Macro (bogusState, "_vcc_current_state", [])
                let updateFor obj =
                  [VarWrite (bogusEC, [curstate], 
                    pureEx (Macro (bogusState, "_vcc_release", [now; mkRef curstate; this; obj])));
                   Expr.MkAssume (pureEx (Macro (boolBogusEC(), "_vcc_typed", [obj])))]
                let addOwnees = extractKeeps updateFor props
                let staticUnwrap = Macro (callComm, "_vcc_static_unwrap", [pureEx this; mkRef curstate])
                Some (Expr.MkBlock (save @ save2 @ [checkWrap; checkWr; assumeInv] @ addOwnees @ [pre; staticUnwrap] @ check @ [assume]))
              | _ -> die()
            
          | _ -> 
            match this.Type with
              | ObjectT
              | Ptr (Type.Ref _) 
              | Ptr (TypeVar _)
              | Array (_, _) -> 
                let (save, check) = saveAndCheckInvariant helper isOnUnwrap 8015 "fails on unwrap" this
                let checkWrap = propAssert 8016 "'{0}' is not wrapped before unwrap" "_vcc_wrapped" this
                let assumeInv = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_inv", [ignoreEffects this]))
                let assume = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_full_stop", []))
                let pre = Expr.MkAssume (Macro (boolBogusEC(), "_vcc_pre_unwrap", []))
                Some (Expr.MkBlock (save @ [checkWrap; assumeInv; pre; expr] @ check @ [assume]))
              | t -> 
                helper.Error (expr.Token, 9621, "call to unwrap(...) with an improper type: " + t.ToString(), None)
                None
      | _ -> None

    // ============================================================================================================
    
    let notInTestsuite expr =
      if helper.Options.RunTestSuite then Expr.MkBlock []
      else expr

    /// Add checks for writability/readability of memory
    let addMemoryChecks ctx self = function
      | MemoryWrite (c, p, expr) ->
        if expr.Type.IsComposite then helper.Oops (expr.Token, "non primitive type in memory write")
        let istyped = propAssert 8506 "{0} is typed" "_vcc_typed2" p
        let wrassert = propAssert 8507 "{0} is writable" "prim_writes_check" p
        Some (Expr.MkBlock [notInTestsuite istyped; wrassert; MemoryWrite (c, self p, self expr)])
      
      | Macro (ec, "by_claim", ([c; obj; _] as args)) when not ctx.IsPure ->
        let vc = Expr.MkAssert (Macro (afmte 8508 "{1} is a valid claim (in by_claim({1}, {0}))" [obj; c], "_vcc_valid_claim", [c]))
        let indom = Expr.MkAssert (Macro (afmte 8509 "object {0} is claimed by {1} (in by_claim({1}, {0}))" [obj; c], "_vcc_in_claim_domain", [obj; c]))
        Some (Expr.MkBlock [indom; vc; Macro (ec, "by_claim", List.map self args)])
        
      | Call (c, ({ Name = "_vcc_from_bytes"|"_vcc_to_bytes"} as fn), _, args) as call ->
        let obj = args.Head
        let w = Macro ({ obj.Common with Type = Type.PtrSet }, "_vcc_extent", [obj])
        let prop = afmte 8510 "{1} is writable in call to {0}" [call; w]
        Some (Expr.MkBlock [Expr.MkAssert (Expr.Macro (prop, "writes_check", [w])); 
                            Macro (c, fn.Name, List.map self args)])
      
      | Call (c, f, targs, args) as call when not ctx.IsPure ->
        let f' = f.Specialize(targs, false)
        let wrasserts =
          match f'.Writes with
            | [] -> []
            | _ ->
              let subst = new Dict<_,_>()
              let rec loop = function
                | (p :: pp, v :: vv) ->
                  subst.Add (p, v)
                  loop (pp, vv)
                | ([], _) -> () // for varargs functions
                | _ -> helper.Die()
              loop (f'.InParameters, args)             

              [for w in f'.Writes ->
                let w' = w.Subst (subst)
                let prop = afmte 8510 "{1} is writable in call to {0}" [call; w']
                let ch =
                  match w'.Type with
                    | Ptr t when t <> Void && not t.IsComposite -> "prim_writes_check"
                    | _ -> "writes_check"
                Expr.MkAssert (Expr.Macro (prop, ch, [w']))]
                                        
        let check_req acc = function
          | Call (_, { Name = "_vcc_is_atomic_obj" }, _, [IntLiteral (_, n)]) ->
            let arg = List.nth args (Math.BigInt.ToInt32(n))
            Expr.MkAssert (Macro (afmte 8532 "{1} is atomically updated in call to {0}" [call; arg],
                                  "is_atomic_obj", [arg])) :: acc
          | _ -> acc
        match List.fold check_req wrasserts f'.Requires with
          | [] -> None
          | checks -> Some (Expr.MkBlock (checks @ [Call (c, f, targs, List.map self args)]))          
        
      | Deref (c, p) when not ctx.IsPure ->
        let rd = Expr.MkAssert (Macro ({c with Type = Bool}, "reads_check_normal", [ignoreEffects p]))
        Some (Expr.MkBlock [rd; Deref (c, self p)])
        
      | Macro(c, "_vcc_vs_ctor", [p]) as m when not ctx.IsPure ->
        let rec deepReadsCheck (e : Expr) =
          match e.Type with
            | Type.Ptr(Type.Ref(td)) -> 
              let locAssert = Expr.MkAssert(Expr.Macro({e.Common with Type = Bool}, "reads_check_normal", [ignoreEffects e]))
              let fldAssert (fld:Field) = 
                let ec = { forwardingToken e.Token None (fun() -> e.Token.Value + "." + fld.Name) with Type = Ptr(fld.Type) }
                deepReadsCheck (Expr.Dot(ec, e, fld))
              locAssert :: (td.Fields |> List.map fldAssert |> List.concat)
            | _ -> []
        Some (Expr.MkBlock(deepReadsCheck p @ [Macro(c, "_vcc_vs_ctor", [self p])]))
        
      | _ -> None
    
    // ============================================================================================================
    
    let checkableOps = [ "+"; "-"; "*"; "/"; "%" ]
    
    let inRangeDiv ec t (args: Expr list) =
      let suff = intSuffix t
      Expr.Macro (ec, "in_range_div_" + suff, args)

    let addShiftChecks ctx self = function
      | Prim(c, (Op((">>"|"<<"), _) as op), [arg1; arg2]) when not ctx.IsPure ->
        let arg1 = self arg1
        let arg2 = self arg2
        let arg2' = ignoreEffects arg2 //8529
        let comm = afmte 8519 "{0} in admissible range (in shift)" [arg2']
        let inrange = Expr.Prim(comm, Op("&&", Processed), [ Expr.Prim({bogusEC with Type = Bool}, Op("<=", Processed), [mkInt 0; arg2']);
                                                             Expr.Prim({bogusEC with Type = Bool}, Op("<", Processed), [arg2'; mkInt (c.Type.SizeOf * 8)]) ])
        addStmtsOpt [Expr.MkAssert(inrange)] (Prim(c, op, [arg1; arg2]))
      | _ -> None
                                                                    

    let isFloatingPoint = function | Type.Primitive _ -> true | _ -> false

    let addDivByZeroChecks ctx self = function
      | Prim(c, (Op(("/"|"%"), _) as op), [arg1; arg2]) when not ctx.IsPure && not (isFloatingPoint c.Type) ->
        let arg1 = self arg1
        let arg2 = self arg2
        let arg2' = ignoreEffects arg2
        let comm = afmte 8519 "{0} != 0 (in division by zero)" [arg2']
        let nonzero = Expr.Prim(comm, Op("!=", Processed), arg2' :: [Expr.IntLiteral(comm, zero)])
        addStmtsOpt [Expr.MkAssert(nonzero)] (Prim(c, op, [arg1; arg2]))
      | _ -> None

    /// For a(checked+)b add assertion $check.add(a,b).
    let addOverflowChecks ctx self = function
      | Cast (c, Checked, e') when not ctx.IsPure ->
        let types = (e'.Type, c.Type)
        let newe = Cast (c, Processed, self e')
        if Type.ConversionIsLossless types then Some newe
        else
          let comm = afmte 8518 ("{0} fits range of " + c.Type.ToString()) [e']
          addStmtsOpt [Expr.MkAssert (inRange comm (ignoreEffects newe))] newe      
                       
      | Prim (c, Op(opName, Checked) , args) when c.Type = Type.MathInteger ->
        Some(Prim(c, Op(opName, Processed), List.map self args))
      | Prim (c, (Op(opName, Checked) as op), args) as e when not ctx.IsPure ->
        let args = List.map self args
        let newop = Prim (c, Op(opName, Processed), args)
        let checks =
          match opName with
            | ("/"|"%") ->
              match args with
              | [arg1; arg2] ->
                let arg1 = ignoreEffects arg1
                let arg2 = ignoreEffects arg2
                let overflow =
                  if e.Type.IsSignedInteger then // div and mod overflow iff parameters are minint and -1 or dividing by zero)
                    let comm = afmte 8003 "{0} might overflow (in div/mod case)" [e]
                    [inRangeDiv comm e.Type [arg1;arg2]]
                  else []
                overflow
              | _ -> failwith "binary operation expected"
            | _  -> 
              let comm = afmte 8004 "{0} might overflow" [e]
              [inRange comm (ignoreEffects newop)]
        addStmtsOpt (List.map Expr.MkAssert checks) newop
      
      | _ -> None

    let unchecked (newop:Expr) =
      match newop.Type with
        | Type.Integer k ->
          Expr.Macro (newop.Common, "unchecked_" + Type.IntSuffix k, [newop])              
        | _ -> failwith ("non-integer primitive operation " + newop.ToString())
    
    let stripRemainingChecked self = function
      | Prim (c, Op(opName, st), args) as e when st <> Processed ->
        let newop = self (Expr.Prim (c, Op(opName, Processed), args))
        let newop =
          if st = Unchecked && _list_mem opName checkableOps then
            unchecked newop
          else newop
        Some newop
      | Cast (c, st, e') when st <> Processed ->
        let newop = self (Cast (c, Processed, e'))
        let newop =
          if st = Unchecked && not (Type.ConversionIsLossless (e'.Type, c.Type)) then
            unchecked newop
          else newop
        Some newop            
      | _ -> None
      

    // ============================================================================================================

    let reportCheckedOpsInBvLemma self = function
      | Expr.Assert (_, Expr.Macro (_, "_vcc_bv_lemma", [e])) -> 
        let reportCheckedOpsInBvLemma' self = function 
          | Expr.Prim (_, Op (("+"|"-"|"*"|"/"|"%"), Checked), _) as e ->
            helper.Error (e.Token, 9659, "operators in bv_lemma(...) need to be unchecked (expression: " + e.Token.Value + ")")
            None
          | _ -> None
        let _ = e.Map(false, reportCheckedOpsInBvLemma')
        None
      | _ -> None

    // ============================================================================================================
    
    helper.AddTransformer ("check-begin", Helper.DoNothing)
    
    helper.AddTransformer ("check-report-checked-in-bv-lemma", Helper.Expr reportCheckedOpsInBvLemma)
    helper.AddTransformer ("check-special-calls", Helper.Expr handleSpecialCalls)
    helper.AddTransformer ("check-memory-access", Helper.ExprCtx addMemoryChecks)
    helper.AddTransformer ("check-overflows", Helper.ExprCtx addOverflowChecks)
    helper.AddTransformer ("check-div-by-zero", Helper.ExprCtx addDivByZeroChecks)
    helper.AddTransformer ("check-shift-bits-in-range", Helper.ExprCtx addShiftChecks)
    helper.AddTransformer ("check-remove-checked", Helper.Expr stripRemainingChecked)
    
    helper.AddTransformer ("check-end", Helper.DoNothing)
