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
 
 module TransFinalize =

  let init (helper:Helper.Env) =
    // ============================================================================================================

    let addRangeAssumptions =
      let rangeAssumptions (parm : Variable) =
        match parm.Type with
          | Ptr _
          | Integer _ ->
            [inRange (boolBogusEC ()) (mkRef parm)]
          | _ ->
            []                    

      let mkFreeEnsures (expr:Expr) = Expr.Macro(expr.Common, "free_ensures", [expr])

      let quantRange self = function
        | Quant (c, q) ->
          let lst = List.concat (List.map rangeAssumptions q.Variables)
          let condition = Some (multiAnd (Option.toList q.Condition @ lst))
          Some (Quant (c, { q with Condition = condition; Body = self q.Body }))
        | _ -> None        

      let rangeAssume (parm:Variable) =
        List.map Expr.MkAssume (rangeAssumptions parm)
      let aux (h:Function) =
        if not (_list_mem IsAdmissibilityCheck h.CustomAttr) then
          h.Body <- Option.map (addStmts (List.concat (List.map rangeAssume h.InParameters))) h.Body
          h.Ensures <- List.map mkFreeEnsures (List.concat (List.map rangeAssumptions h.OutParameters)) @ h.Ensures
        if h.RetType._IsInteger || h.RetType._IsPtr then
          let ec = { boolBogusEC() with Token = h.Token }
          h.Ensures <- (mkFreeEnsures (inRange ec (Expr.Result({bogusEC with Type = h.RetType})))) :: h.Ensures
        h
      mapFunctions aux >> deepMapExpressions quantRange
      
    // ============================================================================================================
    
    let addReturnConversions decls =
      for d in decls do
        match d with
          // we need the precise type information so the type can be added or stripped in the translator
          | Top.FunctionDecl ({ Body = Some b; RetType = (Ptr _|ObjectT) } as f) ->
            let repl _ = function
              | Return (c, Some e) ->
                Some (Return (c, Some (Cast ({ e.Common with Type = f.RetType }, Processed, e))))
              | _ -> None            
            f.Body <- Some (b.SelfMap repl)
          | _ -> ()
      decls
    
    
    // ============================================================================================================
     
    /// Change if (cond) tmp = e1; else tmp = e2; into tmp = cond?e1:e2; (as there are no
    /// assertions associated with e1 and e2).
    let foldIteBack self = function
      | If (c, cond, VarWrite (_, tmp, e1), VarWrite (_, tmp', e2)) when tmp = tmp' ->
        Some (VarWrite (c, tmp, self (Expr.Macro (e1.Common, "ite", [cond; e1; e2]))))
      | _ -> None
      
    // ============================================================================================================
    
    let introduceAndOrs self = function
      | Expr.Macro (c, "ite", [e1; e2; EFalse]) -> Some (self (Expr.Prim (c, Op("&&", Processed), [e1; e2])))
      | Expr.Macro (c, "ite", [e1; ETrue; e2]) -> Some (self (Expr.Prim (c, Op("||", Processed), [e1; e2])))
      | Expr.Macro (c, "ite", [e1; e2; ETrue]) -> Some (self (Expr.Prim (c, Op("==>", Processed), [e1; e2])))
      
      | Expr.Prim (_, Op("||", _), [e1; EFalse])
      | Expr.Prim (_, Op("||", _), [EFalse; e1])
      | Expr.Prim (_, Op("&&", _), [e1; ETrue])
      | Expr.Prim (_, Op("&&", _), [ETrue; e1]) -> Some (self e1)
      
      | x -> None
      
    // ============================================================================================================
    

    let addBoolConversions decls =
      let rec f (x:Expr) =
        let self (e:Expr) = e.SelfMap doExpr
        convertToBool self x
        
      and fs = List.map f
      
      and doExpr self = function
        | Expr.Ref _
        | Prim _
        | Expr.Call _
        | IntLiteral _
        | BoolLiteral _
        | Deref _
        | Dot _
        | Index _
        | Cast _
        | Result _
        | Old _
        | Macro _
        | VarWrite _
        | MemoryWrite _
        | Goto _
        | Label _
        | Block _
        | Return _
        | VarDecl _
        | Stmt _
        | Pure _
        | UserData _
        | SizeOf _
        | Comment _ -> None
        
        | Quant (c, q) -> Some (Quant (c, { q with Body = f q.Body; Condition = Option.map f q.Condition }))
        | If (c, cond, e1, e2) -> Some (If (c, f cond, self e1, self e2))
        | Loop (c, invs, writes, body) -> Some (Loop (c, fs invs, List.map self writes, self body))
        | Assert (c, cond) -> Some (Assert (c, f cond))
        | Assume (c, cond) -> Some (Assume (c, f cond))
        | Atomic (c, objs, body) -> Some (Atomic (c, List.map self objs, self body))

      let aux d =
        match d with
          | Top.Axiom e -> Top.Axiom (f e)
          | Top.GeneratedAxiom(e, origin) -> Top.GeneratedAxiom(f e, origin)
          | Top.FunctionDecl x ->
            x.Requires <- fs x.Requires
            x.Ensures <- fs x.Ensures
            x.Body <- Option.map (fun (e:Expr) -> e.SelfMap doExpr) x.Body
            d          
          | Top.TypeDecl td ->
            td.Invariants <- fs td.Invariants; d
          | Top.Global _ -> d
          
      // run handleConversions again to get rid of the junk we have generated
      decls |> List.map aux |> Normalizer.handleConversions helper            
    
    // ============================================================================================================
  
    let removeTrivialBitvectorOperations self = function
      | Expr.Macro (_, ("bv_extract_signed" | "bv_extract_unsigned"), 
                    [e; IntLiteral(_, sz); IntLiteral(_, z); IntLiteral(_, sz') ]) when z = zero && sz = sz' -> Some(self e)
      | Expr.Macro (_, "bv_update",
                    [_; IntLiteral(_, sz); IntLiteral(_, z); IntLiteral(_, sz'); e ]) when z = zero && sz = sz' -> Some(self e)
      | _ -> None
    
    let theKeepsWarning = function
      | TypeDecl td as decl when staticOwns td ->
        let rec check top self = function
          | Macro(_, "labeled_invariant", [_; i]) when top ->
            i.SelfVisit (check true)
            false
          | Prim (_, Op (("<==>"|"=="), _), [cond; keeps]) as e when top ->
            cond.SelfVisit (check false) 
            cond.SelfVisit (check true)
            false
          | BoolOp (_, "&&", e1, e2) as e when top -> self e1; self e2; false
          | Macro (_, "keeps", _) as e ->
            if not top then 
              helper.Warning (e.Token, 9110, "keeps(...) (or set_in(..., owns(this))) is not allowed here; " +
                                             "annotate the type '" + td.Name + "' with vcc(dynamic_owns)")
            true
          // 9111 for | Macro (_, "_vcc_set_in", _) as e ?
          | e when top ->
            e.SelfVisit (check false)
            false
          | _ -> true
        deepVisitExpressions (check true) [decl]
        decl
            
      | decl -> decl

    // ============================================================================================================
   
    let errorForMissingDynamicOwns decls =

      let checkAndWarn self = function
        | CallMacro(_, "_vcc_owns", _, [_; Cast(_,_,expr)]) 
        | CallMacro(_, "_vcc_owns", _, [_; expr]) as owns -> 
          match expr.Type with
            | Ptr(Type.Ref(td)) when staticOwns td ->
              helper.Error(owns.Token, 9662, "Explicit reference to owns-set of type '" + td.Name + "', which is static. Use keeps(...) or mark '" + td.Name + "' with vcc(dynamic_owns).")
              true
            | _ -> true
        | _ -> true
        
      forEachInvariant checkAndWarn decls
      decls

    // ============================================================================================================          

    let assignExpressionStmts self = 
      let dummyId = ref 0
      let ignoreType = function
        | Type.Void
        | Type.Ref({Kind = TypeKind.MathType; Name = "$$bogus$$" }) -> true
        | _ -> false
      let splitLast = 
        let rec loop acc = function
          | [] -> helper.Die()
          | [x] -> x, List.rev acc
          | x::xs -> loop (x::acc) xs
        loop []
      let rec stmtalize = function
        | Block(ec, []) -> [Block(ec, [])]
        | Block(ec, stmts) ->
          let last, stmts' = splitLast stmts
          [Block({ec with Type = Type.Void}, stmts' @ stmtalize last)]
        | expr when ignoreType expr.Type -> [expr]
        | expr ->
          let ecVoid = {expr.Common with Type = Type.Void}
          let dummy = getTmp helper ("stmtexpr" + ((!dummyId).ToString())) expr.Type VarKind.Local
          let decl = VarDecl(ecVoid, dummy)
          let assign = VarWrite(ecVoid, [dummy], expr)
          incr dummyId
          [decl; assign]
      function
        | If(ec, cond, e1, e2) -> Some(If(ec, self cond, Expr.MkBlock(stmtalize (self e1)), Expr.MkBlock(stmtalize (self e2))))
        | Loop(ec, inv, writes, stmts) -> Some(Loop(ec, List.map self inv, List.map self writes, Expr.MkBlock(stmtalize (self stmts))))
        | Stmt(ec, expr) when not (ignoreType expr.Type) -> Some(Expr.MkBlock(stmtalize (self expr)))
        | Atomic(ec, args, expr) -> Some(Atomic(ec, List.map self args, Expr.MkBlock(stmtalize (self expr))))
        | Block(ec, []) -> None
        | Block(ec, stmts) ->
          let last, stmts' = splitLast stmts
          Some(Block(ec, (stmts' |> List.map self |> List.map stmtalize |> List.concat) @ [ self last ]))
          
        | _ -> None
        

    // ============================================================================================================

    let flattenBlocks self = function
      | Expr.Block(ec, stmts) ->
        let rec loop acc = function
          | [] -> List.rev acc
          | Expr.Block(_, nested) :: stmts -> loop acc (nested @ stmts)
          | Expr.Comment(_, comment) :: stmts when comment.StartsWith("__vcc__") -> loop acc stmts
          | stmt :: stmts -> loop (self stmt :: acc) stmts
        Some(Expr.Block(ec, loop [] stmts))
      | _ -> None
        
    // ============================================================================================================

    let handleStackAllocations =
      
      let insertAfterVarDecls toInsert = function
        | Expr.Block(ec, stmts) -> 
          let rec loop decls = function
            | (VarDecl _ as vd) :: stmts-> loop (vd::decls) stmts
            | stmts -> List.rev decls @ toInsert @ stmts
          Expr.Block(ec, loop [] stmts)
        | e -> Expr.MkBlock(toInsert @ [e])
            
      let handleFunction (f : Function) =
        match f.Body with
          | None -> f
          | Some body ->
            let allocations = ref []         
            let handleExpr self = function
              | VarWrite(ec, [v], CallMacro(_, "_vcc_stack_alloc", _, _)) as vw -> 
                let vName = if v.Name.StartsWith "addr." then v.Name.Substring(5) else v.Name
                let ec' = {forwardingToken (ec.Token) None (fun () -> "stack_free(&" + vName + ")") with Type = Void }
                let vRef = Expr.Ref({forwardingToken (ec.Token) None (fun () -> "&" + vName) with Type = v.Type} , v)
                let free = Expr.Call(ec', internalFunction helper "stack_free", [], [Expr.Macro(ec', "stackframe", []); vRef])
                let freeAtCleanup = Expr.Macro(ec, "function_cleanup", [Expr.Stmt(ec, free)])
                allocations := vw :: !allocations
                Some(freeAtCleanup)
              | _ -> None
            let body' = body.SelfMap(handleExpr) |> insertAfterVarDecls (List.rev !allocations)
            f.Body <- Some(body')
            f
            
      mapFunctions handleFunction
            
    // ============================================================================================================
    
    let errorForOldInOneStateContext decls =
    
      let reportErrorForOld self = function
        | Expr.Old(ec, Expr.Macro(_, "prestate", []), expr) ->
          helper.Error(ec.Token, 9697, "old(...) is allowed only in two-state contexts")
          false
        | CallMacro(ec, n, _, args) ->
          match Simplifier.alwaysPureCalls.TryGetValue(n) with
            | true, signature when signature.Contains("s") -> 
              helper.Error(ec.Token, 9697, "calling '" + n + "' is allowed only in two-state contexts")
              false
            | _ -> true
        | _ -> true
            
      let checkFunction (fn:Function) =
        List.iter (fun (e : Expr) -> e.SelfVisit(reportErrorForOld)) 
                  (fn.Requires @ (if fn.IsPure then fn.Ensures else []) @ fn.Reads @ fn.Writes) 
        
      for d in decls do
        match d with
          | Top.FunctionDecl(fn) -> checkFunction fn
          | _ -> ()
            
      decls
    
    // ============================================================================================================

    let errorForStateWriteInPureContext decls = 
    
      let reportErrorForStateWriteInPureContext ctx self = function
        | CallMacro(ec, ("_vcc_alloc"|"_vcc_spec_alloc"|"_vcc_spec_alloc_array"),_ ,_) when ctx.IsPure -> 
          helper.Error(ec.Token, 9703, "Memory allocation in pure context is not allowed."); false
        | MemoryWrite(ec, loc, _) when ctx.IsPure -> 
          helper.Error(ec.Token, 9703, "Writing memory location '" + loc.Token.Value + "' in pure context is not allowed."); false
        | Call(ec, fn, _, _) when ctx.IsPure && fn.Writes.Length > 0 -> 
          helper.Error(ec.Token, 9703, "Calling function '" + fn.Name + "' with non-empty writes clause in pure context is not allowed."); false
        | _ -> true
    
      let checkFunction (fn:Function) = 
        List.iter (fun (e:Expr) -> e.SelfCtxVisit(true, reportErrorForStateWriteInPureContext)) (fn.Requires @ fn.Ensures @ fn.Reads @ fn.Writes)
        Option.iter (fun (e:Expr) -> e.SelfCtxVisit(fn.IsPure, reportErrorForStateWriteInPureContext)) fn.Body

      for d in decls do
        match d with 
          | Top.FunctionDecl(fn) -> checkFunction fn
          | _ -> ()
    
      decls

    // ============================================================================================================

    let rec flattenOld _ = function
      | Old(ec, (Expr.Macro(_, "prestate", []) as ps), expr) ->
        let removeOldForPrestate self = function
          | Old(_, Expr.Macro(_, "prestate", []), expr)  -> Some(self expr)
          | Old(_, _, _) as o -> Some(o.SelfMap(flattenOld))
          | _ -> None
        Some(Old(ec, ps, expr.SelfMap(removeOldForPrestate)))
      | _ -> None
    
    // ============================================================================================================

    let insertTypeArgumentForWrapUnwrap _ = 
      let typeOf (expr:Expr) = Expr.Macro({expr.Common with Type = Type.TypeIdT}, "_vcc_typeof", [expr])
      function
        | CallMacro(ec, ("_vcc_wrap"|"_vcc_unwrap"|"_vcc_deep_unwrap" as name), _, [e]) -> Some(Macro(ec, name, [e; typeOf e]))
        | _ -> None
    
    // ============================================================================================================
    
    let errorForWhenClaimedOutsideOfClaim decls =

      let errorForWhenClaimedOutsideOfClaim' _ = function
        | Macro(_, ("claim" | "_vcc_claims" | "upgrade_claim"), body) -> false
        | CallMacro(ec, "_vcc_when_claimed", _, _) ->
          helper.Error(ec.Token, 9708, "'when_claimed' cannot be used outside of a claim.")
          false
        | _ -> true

      deepVisitExpressions errorForWhenClaimedOutsideOfClaim' decls
      decls

    // ============================================================================================================

    let errorForInvWithNegativPolarity decls =
    
      // polarity: -1, 1, and 0 for unknown 
      // once we are 'unknown', only search for invariants and report errors
      let rec checkPolarity polarity _ = function
        | CallMacro(ec, ("_vcc_inv"|"_vcc_inv2"), _, _) ->
          if (polarity <= 0) then
            let polarityStatus = if polarity = 0 then "unknown" else "negative"
            helper.Error(ec.Token, 9712, "Use of 'inv(...)' or 'inv2(...)' with " + polarityStatus + " polarity.")
          true
        | _ when polarity = 0 -> true // stick on unknown
        | Macro(_, "labeled_invariant", _)
        | Prim(_, Op(("||"|"&&"), _), _)
        | Quant _
        | Old _ -> true // these do not change polarity
        | Prim(_, Op("!", _), [e]) -> e.SelfVisit (checkPolarity (-polarity)); false
        | Prim(_, Op("<==", _), [e2; e1])
        | Prim(_, Op("==>", _), [e1; e2]) -> e1.SelfVisit (checkPolarity (-polarity)); e2.SelfVisit (checkPolarity polarity); false
        | Macro(_, "ite", [cond; e1; e2]) -> // (cond ==> e1) && (!cond ==> e2)
          cond.SelfVisit (checkPolarity 0); e1.SelfVisit (checkPolarity polarity); e2.SelfVisit (checkPolarity polarity); false
        | e -> e.SelfVisit (checkPolarity 0); false // AST node with unknown effect on polarity, switch to unknown polarity
        
      for d in decls do
        match d with 
          | Top.TypeDecl(td) -> List.iter (fun (e:Expr) -> e.SelfVisit(checkPolarity 1)) td.Invariants
          | Top.Axiom(e)
          | Top.GeneratedAxiom(e,_) -> e.SelfVisit(checkPolarity 1)
          | _ -> ()
          
      decls
    
    // ============================================================================================================
    
    helper.AddTransformer ("final-begin", Helper.DoNothing)
    
    helper.AddTransformer ("final-range-assumptions", Helper.Decl addRangeAssumptions)
    helper.AddTransformer ("final-return-conversions", Helper.Decl addReturnConversions)
    helper.AddTransformer ("final-flatten-blocks", Helper.Expr flattenBlocks)
    helper.AddTransformer ("final-free-stack", Helper.Decl handleStackAllocations)
    helper.AddTransformer ("final-stmt-expressions", Helper.Expr assignExpressionStmts)
    helper.AddTransformer ("final-linearize", Helper.Decl (ToCoreC.linearizeDecls helper))
    helper.AddTransformer ("final-keeps-warning", Helper.Decl (List.map theKeepsWarning))
    helper.AddTransformer ("final-dynamic-owns", Helper.Decl errorForMissingDynamicOwns)
    helper.AddTransformer ("final-error-old", Helper.Decl errorForOldInOneStateContext)
    helper.AddTransformer ("final-error-pure", Helper.Decl errorForStateWriteInPureContext)
    helper.AddTransformer ("final-error-when-claimed", Helper.Decl errorForWhenClaimedOutsideOfClaim)
    helper.AddTransformer ("final-error-inv-polarity", Helper.Decl errorForInvWithNegativPolarity)
    helper.AddTransformer ("final-before-cleanup", Helper.DoNothing)
    // reads check goes here
    
    helper.AddTransformer ("final-fold-ITE", Helper.Expr foldIteBack)
    helper.AddTransformer ("final-ITE-to-logical", Helper.Expr introduceAndOrs)
    helper.AddTransformer ("final-bool-conversions", Helper.Decl addBoolConversions)
    helper.AddTransformer ("final-bv-cleanup", Helper.Expr removeTrivialBitvectorOperations)
    helper.AddTransformer ("final-flatten-old", Helper.Expr flattenOld)
    helper.AddTransformer ("final-insert-type-arguments", Helper.Expr insertTypeArgumentForWrapUnwrap)
    helper.AddTransformer ("final-insert-state-arguments", Helper.Expr (ToCoreC.handlePureCalls helper))
    
    helper.AddTransformer ("final-end", Helper.DoNothing)
