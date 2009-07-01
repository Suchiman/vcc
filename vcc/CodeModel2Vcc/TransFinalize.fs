//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light


namespace Microsoft.Research.Vcc
 open Microsoft.Research.Vcc
 open Microsoft.Research.Vcc.TransUtil
 open Microsoft.Research.Vcc.CAST
 
 module TransFinalize =

  let init (helper:Helper.Env) =
    // ============================================================================================================

    let addRangeAssumptions =
      let rangeAssumptions (parm : Variable) =
        match parm.Type with
          | Integer _ ->
            [inRange (boolBogusEC ()) (mkRef parm)]
          | _ ->
            []                    

      let quantRange self = function
        | Quant (c, q) ->
          let lst = List.concat (List.map rangeAssumptions q.Variables)
          let condition = Some (multiAnd (Option.to_list q.Condition @ lst))
          Some (Quant (c, { q with Condition = condition; Body = self q.Body }))
        | _ -> None        

      let rangeAssume (parm:Variable) =
        List.map Expr.MkAssume (rangeAssumptions parm)
      let aux (h:Function) =
        h.Body <- Option.map (addStmts (List.concat (List.map rangeAssume h.InParameters))) h.Body
        if h.RetType._IsInteger then
          let ec = { boolBogusEC() with Token = h.Token }
          h.Ensures <- Expr.Macro(ec, "free_ensures", [inRange ec (Expr.Result({bogusEC with Type = h.RetType}))]) :: h.Ensures
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
        | Expr.Ref (_, _)
        | Prim (_, _, _)
        | Expr.Call (_, _, _, _)
        | IntLiteral (_, _)
        | BoolLiteral (_, _) 
        | Deref (_, _)
        | Dot (_, _, _)
        | Index (_, _, _)
        | Cast (_, _, _)
        | Result (_)
        | Old (_, _, _)  
        | Macro (_, _, _)
        | VarWrite (_, _, _)
        | MemoryWrite (_, _, _)
        | Goto (_, _)
        | Label (_, _)
        | Block (_, _)
        | Return (_, _)
        | VarDecl (_, _)
        | Stmt (_, _)
        | Pure (_, _)
        | UserData(_, _)
        | Comment (_, _) -> None
        
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
        | CallMacro(_, "_vcc_owns", [_; Cast(_,_,expr)]) 
        | CallMacro(_, "_vcc_owns", [_; expr]) as owns -> 
          match expr.Type with
            | Type.Ptr(Type.Ref(td)) when staticOwns td ->
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
      let stmtalize (expr : Expr) = 
        let ecVoid = {expr.Common with Type = Type.Void}
        let dummy = getTmp helper ("stmtexpr" + ((!dummyId).ToString())) expr.Type VarKind.Local
        let decl = VarDecl(ecVoid, dummy)
        let assign = VarWrite(ecVoid, [dummy], expr)
        incr dummyId
        [decl; assign]
      function
        | Stmt(ec, expr) when not (ignoreType expr.Type) -> Some(Expr.MkBlock(stmtalize (self expr)))
        | Block(ec, []) -> None
        | Block(ec, stmts) ->
          let splitLast = 
            let rec loop acc = function
              | [] -> helper.Die()
              | [x] -> x, List.rev acc
              | x::xs -> loop (x::acc) xs
            loop []
          let stmtalizeNonVoid (expr : Expr) = if ignoreType expr.Type then [expr] else stmtalize (expr)
          let last, stmts' = splitLast stmts
          //System.Diagnostics.Debugger.Break()
          Some(Block(ec, (stmts' |> List.map self |> List.map stmtalizeNonVoid |> List.concat) @ [ self last ]))
          
        | _ -> None
        

    // ============================================================================================================
    
    helper.AddTransformer ("final-begin", Helper.DoNothing)
    
    helper.AddTransformer ("final-range-assumptions", Helper.Decl addRangeAssumptions)
    helper.AddTransformer ("final-return-conversions", Helper.Decl addReturnConversions)
    helper.AddTransformer ("final-stmt-expressions", Helper.Expr assignExpressionStmts)
    helper.AddTransformer ("final-linearize", Helper.Decl (ToCoreC.linearizeDecls helper))
    helper.AddTransformer ("final-keeps-warning", Helper.Decl (List.map theKeepsWarning))
    helper.AddTransformer ("final-dynamic-owns", Helper.Decl errorForMissingDynamicOwns)
    helper.AddTransformer ("final-before-cleanup", Helper.DoNothing)
    // reads check goes here
    
    helper.AddTransformer ("final-fold-ITE", Helper.Expr foldIteBack)
    helper.AddTransformer ("final-ITE-to-logical", Helper.Expr introduceAndOrs)
    helper.AddTransformer ("final-bool-conversions", Helper.Decl addBoolConversions)
    helper.AddTransformer ("final-bv-cleanup", Helper.Expr removeTrivialBitvectorOperations)
    helper.AddTransformer ("final-insert-state-arguments", Helper.Expr (ToCoreC.handlePureCalls helper))
    
    helper.AddTransformer ("final-end", Helper.DoNothing)
