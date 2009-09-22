//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
 open Microsoft.Research.Vcc
 open Microsoft.Research.Vcc.TransUtil
 open Microsoft.Research.Vcc.Util
 open Microsoft.Research.Vcc.CAST
 
 module Normalizer =
 
  // ============================================================================================================          
  let rec doHandleComparison helper self = function
    | Expr.Prim (c, op, [Expr.Cast ({ Type = Integer _ }, _, a1); Expr.Cast ({ Type = Integer _ }, _, a2)]) 
      when op.IsEqOrIneq && a1.Type = Type.Bool && a2.Type = Type.Bool -> 
      Some (self (Expr.Prim (c, op, [a1; a2])))
    | Expr.Prim ({ Type = Type.Integer _ } as c, op, args) when op.IsEqOrIneq ->
      Some (self (Expr.Cast (c, Processed, Expr.Prim ({ c with Type = Type.Bool }, op, args))))
    | Expr.Prim (c, op, [a1; a2]) when op.IsEqOrIneq ->
      let toGenericPtr (e:Expr) isSpec =
        Expr.Cast ({ e.Common with Type = Type.MkPtr(Integer IntKind.UInt8, isSpec) }, Processed, e)
      match a1.Type, a2.Type with
        | PtrSoP(t, isSpec), PtrSoP(t', isSpec') ->
          if isSpec <> isSpec' then die()
          if (t = Void) or (t' = Void) then 
            let macro = if op.IsEq then "_vcc_ptr_eq" else "_vcc_ptr_neq"
            Some (self (Expr.Macro (c, macro, [toGenericPtr a1 isSpec; toGenericPtr a2 isSpec])))
          elif t = t' then None
          else 
            Some (self (Expr.Prim (c, op, [toGenericPtr a1 isSpec; toGenericPtr a2 isSpec])))
        | _ -> None
    | Expr.Dot (c, e, ({ Type = Array (t, _) } as f)) when c.Type <> SpecPtr t && c.Type <> PhysPtr t ->
      Some (self (Expr.MkDot (c, e, f)))
    | _ -> None
  
  
  let doHandleConversions = 
    let rec doHandleConversions' inGroupInvariant self = function
      | Expr.Cast ({ Type = ObjectT}, _, e) -> Some (self e)
      | Expr.Cast ({Type = Ptr(t1) } as ec, cs, Expr.Cast ({ Type = (ObjectT | Ptr(Type.Ref(_) | Void)) }, _, e')) when t1 <> Void -> Some (self (Cast(ec, cs, e')))
      | Expr.Cast ({ Type = t1 }, (Processed|Unchecked), Expr.Cast (_, (Processed|Unchecked), e')) when e'.Type = t1 -> Some (self e')
      | Expr.Cast ({ Type = Bool }, _, Expr.Cast (_, _, e')) when e'.Type = Bool -> Some (self e')
      | Expr.Cast ({ Type = PtrSoP(_, isSpec) } as c, _, Expr.IntLiteral (_, ZeroBigInt)) -> 
        Some (self (Expr.Cast (c, Processed, Expr.Macro ({c with Type = Type.MkPtr(Void, isSpec)}, "null", []))))
      | Expr.Cast ({ Type = PtrSoP(_, isSpec) } as c, _, e) when e.Type._IsInteger ->
        match e.Type with
          | Integer k ->
            Some (self (Expr.Cast (c, Processed, Expr.Macro ({c with Type = Type.MkPtr(Void, isSpec)}, "_vcc_" + (Type.IntSuffix k) + "_to_ptr" , [e]))))
          | _ -> None
      | Expr.Cast ({ Type = Integer k }, _, Expr.IntLiteral (c, n)) ->
        let (min, max) = Type.IntRange k
        if min <= n && n <= max then
          Some (Expr.IntLiteral ({ c with Type = Integer k }, n))
        else
          None 
      | Expr.Cast ({ Type = MathInteger }, _, expr) when expr.Type._IsInteger -> Some(self(expr))
      | Expr.Cast(ec, _, Expr.Macro(tc, "this", [])) when inGroupInvariant && ec.Type = tc.Type -> 
        None // Do not remove this cast because the type of 'this' will change later on
      | Expr.Cast (_, _, e') as e ->
        match e'.Type, e.Type with
          | Ptr _, Ptr Void -> Some (self e')
          | _, Type.Ref { Name = "#Object" } -> Some (self e')
          | t, t' when t = t' -> Some (self e')
          | _ -> None
      | Expr.Call(c, ({Name = "_vcc_inv_group"} as fn), targs, args) -> Some(Expr.Call(c, fn, targs, List.map (fun (e:Expr) -> e.SelfMap(doHandleConversions' true)) args))
      | _ -> None
    doHandleConversions' false
    
  let handleConversions helper = deepMapExpressions (doHandleComparison helper) >> deepMapExpressions doHandleConversions
    
  // ============================================================================================================      
  
  let init (helper:Helper.Env) =
    let internalFunction = TransUtil.internalFunction helper
 
    // ============================================================================================================
    
    // It is likely that FELT is wrong here, in C there is no difference
    // between passing int a[10] and int *a. Also it sometimes creates
    // temproraries of such types.    
    let removeFixedSizeArraysAsParameters decls =
      let map = new Dict<_,_>()
      let replParm (p : Variable) =
        match map.TryGetValue p with
          | true, p -> p
          | _ ->
            match p.Type with
              | Array (t, _) ->
                let p' = { p with Type = Type.MkPtr(t, p.IsSpec) }
                map.Add (p, p')
                p'          
              | _ -> p       
              
      let replRef self = function
        | VarDecl (c, v) when replParm v <> v -> Some (VarDecl (c, replParm v))
        | Macro (c, "=", [Expr.Ref (c', v); Expr.Deref (_, e)]) when map.ContainsKey v ->
          Some (Macro (c, "=", [Expr.Ref ({c' with Type = map.[v].Type }, map.[v]); self e]))
        | Macro (c, "&", [Expr.Ref (_, v)]) when map.ContainsKey v -> Some (Expr.Ref ({ c with Type = map.[v].Type }, map.[v]))
        | Expr.Ref (c, v) when map.ContainsKey v ->
          helper.Oops (c.Token, "fixed size array used as value")
          None
        | _ -> None        
      for d in decls do
        match d with          
          | Top.FunctionDecl h ->
            h.Parameters <- List.map replParm h.Parameters          
          | _ -> ()
      deepMapExpressions replRef decls
      
    // ============================================================================================================
    
    let handleLemmas self = function 
      | Assert (c, e) -> 
        let e = self e 
        Some (Expr.MkBlock [Assert (c, e); Assume (c, e)])
      | Macro (_, "loop_contract", _) as expr -> Some expr
      | _ -> None      
         
    // ============================================================================================================
    
    let handleClaims self = function
      | Call (c, ({ Name = ("_vcc_claim"|"_vcc_upgrade_claim" as name) } as fn), _, args) ->
        match List.rev args with
          | x :: xs ->
            Some (self (Macro (c, name.Replace("_vcc_", ""), Expr.Pure (x.Common, convertToBool (fun x -> x) x) :: xs)))
          | _ -> 
            helper.Oops (c.Token, "no arguments to claim")
            None
            
      | Call (c, { Name = "_vcc_unclaim" }, _, args) ->
        Some (Stmt (c, Macro (c, "unclaim", List.map self args)))

      | Call (c, { Name = "_vcc_begin_update" }, _, args) ->
        Some (Stmt (c, Macro (c, "begin_update", List.map self args)))
        
      | Atomic (c, objs, body) -> 
        let errorIfNotPtrToStruct (expr : Expr) =
          match expr.Type with
            | Ptr(Type.Ref(_)| Claim) -> ()
            | Type.ObjectT -> ()
            | t -> helper.Error(expr.Token, 9668, "'" + expr.Token.Value + "' has non-admissible type '" + t.ToString() + "' for atomic")
        List.iter errorIfNotPtrToStruct objs
        Some (Atomic (c, List.map (fun (e:Expr) -> Pure (e.Common, self e)) objs, self body))
        
      | _ -> None
    
    // ============================================================================================================

    let addExplicitReturn =
      let doDecl = function
        | Top.FunctionDecl ({ RetType = Void; Body = Some b } as f) ->
          let rec addIt = function
            | Block (c, lst) as e ->
              let rec repl acc = function
                | [x] -> Block (c, List.rev (addIt x :: acc))
                | x :: xs -> repl (x :: acc) xs
                | [] -> Block (c, [])
              repl [] lst
            | Comment (c, "empty") -> Return (c, None)
            | x -> x
          f.Body <- Some (addIt b)
          Top.FunctionDecl f        
        | x -> x
      List.map doDecl

    // ============================================================================================================

    let miscNorm =
      let stringLiterals = new Dict<_,_>()
                
      // TODO rename this
      let doFixQuantifiers self = function
        | Expr.Quant (ec, { Kind = k1;
                            Variables = vars1;
                            Triggers = triggers;      
                            Condition = None;
                            Body = Expr.Quant (_, ({ Kind = k2;                                                   
                                                     Triggers = []; 
                                                     Variables = vars2 } as q)) } ) when k1 = k2 ->
          Some (self (Expr.Quant (ec, { q with Variables = vars1 @ vars2; Triggers = triggers })))
        | Expr.Call (c, { Name = "_vcc_boogie" }, _, [Expr.Cast (_, _, EString (c', v))]) ->
          Some (Expr.Macro (c, "boogie_quote", [Expr.Macro (c', v, [])]))
        | Expr.Call (c, { Name = "_vcc_inv_group"}, _, [Expr.Cast (_, _, EString (c', v)); groupInv]) ->
          Some (Expr.Macro (c, "group_invariant", [Expr.Macro (c', v, []); self groupInv]))
        | EString (c, v) ->      
          let id =
            match stringLiterals.TryGetValue v with
              | (true, id) -> id
              | _ -> 
                let id = stringLiterals.Count
                stringLiterals.Add (v, id)
                id
          Some (Expr.Macro (c, "_vcc_get_string_literal", [mkInt id; mkInt v.Length]))
        | Expr.Macro (c, "ptr_addition", [e1; e2]) as expr ->
          let ptr, off =
            if e1.Type._IsPtr then (e1, e2)
            else (e2, e1)
          let elType = elementTypeForArithmetic ptr.Type
          let off =
            match extractArraySize helper expr elType off with
              | Some e -> e
              | None -> Expr.IntLiteral (off.Common, one)
          Some (self (Expr.Index (c, ptr, off)))
        | Expr.Prim (c, Op ("-", _), [e1; e2]) when e1.Type._IsPtr && e2.Type._IsPtr ->      
          match e1.Type, e2.Type with
            | Ptr t1, Ptr t2 when t1 = t2 ->
                let expr = Expr.Macro (c, "_vcc_byte_ptr_subtraction", [e1; e2])
                let expr =
                  if t1.SizeOf = 1 then
                    expr
                  else
                    Expr.Prim (c, Op ("/", Processed), [expr; mkInt t1.SizeOf])
                Some (self expr)
            | _ -> 
              helper.Error (c.Token, 9601, "pointer types in subtraction differ", None)
              None
        
        | Expr.Prim (c, (Op (("<"|">"|"<="|">="), _) as op), [e1; e2]) when e1.Type._IsPtr && e2.Type._IsPtr ->
          let ec = { c with Type = Type.Integer IntKind.Int64 }
          Some (self (Expr.Prim (c, op, [Expr.Macro (ec, "_vcc_byte_ptr_subtraction", [e1; e2]);
                                         Expr.IntLiteral (ec, zero)])))
        
        | Expr.Prim (c, (Op ("-", _) as op), [e1; e2]) when e1.Type._IsPtr && not e2.Type._IsPtr ->  
          let tok = if e2.Token <> Token.NoToken then e2.Token else e1.Token    
          Some (self (Expr.Macro (c, "ptr_addition", [e1; Expr.Prim ({ e2.Common with Token = tok }, Op("-", CheckedStatus.Processed), [e2])])))
          
        | Expr.Prim (c, Op ("-", ch), [e]) ->
          Some (self (Expr.Prim (c, Op ("-", ch), [Expr.IntLiteral (c, zero); e])))
        
        | Expr.Prim (c, (Op (("&"|"|"|"^"), _) as op), [e1; e2]) when e1.Type = Bool || e2.Type = Bool ->
          let toInt (e:Expr) = 
            if e.Type = Bool then
              Cast ({ e.Common with Type = Integer IntKind.Int32 }, Processed, e)
            else e
          let c = if c.Type = Bool then { c with Type = Integer IntKind.Int32 } else c
          Some (self (Expr.Prim (c, op, [toInt e1; toInt e2])))
        
        // this is an easy one ;-)
        | Expr.Prim (_, Op ("+", _), [e]) -> Some (self e)
        | Expr.Macro (c, "stack_allocate_array", [s]) -> 
          match c.Type with 
          | Ptr elemType ->
            let arrTy = 
              match s with
              | Expr.IntLiteral(_, sz) -> Type.Array(elemType, (int)sz)
              | _ -> die()
            Some (Expr.Cast(c, Processed, 
                            Expr.Call({ c with Type = PhysPtr Void }, internalFunction "stack_alloc", [arrTy], [Expr.Macro(bogusEC, "stackframe", [])]))) // TODO Ptr kind
          | _ -> die()        
        | Expr.Cast ({ Type = Ptr t } as c, _, Expr.Call (_, { Name = "malloc" }, _, [sz])) as expr ->
          match extractArraySize helper expr t sz with
            | None ->
              Some (Expr.Call (c, internalFunction "alloc", [], [typeExpr t]))
            | Some elts ->
              // TODO overflow! we get rid of multiplication here
              let t' = typeExpr t
              let arrTy = Expr.Macro (t'.Common, "_vcc_array", [t'; self elts])
              Some (Expr.Cast (c, Processed, 
                               Expr.Call ({ c with Type = PhysPtr Void }, internalFunction "alloc", [], [arrTy])))
        | Expr.Call (c, { Name = "free" }, _, [p]) ->
          Some (Expr.Call (c, internalFunction "free", [], [self p]))
        | Expr.Macro (c, "&", [Macro (c', "this", [])]) when not c'.Type._IsPtr ->
          Some (self (Macro (c, "this", [])))
        | Expr.Call (c, { Name = "_vcc_create_set" }, _, pars) ->
          match pars with
            | [] 
            | [_] -> Some(Expr.Macro(c, "_vcc_set_empty", []))
            | _ :: rest ->
              let mkSingleton (e:Expr) = Expr.Macro({e.Common with Type = c.Type}, "_vcc_set_singleton", [e])
              let mkUnion (e1:Expr) (e2:Expr) = Expr.Macro(e1.Common, "_vcc_set_union", [e1;e2])
              let rec createUnion exprs =
                let rec splitAt n acc (l : 'a list) =
                  if (n = 0 || l.IsEmpty) then (List.rev acc, l)
                  else match l with
                       | x :: xs -> splitAt (n-1) (x::acc) xs
                       | _ -> die()
                match exprs with
                | [] -> die()
                | [expr] -> mkSingleton expr
                | _ -> 
                  let (a,b) = splitAt (exprs.Length / 2) [] exprs
                  mkUnion (createUnion a) (createUnion b)
              Some(rest |> List.map self |> createUnion)
   
        | _ -> None
    
      deepMapExpressions doFixQuantifiers
        
    // ============================================================================================================
    
    let normalizeWrites decls =
      let fixOne (e:Expr) =
        match e.Type with
          | MathTypeRef "ptrset"
          | ObjectT
          | Array _
          | Ptr _ -> e         
          | _ ->
            match e with
              | Call (c, { Name = ("_vcc_ref_cnt") }, _, [p]) ->
                //helper.Error (c.Token, 9999, "the ref_cnt(...) no longer resides in memory and cannot be written to", None)
                p
              | _ -> e // we will catch this error later
                
      let fixWrites = function
        | Top.FunctionDecl f ->
          f.Writes <- List.map fixOne f.Writes
          f.Reads <- List.map fixOne f.Reads
        | _ -> ()
      List.iter fixWrites decls
      decls       
   
    // ============================================================================================================
    
    let inlineAtomics decls =
      let inlines = gdict()
      let isntInline = function
        | Top.FunctionDecl fd ->
          if hasBoolAttr "atomic_inline" fd.CustomAttr then
            if fd.IsPure then
              helper.Error(fd.Token, 9667, "Pure function '" + fd.Name + "' cannot be inlined.")
              true
            else 
              if fd.Requires.Length > 0 || fd.Ensures.Length > 0 || fd.Reads.Length > 0 || fd.Writes.Length > 0 then
                helper.Warning(fd.Token, 9117, "Contracts of inlined function '" + fd.Name + "' are ignored.")
              inlines.Add (fd, true)
              false
          else true
        | _ -> true
        
      let _inline self = function
        | Call (c, f, targs, args) when inlines.ContainsKey f ->
          let f = f.Specialize(targs, true)
          let map = gdict()
          let bindIn (formal:Variable) = function
            | Expr.Ref _ as r -> 
              map.Add (formal, r)
              []
            | actual ->
              let c = actual.Common
              let cvoid = { c with Type = Void }
              let tmp = getTmp helper formal.Name formal.Type VarKind.Local
              map.Add (formal, Ref (c, tmp))
              [VarDecl (cvoid, tmp);
               Macro (cvoid, "=", [ Ref ({ c with Type = tmp.Type }, tmp); actual ])]
          let bindOut (formal:Variable) = function
            | Macro(_, "out", [actual]) ->
              let c = actual.Common
              let cvoid = { c with Type = Void }
              let tmp = getTmp helper formal.Name formal.Type VarKind.SpecLocal
              map.Add (formal, Ref (c, tmp))
              VarDecl (cvoid, tmp), Macro(cvoid, "spec", [Macro (cvoid, "=", [ actual; Ref ({ c with Type = tmp.Type }, tmp)])])
            | _ -> die()
          let inPars, inActuals, outPars, outActuals = 
            let rec loop (formals:Variable list) actuals fiAcc aiAcc foAcc aoAcc = 
              match formals, actuals with
                | [], [] -> List.rev fiAcc, List.rev aiAcc, List.rev foAcc, List.rev aoAcc
                | ({Kind = VarKind.OutParameter} as formal)::formals', actual::actuals' -> 
                  loop formals' actuals' fiAcc aiAcc (formal::foAcc) (actual::aoAcc)
                | formal::formals', actual::actuals' -> loop formals' actuals' (formal::fiAcc) (actual::aiAcc) foAcc aoAcc
                | _ -> die()
            loop f.Parameters args [] [] [] []

          let inits = List.map2 bindIn inPars inActuals |> List.concat
          let declsForOutPars, outParAssignmentOnExit = List.map2 bindOut outPars outActuals |> List.unzip
          let resVar = getTmp helper "res" f.RetType VarKind.Local
          let subst self = function
            | Ref (_, v)  ->
              match map.TryGetValue(v) with
                | true, v' -> Some (map.[v])
                | false, _ -> None
            | VarDecl (_, v) when map.ContainsKey v ->
              Some (Expr.MkBlock [])
            | Return (c, Some e) ->              
              Some (Macro (c, "=", [Ref (c, resVar); self e]))
              // TODO check if it the return doesn't disturn control flow
              // TODO look for gotos and such
            | Return (c, None) ->
              Some (Expr.MkBlock [])
            | _ -> None            
          let body = 
            match f.Body with
              | Some stmt -> Macro ({c with Type = Type.Void}, "inlined_atomic", [stmt.SelfMap subst])
              | None -> Comment(bogusEC, "inlined function without body")
          let body =
            if f.RetType = Void then
              body :: outParAssignmentOnExit
            else
              [VarDecl ({c with Type = Void}, resVar); body] @ outParAssignmentOnExit @ [Ref (c, resVar)]
          Some (Expr.MkBlock (inits @ declsForOutPars @ body))
        | d -> None
              
      let decls = List.filter isntInline decls      
      decls |> deepMapExpressions _inline
   
    // ============================================================================================================
   
    // normalize expressions of the form CONTAINING_RECORD(addr, T, f1.f2) into
    // CONTAINING_RECORD(CONTAINING_RECORD(addr, T0, f2), T, f1)
   
    let normalizeContainingStruct self = function
      | CallMacro(ec, "_vcc_containing_struct", _, [addr; df]) ->
        let cs =  "_vcc_containing_struct"
        match df with
          | Expr.Macro(_, "&", [Expr.Deref(_, (Expr.Dot(_, Expr.Cast(_, _, Expr.Macro(_, "null", [])),fld) as dot))]) ->
            Some(Macro(ec, cs, [self addr; Expr.UserData(bogusEC, fld)]))
          | Expr.Macro(_, "&", [Expr.Deref(_, (Expr.Dot(_, e, fld) as dot))])->
            let result = Some (self (Macro(ec, cs, [Expr.Macro({ ec with Type = PhysPtr(Type.Ref(fld.Parent))}, cs, [addr; Expr.UserData(bogusEC, fld)]); e]))) // TODO Ptr kind
            result
          | _ -> None
      | _ -> None
   
    // ============================================================================================================

    let normalizeAtomicOperations self = function
      | Macro(ec, "atomic_op", args) -> 
        let splitLastTwo = 
          let rec splitLastTwo acc = function
          | [x;y] -> x, y, List.rev acc
          | x :: xs -> splitLastTwo (x :: acc) xs
          | [_]
          | [] -> die()
          splitLastTwo []
        let cvoid = {ec with Type = Void}
        let ghostUpdate, op, atomicParameters = splitLastTwo args
        let tmp = getTmp helper "atomic_op" op.Type VarKind.Local
        let res = Expr.Ref({bogusEC with Type = op.Type}, tmp)
        let op' = Macro(op.Common, "=", [res; op])
        let ghostUpdate' = 
          match ghostUpdate with
            | Macro(_, "null", []) -> []
            | _ ->
              let fixupResult self = function
                | Result _ -> Some(res)
                | _ -> None
              [ghostUpdate.SelfMap fixupResult]
        let stmts = VarDecl(cvoid, tmp) :: Atomic(cvoid, atomicParameters, Expr.MkBlock (op':: ghostUpdate')) :: [res]
        Some(Block(ec, stmts))
      | _ -> None
    
    // ============================================================================================================

    let normalizeSkinnyExpose self = function
      | Macro (ec, "while", [Macro (_, "loop_contract", contract); CallMacro (_, "_vcc_skinny_expose", _, objects); body]) ->
        let ptrsetEC = { bogusEC with Type = Type.PtrSet }
        let empty = Macro (ptrsetEC, "_vcc_set_empty", [])
        let single e = Macro (ptrsetEC, "_vcc_set_singleton", [e])
        let union a b = Macro (ptrsetEC, "_vcc_set_union", [a; b])
        let extractWrites acc = function
          | Assert (_, Macro (_, "loop_writes", [e])) -> e :: acc
          | e ->
            helper.Error (e.Token, 9674, "skinny_expose(...) does not allow invariants, only writes(...)")
            acc
        let writes = List.rev (List.fold extractWrites [] contract)
                
        let setify acc (e:Expr) =
          let e = 
            match e.Type with 
              | MathTypeRef "ptrset" -> e
              | _ -> single e
          if acc = empty then e
          else union acc e
        
        let setify = List.fold setify empty
        let isNonStruct (e:Expr) = 
          match e.Type with
            | Ptr (Type.Ref { Kind = Struct|Union }) -> false
            | _ -> true
        
        let fnToken (obj:Expr) name =
          { forwardingToken obj.Token None (fun () -> name + "(" + obj.Token.Value + ")") with Type = Void }
    
        let prestateVar = getTmp helper "prestate" Type.MathState VarKind.SpecLocal
        let nowstate = Expr.Macro ({ bogusEC with Type = Type.MathState }, "_vcc_current_state", [])        
        let prestate = mkRef prestateVar
        let saveState = [VarDecl (bogusEC, prestateVar); Macro (bogusEC, "=", [prestate; nowstate])]
        let postUnwrapVar = getTmp helper "postUnwrap" Type.MathState VarKind.SpecLocal
        let postUnwrap = mkRef postUnwrapVar
        let savePostUnwrapState = [VarDecl (bogusEC, postUnwrapVar); Macro (bogusEC, "=", [postUnwrap; nowstate])]
        let old (e:Expr) = Old ({ e.Common with Token = new WarningSuppressingToken (e.Token, 9106) }, prestate, e)
        
        let writeSet = old (setify writes)
        let primWriteSet = old (setify (List.filter isNonStruct writes))
        
        let writesCheck =
          let assrt id msg name =
            let tok = afmte id msg objects
            Expr.MkAssert (Macro (tok, "_vcc_updated_only_" + name, [postUnwrap; nowstate; writeSet]))
          [assrt 8530 "skinny_expose({0}, ...) body has written at an unlisted location" "values";
           assrt 8530 "skinny_expose({0}, ...) body has written at an unlisted location in a domain" "domains"]
        
        let introduceWrapUnwrap acc obj =
          let obj' = old obj
          let wrapLike name vcc_name =
            let tok = fnToken obj name
            Stmt (tok, Macro (tok, vcc_name, [obj']))
            
          let owns st = Macro ({ bogusEC with Type = Type.PtrSet }, "_vcc_owns", [st; obj])
          let checkOwns = 
            let tok = afmte 8531 "owns({0}) was updated inside skinny_expose(...)" [obj]
            Expr.MkAssert (Prim (tok, Op ("==", Processed), [owns nowstate; owns prestate]))
            
          [wrapLike "unwrap" "_vcc_unwrap"] @ acc @ 
          [checkOwns;
           wrapLike "wrap" "_vcc_wrap_non_owns"]
        
        let finalAssume =
          Expr.MkAssume (Macro (boolBogusEC(), "_vcc_domain_updated_at", [prestate; nowstate; old objects.Head; primWriteSet ]))
          
        let totalBody = 
          saveState @
          (List.rev objects |> List.fold introduceWrapUnwrap (savePostUnwrapState @ [body] @ writesCheck)) @
          [finalAssume]
          
        Some (Expr.MkBlock totalBody)
      | _ -> None
    
    // ============================================================================================================

    let removeDerefFromOutParams self = function
      | Deref(ec, Ref(_, ({Kind = VarKind.OutParameter} as v))) -> Some(Ref(ec, v))
      | _ -> None
    
    // ============================================================================================================

    let normalizeInitializers self = 
    
      let splitOfLast l = 
        let rl = List.rev l
        rl.Head, List.rev (rl.Tail)
    
      let subst v expr (e:Expr) = 
        let doSubst _ = function
          | Expr.Ref(_,v) -> Some(expr)
          | _ -> None
        e.SelfMap(doSubst)
        
      let shouldHandle = function
        | Type.Ref(td) when hasBoolAttr "record" td.CustomAttr -> true
        | _ -> false
        
      let foldBackFieldAssignments ec tmp =
        let rec foldBackFieldAssignments' (acc : Expr) = function
          | Macro(_, "=", [Ref(_,v); e]) when v = tmp -> e
          | Macro(_, "=", [Deref(_, Dot(_, Macro(_, "&", [Ref(_,v)]), f)); e]) when v = tmp -> 
            Macro({acc.Common with Type = v.Type}, "vs_updated", [Expr.MkDot(acc, f); self e])
          | Block(_, stmts) -> recurse acc stmts
          | _ -> acc
        and recurse = List.fold foldBackFieldAssignments' 
        recurse (Macro({ec with Type = Type.MathStruct}, "vs_zero",  []))
        
      function
        | Block(ec, stmts) when shouldHandle (ec.Type) ->
            match splitOfLast stmts with 
              | Ref(ec, v), stmts' -> Some(foldBackFieldAssignments ec v stmts')
              | _ -> None        
        | _ -> None
    
    // ============================================================================================================
    
    let normalizeReinterpretation self = 
      let asArray sz (obj:Expr) =
        let msg () = "as_array((uint8_t*)" + obj.Token.Value + ", " + sz.ToString() + ")"
        let bec = { forwardingToken obj.Token None msg with Type = PhysPtr Type.Byte } // TODO Ptr kind
        Macro (bec, "_vcc_as_array", [Cast (bec, Unchecked, obj); sz])
      let typeId (obj:Expr) =
        Macro ({ obj.Common with Type = Type.Math "typeid_t" }, "_vcc_typeof", [obj])
      function
        | Call (c, ({ Name = "_vcc_from_bytes" } as fn), _, [CallMacro (_, "_vcc_as_array", [], [arg; sz]) as arr]) ->
          let eltSz =
            match arg.Type with
              | Ptr t -> mkInt t.SizeOf
              | _ ->
                helper.Error (c.Token, 9684, "wrong type of object in from_bytes(as_array(...))", None)
                mkInt 1
          let sz = Prim (sz.Common, Op("*", Processed), [eltSz; sz])
          Some (Stmt (c, Call (c, fn, [], [asArray sz arg; typeId arr])))
        
        | Call (c, ({ Name = "_vcc_from_bytes" } as fn), _, [obj]) ->
          let sz =
            match obj.Type with
              | Ptr t ->
                if not t.IsComposite then
                  helper.Error (c.Token, 9700, "reinterpretation to a primitive type is not supported; please use a single-element array instead")
                t.SizeOf
              // Seems to be never reached
              // | Array (_, _) as a -> a.SizeOf
              | _ -> 
                helper.Error (c.Token, 9684, "wrong type of object in from_bytes(...)", None)
                1
          Some (Stmt (c, Call (c, fn, [], [asArray (mkInt sz) obj; typeId obj])))
        | Call (c, ({ Name = "_vcc_from_bytes" } as fn), _, [obj; sz]) ->
          match obj.Type with
            | Ptr t ->
              if not t.IsComposite then
                helper.Error (c.Token, 9700, "reinterpretation to a primitive type is not supported; please use a single-element array instead")
            | _ -> 
              helper.Error (c.Token, 9684, "wrong type of object in from_bytes(...)", None)
          Some (Stmt (c, Call (c, fn, [], [asArray sz obj; typeId obj])))
        | CallMacro (c, "_vcc_from_bytes", _, _) ->
          helper.Error (c.Token, 9685, "wrong number of arguments to from_bytes(...)", None)
          None
        | _ -> None
    
    // ============================================================================================================
    
    let reportGenericsErrors decls = 
    
      let reportGenericsErrors' self =
        let errorForVarType = function
          | Expr.Ref _ -> None // these are caught for the declaration
          | e ->
            match e.Type with
              | TypeVar({Name = name}) -> 
                helper.Error(e.Token, 9691, "Expression '" + e.Token.Value + "' is of generic type '" + name + "'; only pointers to generic types are supported.")
                Some(bogusExpr)
              | _ -> None
        function
          | Expr.VarDecl(ec, {Name = name; Type = TypeVar({Name = tvName}); Kind = (Local|SpecLocal) }) ->
            helper.Error(ec.Token, 9693, "Cannot declare local '" + name + "' of generic type '" + tvName + "'")
            Some(bogusExpr)
          | Expr.Call(ec,{TypeParameters = tpars}, targs, _) as e -> 
            errorForVarType e
          | e -> errorForVarType e

      for d in decls do
        match d with 
          | Top.FunctionDecl(fn) ->
            let errorForGenericPar (v : Variable) =
              match v.Type with 
                | TypeVar({Name = tvName}) ->
                  helper.Error(fn.Token, 9693, "Cannot declare parameter '" + v.Name + "' of generic type '" + tvName + "'")
                | _ -> ()
            List.iter errorForGenericPar (fn.Parameters)
          | _ -> ()
          
      decls |> deepMapExpressions reportGenericsErrors' 
    // ============================================================================================================

    let normalizeUse self = function
      | CallMacro(ec, "_vcc_use", _, [lbl; e]) ->
        let rec normalizeLabel = function
          | Cast(_, _, Macro(_, "&", [Macro(_, "string", [lbl])])) -> lbl
          | _ -> die()
        Some(Macro(ec, "_vcc_use", [normalizeLabel lbl; self e]))
      | Call(ec, ({Name = "_vcc_in_domain"|"_vcc_in_vdomain"} as fn), targs, [e1; e2]) ->
        let e2' = self e2
        match self e1 with
          | Macro(uc, "_vcc_use", [UserData(_, (:? string as lbl)); e1']) ->
            let lbls = [for s in lbl.Split('|') -> s]
            let mkInDomain l = Call(ec, fn, targs, [Macro(uc, "_vcc_use", [Expr.ToUserData(l); e1']); e2'])
            let mkAnd c1 c2 = Expr.Prim(ec, Op("&&", Unchecked), [c1; c2])
            Some(List.fold (fun expr l -> mkAnd expr (mkInDomain l)) (mkInDomain lbls.Head) lbls.Tail)
          | e1' -> Some(Call(ec, fn, targs, [e1'; e2']))
      | _ -> None
 
    // ============================================================================================================

    let normalizeInlineArrayAccesses self = function
      | Macro(ec, "&", [e]) ->
        match e with 
          | Dot(_,_,f) when f.Type._IsArray -> Some(e)
          | e' -> Some(Macro(ec, "&", [e']))
      | Deref(_, (Dot(_,_,f) as dot)) when f.Type._IsArray -> Some(self dot)
      | _ -> None
 
    // ============================================================================================================
 
    helper.AddTransformer ("norm-begin", Helper.DoNothing)
    helper.AddTransformer ("norm-initializers", Helper.Expr normalizeInitializers)
    helper.AddTransformer ("norm-use", Helper.Expr normalizeUse)
    helper.AddTransformer ("norm-fixed-array-parms", Helper.Decl removeFixedSizeArraysAsParameters)
    helper.AddTransformer ("norm-inline-array-accesses", Helper.Expr normalizeInlineArrayAccesses)
    helper.AddTransformer ("norm-out-params", Helper.Expr removeDerefFromOutParams)
    helper.AddTransformer ("norm-comparison", Helper.Expr (doHandleComparison helper))
    helper.AddTransformer ("norm-conversions", Helper.Expr doHandleConversions)   
    helper.AddTransformer ("norm-generic-errors", Helper.Decl reportGenericsErrors) 
    helper.AddTransformer ("norm-containing-struct", Helper.Expr normalizeContainingStruct)
    helper.AddTransformer ("add-assume-to-assert", Helper.Expr handleLemmas)    
    helper.AddTransformer ("fixup-claims", Helper.Expr handleClaims)    
    helper.AddTransformer ("add-explicit-return", Helper.Decl addExplicitReturn)
    helper.AddTransformer ("norm-atomic-ops", Helper.Expr normalizeAtomicOperations)
    helper.AddTransformer ("norm-skinny-expose", Helper.Expr normalizeSkinnyExpose)
    helper.AddTransformer ("norm-misc", Helper.Decl miscNorm)
    helper.AddTransformer ("norm-writes", Helper.Decl normalizeWrites)
    helper.AddTransformer ("norm-atomic-inline", Helper.Decl inlineAtomics)
    helper.AddTransformer ("norm-reintp", Helper.Expr normalizeReinterpretation)
    helper.AddTransformer ("norm-end", Helper.DoNothing)
      
    