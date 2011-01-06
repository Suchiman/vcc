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

  type StackArrayDetails = SAR of Variable * Type * int * bool
 
  // ============================================================================================================          
  let rec doHandleComparison helper self = function
    | Expr.Prim (c, op, [Expr.Cast ({ Type = Integer _ }, _, a1); Expr.Cast ({ Type = Integer _ }, _, a2)]) 
      when op.IsEqOrIneq && a1.Type = Type.Bool && a2.Type = Type.Bool -> 
      Some (self (Expr.Prim (c, op, [a1; a2])))
    | Expr.Prim ({ Type = Type.Integer _ } as c, op, args) when op.IsEqOrIneq ->
      Some (self (Expr.Cast (c, Processed, Expr.Prim ({ c with Type = Type.Bool }, op, args))))
    | Expr.Prim (c, op, [a1; a2]) when op.IsEqOrIneq ->
      match a1.Type, a2.Type with
        | PtrSoP(t, _), PtrSoP(t', _) ->
          let macro = if op.IsEq then "_vcc_ptr_eq" else "_vcc_ptr_neq"
          Some (self (Expr.Macro (c, macro, [self a1; self a2])))
        | _ -> None
    | Expr.Dot (c, e, ({ Type = Array (t, _) } as f)) when c.Type <> SpecPtr t && c.Type <> PhysPtr t ->
      Some (self (Expr.MkDot (c, e, f)))
    | _ -> None
  
  let doHandleConversions = 
    let rec doHandleConversions' inGroupInvariant self = function
      | Expr.Cast ({ Type = ObjectT}, _, e) -> 
        match e.Type with
          | Ptr _ | ObjectT | Claim -> Some (self e)
          | _ -> None
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
      | Expr.Cast(ec, _, This(tc)) when inGroupInvariant && ec.Type = tc.Type -> 
        None // Do not remove this cast because the type of 'this' will change later on
      | Expr.Cast (_, _, e') as e ->
        match e'.Type, e.Type with
          | (Ptr _|ObjectT), Ptr Void -> Some (self e')
          | _, Type.Ref { Name = "#Object" } -> Some (self e')
          | t, t' when t = t' -> Some (self e')
          | _ -> None
      | Expr.Macro(c, "group_invariant", args) -> Some(Expr.Macro(c, "group_invariant", List.map (fun (e:Expr) -> e.SelfMap(doHandleConversions' true)) args))
      | Expr.Call(c, ({Name = "_vcc_inv_group"} as fn), targs, args) -> Some(Expr.Call(c, fn, targs, List.map (fun (e:Expr) -> e.SelfMap(doHandleConversions' true)) args))
      | _ -> None
    doHandleConversions' false
    
  let handlePurePtrEq (helper:Helper.Env) decls =
    let rec isNull = function
      | Expr.Cast (_, _, e) -> isNull e
      | Expr.Macro (_, "null", []) -> true
      | _ -> false

    let aux (ctx:ExprCtx) self = function
      | Expr.Cast (c, _, p) when c.Type = Type.Bool && p.Type.IsPtr ->
        Some (Expr.Macro (c, "_vcc_ptr_neq_null", [self p]))
      | Expr.Macro (c, ("_vcc_ptr_eq"|"_vcc_ptr_neq" as name), [p1; p2]) ->
        if isNull p1 then
          Some (Expr.Macro (c, name + "_null", [self p2]))
        else if isNull p2 then
          Some (Expr.Macro (c, name + "_null", [self p1]))
        else if ctx.IsPure then
          match p1.Type, p2.Type with
            | Ptr(t1), Ptr(t2) ->
              if t1 <> t2 then
                helper.Warning (c.Token, 9124, "pointers of different types (" + t1.ToString() + " and " + t2.ToString() + ") are never equal in pure context")
              Some (Expr.Macro (c, name + "_pure", [self p1; self p2]))
            | _ -> die()
        else None
      | _ -> None
    if helper.Options.Vcc3 then
      deepMapExpressionsCtx aux decls
    else decls

  let handleConversions helper = 
    deepMapExpressions (doHandleComparison helper) >> 
    deepMapExpressions doHandleConversions >>
    handlePurePtrEq helper
    
  // ============================================================================================================      
  
  let init (helper:Helper.Env) =
    let internalFunction = TransUtil.internalFunction helper
 
    // ============================================================================================================
    
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
        | VarDecl (c, v, attr) when replParm v <> v -> Some (VarDecl (c, replParm v, attr))
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
    
    let makeQuantifiedVarsUnique (expr:Expr) =
      let aux self = function
        | Quant(ec, qd) ->
          let subst = new Dict<_,_>()
          let doSubst (v:Variable) =
            let v' = v.UniqueCopy()
            subst.Add(v,v')
            v'
          let replace (e:Expr) = 
            let replace' _ = function
              | Expr.Ref(ec, v) -> 
                match subst.TryGetValue v with
                  | true, v' -> Some(Expr.Ref(ec, v'))
                  | false, _ -> None
              | _ -> None
            e.SelfMap(replace')
          let vs' = List.map doSubst qd.Variables
          let qd' = { qd with Variables = vs';
                              Triggers = List.map (List.map(replace)) qd.Triggers;
                              Condition = Option.map replace qd.Condition;
                              Body = replace qd.Body }
          Some(Quant(ec, qd'))
        | _ -> None
      expr.SelfMap aux

    let handleLemmas self = 
      function 
        | Assert (c, e, trigs) -> 
          let e = self e 
          Some (Expr.MkBlock [Assert (c, e, trigs); Assume (c, makeQuantifiedVarsUnique e)])
        | Macro (_, "loop_contract", _) as expr -> Some expr
        | _ -> None      
         
    // ============================================================================================================
    
    let inlineCall pref self call =
      match call with
        | Call (ec, fn, _, args) ->
          let callTok = ec.Token
          let inExpansion = fun () -> "from expansion of '" + pref + callTok.Value + "'"
          
          let overrideMacroLocation (expr:Expr) =
            let orig = expr.Token
            let related = new ForwardingToken (orig, orig.Related, inExpansion) :> Token
            let primary = new ForwardingToken (callTok, Some related, fun () -> orig.Value)
            expr.WithCommon { expr.Common with Token = primary }

          let updateArgLocation (expr:Expr) =
            let orig = expr.Token
            let related = new ForwardingToken (fn.Token, orig.Related, inExpansion) :> Token
            let primary = new ForwardingToken (orig, Some related, fun () -> orig.Value)
            expr.WithCommon { expr.Common with Token = primary }
          let args = List.map (fun expr -> ((self expr) : Expr).DeepMap updateArgLocation) args

          let subst = gdict()
          List.iter2 (fun f a -> subst.Add (f, a)) fn.Parameters args 

          (fun (expr:Expr) ->
            (expr.DeepMap overrideMacroLocation).Subst subst |> makeQuantifiedVarsUnique)
        | _ -> die()

    let inlineSpecMacros decls =
      let isSpecMacro (fn:Function) = hasCustomAttr AttrSpecMacro fn.CustomAttr
      let doInline self =
        function 
          | Call (ec, fn, targs, args) when isSpecMacro fn ->
            let fn' = fn.Specialize(targs, false)
            match fn'.Ensures with
              | [Expr.Prim (_, Op ("==", _), ([Result _; e]|[e; Result _]))] ->
                if e.HasSubexpr (function Result _ -> true | _ -> false) then
                  helper.Error (fn.Token, 9714, "'result' cannot be used recursively in a spec macro definition", Some ec.Token)
                  None
                else                 
                  Some (inlineCall "" self (Call({ec with Type = fn'.RetType}, fn', [], args)) e)
              | _ ->
                helper.Error (fn.Token, 9715, "spec macros should have one ensures clause of the form 'result == expr'", Some ec.Token)
                None
          | _ -> None
      deepMapExpressions doInline decls |>
        List.filter (function Top.FunctionDecl f when isSpecMacro f -> false | _ -> true)    

    // ============================================================================================================

    let deepSplitConjunctions self = 
      // TODO: support in new syntax
      let aux self = function
        | Quant (ec, q) ->
          let q = { q with Body = self q.Body }
          let mkQ (e:Expr) = 
            let related = 
              match ec.Token with
                | :? ForwardingToken as t -> t.Related
                | _ -> None
            let t = forwardingToken ec.Token related (fun () -> e.Token.Value + " in " + ec.Token.Value)
            let vars = gdict()
            for v in q.Variables do
              vars.Add (v.UniqueId, true)
            let aux _ = function
              | Expr.Ref (_, v) ->
                vars.Remove v.UniqueId |> ignore
                true
              | _ -> true
            e.SelfVisit aux
            q.Triggers |> List.iter (fun l -> l |> List.iter (fun e -> e.SelfVisit aux))
            let newVars = q.Variables |> List.filter (fun v -> not (vars.ContainsKey v.UniqueId))
            makeQuantifiedVarsUnique (Quant ({ ec with Token = t.Token }, { q with Body = e ; Variables = newVars }))
          match splitConjunctionEx true q.Body with
            | [] -> die()
            | [_] -> Some (Quant (ec, q))
            | x :: xs -> 
              Some (List.fold mkAnd (mkQ x) (List.map mkQ xs))
        | CallMacro (_, "_vcc_split_conjunctions", [], [e]) -> Some(e) // strip nested occurrences of split_conjunctions
        | _ -> None
      function 
        | CallMacro (_, "_vcc_split_conjunctions", [], [e]) ->
          Some (e.SelfMap aux)
        | _ -> None      
         
    // ============================================================================================================
    
    let splitConjunctionsInAssertions self = 
      function 
        | Assert (c, e, trigs) -> 
          let exprs = splitConjunctionEx true (self e)
          if exprs.IsEmpty then die()          
          Some (Expr.MkBlock (exprs |> List.map (fun e -> Assert ({ e.Common with Type = Type.Void }, e, trigs))))
        | _ -> None      
         
    // ============================================================================================================
    
    let handleClaims self = function
      | Call (c, ({ Name = ("_vcc_claim"|"_vcc_upgrade_claim" as name) } as fn), _, args) ->
        match List.rev args with
          | [_] ->
            helper.Error(c.Token, 9710, "claim(...) requires at least one object and a claimed property")
            None
          | x :: xs ->
            if (x.Type._IsPtr) then
              helper.Error(x.Token, 9711, "claimed property must not be of pointer type")
            Some (self (Macro (c, name.Replace("_vcc_", ""), Expr.Pure (x.Common, convertToBool (fun x -> x) x) :: (List.rev xs))))
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
    
    let fixupOld (ctx:ExprCtx) self = function
      | Old(_, (CallMacro(_, "_vcc_by_claim", _, _)), _) -> None
      | Old (c, _, _) as o when not ctx.IsPure -> Some (Expr.Pure (c, o))
      | _ -> None
    
    // ============================================================================================================

    let addExplicitReturn =
      let doDecl = function
        | Top.FunctionDecl ({ RetType = Void; Body = Some b } as f) ->
          let rec addIt = function
            | Block (c, lst, cs) as e ->
              let rec repl acc = function
                | [x] -> Block (c, List.rev (addIt x :: acc), cs)
                | x :: xs -> repl (x :: acc) xs
                | [] -> Block (c, [], cs)
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
                            Triggers = triggers1;      
                            Condition = None;
                            Body = Expr.Quant (_, ({ Kind = k2;                                                   
                                                     Triggers = triggers2; 
                                                     Variables = vars2 } as q)) } ) when k1 = k2 ->          
          match triggers1, triggers2 with
            | [], t
            | t, [] -> Some (self (Expr.Quant (ec, { q with Variables = vars1 @ vars2; Triggers = t })))
            | _ -> None

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
        
        | Expr.Prim (c, (Op(("<<"|">>"), _) as op), [e1; Cast(_,_, e2)]) ->
          Some(self (Expr.Prim(c, op, [e1; e2])))
        
        | Expr.Prim (c, (Op (("&"|"|"|"^"), _) as op), [e1; e2]) when e1.Type = Bool || e2.Type = Bool ->
          let toInt (e:Expr) = 
            if e.Type = Bool then
              Cast ({ e.Common with Type = Integer IntKind.Int32 }, Processed, e)
            else e
          let c = if c.Type = Bool then { c with Type = Integer IntKind.Int32 } else c
          Some (self (Expr.Prim (c, op, [toInt e1; toInt e2])))
        
        // this is an easy one ;-)
        | Expr.Prim (_, Op ("+", _), [e]) -> Some (self e)
        | Expr.Macro (c, "stack_allocate_array", [s; isSpec]) -> 
          match c.Type, s with 
          | Ptr elemType, Expr.IntLiteral(_, sz) ->
            Some (Expr.Cast(c, Processed, 
                            Expr.Call({ c with Type = PhysPtr Void }, internalFunction "stack_alloc", [Type.Array(elemType, (int)sz)], [Expr.Macro(bogusEC, "stackframe", []); isSpec]))) 
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
        | Expr.Macro (c, "&", [This(c')]) when not c'.Type._IsPtr ->
          Some (self (This(c)))
        | CallMacro (c, "_vcc_create_set", _, pars) ->
          match pars with
            | [] 
            | [_] -> Some(Expr.Macro(c, "_vcc_set_empty", []))
            | _ :: rest ->
              let mkSingleton (e:Expr) = 
                match e.Type with 
                  | Type.ObjectT 
                  | Type.PhysPtr _
                  | Type.SpecPtr _
                  | Type.Claim -> ()
                  | _ -> helper.Error(e.Token, 9706, "Invalid type '" + e.Type.ToString() + "' in SET expression.")
                Expr.Macro({e.Common with Type = c.Type}, "_vcc_set_singleton", [e])
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

    let handleTriggerHints self = 
      let getLabel = function
        | Macro (_, "labeled_expr", [Label (_, n); e]) -> Some (n.Name, e)
        | _ -> None
      let isLabeled = getLabel >> Option.isSome
      let fixOneTrigger = function
        | x :: xs ->
          if List.exists isLabeled xs then
            helper.Error (x.Token, 0, "only the first expression in trigger is allowed to have label")
          match getLabel x with
            | Some ("hint", x) ->
              (x :: xs) |> List.map (fun e -> [Expr.Macro (e.Common, "trigger_hint", [e])])
            | Some ("level", (IntLiteral (_, n) as e)) when xs.IsEmpty ->
              [[Expr.Macro (x.Common, "trigger_level", [e])]]
            | Some (name, e) ->
              helper.Error (x.Token, 0, "unrecongized quantifier trigger hint")
              []
            | None ->
              [x :: xs]
        | [] -> die()
              
      function
        | Expr.Quant (ec, q) ->
          Some (Expr.Quant (ec, { q with Triggers = List.collect fixOneTrigger q.Triggers; Body = self q.Body}))
        | _ -> None

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
              | CallMacro (c, "_vcc_ref_cnt", _, [p]) ->
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
          if hasCustomAttr "atomic_inline" fd.CustomAttr then
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
        
      let rec _inline inSpec self = function
        | Macro(ec, "spec", [body]) -> Some(Macro(ec, "spec", [body.SelfMap(_inline true)]))
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
              [VarDecl (cvoid, tmp, []);
               Macro (cvoid, "=", [ Ref ({ c with Type = tmp.Type }, tmp); actual ])]
          let bindOut (formal:Variable) = function
            | Macro(_, "out", [actual]) ->
              let c = actual.Common
              let cvoid = { c with Type = Void }
              let tmp = getTmp helper formal.Name formal.Type VarKind.SpecLocal
              map.Add (formal, Ref (c, tmp))
              VarDecl (cvoid, tmp, []), Expr.SpecCode(Macro (cvoid, "=", [ actual; Ref ({ c with Type = tmp.Type }, tmp)]))
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
          let resVar = getTmp helper "res" f.RetType (if f.IsSpec || inSpec then VarKind.SpecLocal else VarKind.Local)
          let subst self = function
            | Ref (_, v)  ->
              match map.TryGetValue(v) with
                | true, v' -> Some (map.[v])
                | false, _ -> None
            | VarDecl (_, v, _) when map.ContainsKey v ->
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
              [VarDecl ({c with Type = Void}, resVar, []); body] @ outParAssignmentOnExit @ [Ref (c, resVar)]
          Some (Expr.MkBlock (inits @ declsForOutPars @ body))
        | d -> None
              
      let decls = List.filter isntInline decls      
      decls |> deepMapExpressions (_inline false)
   
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
              [Expr.SpecCode(ghostUpdate.SelfMap fixupResult)]
        let stmts = VarDecl(cvoid, tmp, []) :: Atomic(cvoid, atomicParameters, Expr.MkBlock (op':: ghostUpdate')) :: [res]
        Some(Block(ec, stmts, None))
      | _ -> None
    
    // ============================================================================================================

    let normalizeSkinnyExpose self = function
      | Macro (ec, "while", [Macro (_, "loop_contract", contract); CallMacro (_, "_vcc_skinny_expose", _, objects); body]) ->
        let ptrsetEC = { bogusEC with Type = Type.PtrSet }
        let empty = Macro (ptrsetEC, "_vcc_set_empty", [])
        let single e = Macro (ptrsetEC, "_vcc_set_singleton", [e])
        let union a b = Macro (ptrsetEC, "_vcc_set_union", [a; b])
        let extractWrites acc = function
          | Assert (_, Macro (_, "loop_writes", [e]), []) -> e :: acc
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
        let saveState = [VarDecl (bogusEC, prestateVar, []); Expr.SpecCode(Macro (bogusEC, "=", [prestate; nowstate]))]
        let postUnwrapVar = getTmp helper "postUnwrap" Type.MathState VarKind.SpecLocal
        let postUnwrap = mkRef postUnwrapVar
        let savePostUnwrapState = [VarDecl (bogusEC, postUnwrapVar, []); Expr.SpecCode(Macro (bogusEC, "=", [postUnwrap; nowstate]))]
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
          (List.rev objects |> List.fold introduceWrapUnwrap (savePostUnwrapState @ [self body] @ writesCheck)) @
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
        | Type.Ref(td) when hasCustomAttr "record" td.CustomAttr -> true
        | _ -> false
        
      let foldBackFieldAssignments ec tmp =
        let rec buildDotExpr tgt = function
          | Ref(_,v) when v = tmp -> tgt
          | Deref(_, Dot(_, Macro(_, "&", [e]), f)) ->
            Expr.MkDot(buildDotExpr tgt e, f)
          | _ -> die()
        let rec foldBackFieldAssignments' (tgt : Expr) = function
          | Macro(_, "=", [Ref(_,v); e]) when v = tmp -> e
          | Macro(_, "=", [fieldExpr; e]) -> 
            Macro({tgt.Common with Type = tgt.Type}, "vs_updated", [buildDotExpr tgt fieldExpr; self e])
          | Block(_, stmts, _) -> recurse tgt stmts
          | _ -> tgt
        and recurse = List.fold foldBackFieldAssignments' 
        recurse (Macro({ec with Type = Type.MathStruct}, "vs_zero",  []))
        
      function
        | Block(ec, stmts, _) when shouldHandle (ec.Type) ->
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
        | Call (c, ({ Name = "_vcc_from_bytes" } as fn), _, [CallMacro (_, "_vcc_as_array", [], [arg; sz]) as arr; preserveZero]) ->
          let eltSz =
            match arg.Type with
              | Ptr t -> mkInt t.SizeOf
              | _ ->
                helper.Error (c.Token, 9684, "wrong type of object in from_bytes(as_array(...))", None)
                mkInt 1
          let sz = Prim (sz.Common, Op("*", Processed), [eltSz; sz])
          Some (Stmt (c, Call (c, fn, [], [asArray sz arg; typeId arr; preserveZero])))
        
        | Call (c, ({ Name = "_vcc_from_bytes" } as fn), _, [obj; preserveZero]) ->
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
          Some (Stmt (c, Call (c, fn, [], [asArray (mkInt sz) obj; typeId obj; preserveZero])))
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
          | Expr.VarDecl(ec, {Name = name; Type = TypeVar({Name = tvName}); Kind = (Local|SpecLocal) }, _) ->
            helper.Error(ec.Token, 9693, "Cannot declare local '" + name + "' of generic type '" + tvName + "'")
            Some(bogusExpr)
          | Expr.Call(ec,{TypeParameters = tpars}, targs, _) as e -> 
            errorForVarType e
          | e -> errorForVarType e

      for d in decls do
        match d with 
          | Top.FunctionDecl(fn) ->
            let reportErrorIfGeneric prefix t =
              match t with 
                | TypeVar({Name = tvName}) ->
                  helper.Error(fn.Token, 9693, "Cannot " + prefix + " of generic type '" + tvName + "'")
                | _ -> ()
          
            List.iter (fun (v : Variable) -> reportErrorIfGeneric ("declare parameter '" + v.Name + "'") v.Type) (fn.Parameters)
            reportErrorIfGeneric "return value" (fn.RetType)
          | _ -> ()
          
      decls |> deepMapExpressions reportGenericsErrors' 
    // ============================================================================================================

    let normalizeUse self = function
      | CallMacro(ec, "_vcc_use", _, [lbl; e]) ->
        let rec normalizeLabel = function
          | Cast(_, _, Macro(_, "&", [Macro(_, "string", [lbl])])) -> lbl
          | _ -> die()
        Some(Macro(ec, "_vcc_use", [normalizeLabel lbl; self e]))
      | CallMacro(ec, ("_vcc_in_domain"|"_vcc_in_vdomain" as fn), [], [e1; e2]) ->
        let e2' = self e2
        match self e1 with
          | Macro(uc, "_vcc_use", [UserData(_, (:? string as lbl)); e1']) ->
            let lbls = [for s in lbl.Split('|') -> s]
            let mkInDomain l = Macro(ec, fn, [Macro(uc, "_vcc_use", [Expr.ToUserData(l); e1']); e2'])
            let mkAnd c1 c2 = Expr.Prim(ec, Op("&&", Unchecked), [c1; c2])
            Some(List.fold (fun expr l -> mkAnd expr (mkInDomain l)) (mkInDomain lbls.Head) lbls.Tail)
          | e1' -> Some(Macro(ec, fn, [e1'; e2']))
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

    let normalizeOnUnwrap = 
      let expandOne = function
         | BoolOp (c1, "||", Prim(_, Op("==", _), [ COld (_, CallMacro (_, "_vcc_closed", _, [This(_)])) as theOld;
                                               CallMacro (_, "_vcc_closed", _, [This(_)]) as theNew]), body) ->
           mkAnd (Prim (c1, Op("==>",Unchecked), [mkAnd theOld (mkNot theNew); body]))
                 (Prim (c1, Op("==>",Unchecked), [mkAnd (mkNot theOld) theNew; body]))
         | expr -> 
           expr
         
      mapInvariants expandOne
                                           
    // ============================================================================================================
    
    let normalizeNewSyntax = 
  
      let newToOldFn = Map.ofList [ 
                                    "\\at",                  "_vcc_in_state"
                                    "\\now",                 "_vcc_current_state"
                                    "\\mine",                "_vcc_keeps"
                                    "\\embedding",           "_vcc_emb"
                                    "\\simple_embedding",    "_vcc_simple_emb"
                                    "\\ghost",               "_vcc_is_ghost_ptr"
                                    "\\fresh",               "_vcc_is_fresh"
                                    "\\thread_local",        "_vcc_thread_local2"
                                    "\\thread_local_array",  "_vcc_is_thread_local_array"
                                    "\\mutable_array",       "_vcc_is_mutable_array"
                                    "\\claims_object",       "_vcc_claims_obj"
                                    "\\claimable",           "_vcc_is_claimable"
                                    "\\make_claim",          "_vcc_claim"
                                    "\\destroy_claim",       "_vcc_unclaim"
                                    "\\active_claim",        "_vcc_valid_claim"
                                    "\\atomic_object",       "_vcc_is_atomic_obj"
                                    "\\universe",            "_vcc_set_universe" 
                                    "\\by_claim_wrapper",    "_vcc_by_claim"
                                    "\\alloc",               "_vcc_spec_alloc"
                                    "\\alloc_array",         "_vcc_spec_alloc_array"
                                    "\\heap_alloc",          "_vcc_alloc"
                                    "\\when_claimed_marker", "_vcc_when_claimed"
                                    "\\extent_fresh",        "_vcc_extent_is_fresh"
                                    "\\malloc_root",         "_vcc_is_malloc_root"
                                    "\\object_root",         "_vcc_is_object_root"
                                    "\\deep_eq",             "_vcc_deep_struct_eq"
                                    "\\shallow_eq",          "_vcc_shallow_struct_eq"
                                  ]

      let newToOldType = Map.ofList [ "\\objset", "ptrset"
                                      "\\state",  "state_t"
                                      "\\type",   "typeid_t"
                                      "\\thread_id", "thread_id" ]

      let fnMap = new Dict<_,_>()

      let normalizeCallsAndFindKeyFunctions = function
        | Top.TypeDecl(td) as decl ->
          let normalizeMine self = function
            | Call(ec, ({Name = "\\mine"} as fn), [], args) -> Some(Call(ec, fn, [], Expr.This({ec with Type = Type.MkPtrToStruct(td)}) :: List.map self args))
            | _ -> None
          deepMapExpressions normalizeMine [decl] |> List.head
        | Top.FunctionDecl({Name = ("\\set_closed_owner"|"\\giveup_closed_owner"|"\\set_owns")} as fn) as decl -> 
          fnMap.Add(fn.Name, fn)
          decl
        | decl -> decl

      let removeMagicEntities = 
        let isNotMagic = function 
          | Top.Global({Name = "\\me"}, _) 
          | Top.TypeDecl({Name = "\\TypeState"}) -> false
          | _ -> true
        List.filter isNotMagic

      let normalizeInDomain self = function
        | Call(ec, {Name = "\\set_in"}, [], [e1; Call(_, {Name = "\\domain"}, [], [e2])])
          -> Some(Macro(ec, "_vcc_in_domain", [self e1; self e2]))
        | _ -> None

      let normalizeSignatures self =
        let selfs = List.map self
        function
          | Call(ec, ({Name = "\\make_claim"} as fn), [], [Macro(_, "set", elems); cond]) -> Some(Call(ec, fn, [], selfs elems @ [cond]))
          | Call(ec, ({Name = "\\destroy_claim"} as fn), [], [cl; Macro(_, "set", elems)]) -> Some(Call(ec, fn, [], selfs (cl :: elems)))
          | Call(ec, ({Name = "\\upgrade_claim"} as fn), [], [Macro(_, "set", claimsSet); prop]) -> Some(Call(ec, fn, [], selfs (claimsSet @ [prop])))
          | Call(ec, ({Name = "\\claimable"} as fn), [], [e]) -> Some(Call(ec, fn, [], [Macro({e.Common with Type = Type.TypeIdT}, "_vcc_typeof", [self e])]))
          | Call(ec, ({Name = "\\havoc_others"} as fn), [], [e]) -> 
            let e' = self e
            Some(Macro(ec, "_vcc_havoc_others", [e'; Macro({e'.Common with Type = Type.TypeIdT}, "_vcc_typeof", [e'])]))
          | _ -> None

      let rec normalizeOwnershipManipulation inAtomic self = 
        let selfs = List.map self
        function
          | Atomic(ec, objs, body) -> Some(Atomic(ec, selfs objs, body.SelfMap(normalizeOwnershipManipulation true)))
          | Macro(ec, "=", [Macro(_, "_vcc_owns", [e1]); 
                            Call(_, {Name = ("\\set_add_element"|"\\set_remove_element" as setOp)}, [], 
                                 [Macro(_, "_vcc_owns", [e1']); e2] )]) when inAtomic  -> 
              let fn = if setOp = "\\set_add_element" then fnMap.["\\set_closed_owner"] else fnMap.["\\giveup_closed_owner"]
              Some(Call({ec with Type = Type.Void}, fn, [], [self e2; self e1]))
          | Macro(ec, "=", [Macro(_, "_vcc_owns", [e1]); e2]) -> Some(Call({ec with Type = Type.Void}, fnMap.["\\set_owns"], [], [self e1; self e2]))
          | _ -> None

      let normalizeMisc self = 
        let selfs = List.map self
        function
          | Macro(ec, "set", elems) -> Some(Macro(ec, "_vcc_create_set", Expr.Bogus :: selfs elems))
          | Macro(ec, "\\is", [arg;UserData(_, (:? Type as t))]) -> Some(Macro(ec, "_vcc_is", [self arg; typeExpr t]))
          | Ref(ec, {Name = "\\me"}) -> Some(Macro(ec, "_vcc_me", []))
          | _ -> None

      let normalizeMacros self = function
        | Macro(ec, m, args) -> 
          match Map.tryFind m newToOldFn with
            | Some m' -> Some(Macro(ec, m', List.map self args))
            | None -> None
        | _ -> None

      let mapFromNewSyntax = function
        
        | Top.FunctionDecl(fn) when fn.Name.StartsWith("\\macro") -> ()
        | Top.FunctionDecl(fn) when fn.Name.StartsWith "\\" ->
          match newToOldFn.TryFind fn.Name with
            | Some oldName -> fn.Name <- oldName
            | None -> fn.Name <- "_vcc_" + fn.Name.Substring(1)
        | Top.TypeDecl(td) ->
          match newToOldType.TryFind td.Name with
            | Some oldName -> td.Name <- oldName
            | None -> ()
        | _ -> ()
      
      let rewriteBvAssertAsBvLemma self = function
        | Assert(ec, expr, []) -> None
        | Assert(ec, expr, [[Macro(_, "labeled_expr", [Label(_, {Name = "bv"})])]]) -> 
          let makeUnchecked self = function
            | Prim(ec, Op(_, CheckedStatus.Unchecked), _) -> None
            | Prim(ec, Op(op, _), args) -> Some(Prim(ec, Op(op, CheckedStatus.Unchecked), List.map self args))
            | _ -> None
          Some(Assert(ec, Expr.Macro(expr.Common, "_vcc_bv_lemma", [expr.SelfMap(makeUnchecked)]), []))
        | Assert(ec, expr, trigs) -> 
          helper.Error(ec.Token, 9713, "unhandled triggers on assert")
          Some(Assert(ec, expr, []))
        | _ -> None
          
      let normalizeGroupInvariants decls = 

        let addGroupToMap (map : Map<_,_>) = 
          let rec getGroupNameFromAttrs = function
          | [] -> None
          | VccAttr(AttrGroupDecl, name) :: _ -> 
            Some name
          | _ :: attrs -> getGroupNameFromAttrs attrs

          function
          | Top.TypeDecl(td) ->
            match getGroupNameFromAttrs td.CustomAttr with
              | Some name -> 
                let parent = td.Parent.Value.UniqueId
                match map.TryFind parent with
                  | None -> map.Add(parent, Set.singleton name)
                  | Some groups -> map.Add(parent, Set.add name groups)
              | None -> map
          | _ -> map
                
        let groups = List.fold addGroupToMap Map.empty decls
      
        let doIt = function
          | Top.TypeDecl(td) as top -> 
            let groupsOfTd = match groups.TryFind td.UniqueId with | Some groupsOfTd -> groupsOfTd | None -> Set.empty
            let ngi (groupsOfTd : Set<_>) self = function
              | Macro(ec, "labeled_invariant", ([Macro(_, lbl, _); i] as args)) when groupsOfTd.Contains lbl -> Some(Macro(ec, "group_invariant", args))
              | _ -> None
            td.Invariants <- List.map (fun (expr:Expr) -> expr.SelfMap (ngi groupsOfTd)) td.Invariants
          | top -> ()

        List.iter doIt decls
        decls

      deepMapExpressions normalizeInDomain >> 
      List.map normalizeCallsAndFindKeyFunctions >> 
      normalizeGroupInvariants >>
      deepMapExpressions (normalizeOwnershipManipulation false) >>
      deepMapExpressions normalizeSignatures >> 
      (fun decls -> List.iter mapFromNewSyntax decls; decls)>> 
      deepMapExpressions normalizeMacros >>
      removeMagicEntities >>
      deepMapExpressions normalizeMisc >> 
      deepMapExpressions rewriteBvAssertAsBvLemma

    // ============================================================================================================
 
    let expandContractMacros decls =
      let isMacro (f:Function) = f.Name.StartsWith "\\macro_"
      let isMacroCall = function
        | Call (_, f, _, _) -> isMacro f
        | Macro(_, "ite", [Cast(_, _, Call(_, f, _, _)); BoolLiteral(_, true); BoolLiteral(_, false)]) -> isMacro f
          // this is how non-bool macros are projected
        | _ -> false
      let expand = function
        | Top.FunctionDecl f ->
          let (macros, reqs) = List.partition isMacroCall f.Requires
          f.Requires <- reqs          
          let handleExpansion = function
            | Call (ec, m, targs, args) as call
            | Macro(_, "ite", [Cast(_, _, (Call(ec, m, targs, args) as call)); BoolLiteral(_, true); BoolLiteral(_, false)]) ->
              let m' = m.Specialize(targs, false)
              let subst = inlineCall "_(" (fun x -> x) (Call({ec with Type=m'.RetType}, m', [], args))
              let substs = List.map subst
              f.Requires <- f.Requires @ substs m'.Requires
              f.Ensures <- f.Ensures @ substs m'.Ensures
              f.Writes <- f.Writes @ substs m'.Writes
              f.Reads <- f.Reads @ substs m'.Reads
              f.Variants <- f.Variants @ substs m'.Variants
            | _ -> die()
          List.iter handleExpansion macros
        | _ -> ()
      let isntMacro = function
        | Top.FunctionDecl f when isMacro f -> false
        | _ -> true
      let decls = decls |> List.filter isntMacro
      List.iter expand decls
      decls
    
    // ============================================================================================================

    let normalizeBvLemma decls =

      let lemmaCheckFunctionDecls = ref []

      let extractBvLemmas (fn : Function) = 
        let idCount = ref -1
        let checkFunctionFor (lemma:Expr) = 
          { Token = lemma.Token
            IsSpec = true
            OrigRetType = Type.Void
            RetType = Type.Void
            Parameters = []
            TypeParameters = []
            Name = fn.Name + "#bv_lemma#" + (incr idCount; (!idCount).ToString())
            Requires = []
            Ensures = []
            Writes = []
            Variants = []
            Reads = []
            CustomAttr = [VccAttr (AttrIsPure, ""); VccAttr(AttrBvLemmaCheck, "true")]
            Body = Some (Expr.Block(lemma.Common, [lemma], None))
            IsProcessed = true
            UniqueId = CAST.unique() } : Function
        let findAmdExtractThem _ = function
          | Expr.Assert(ec, CallMacro(_, "_vcc_bv_lemma", _, _), _) as _assert ->
            let checkFn = checkFunctionFor _assert
            lemmaCheckFunctionDecls := Top.FunctionDecl(checkFn) :: !lemmaCheckFunctionDecls
            Some(Expr.Comment(ec, "bv_lemma extracted into " + checkFn.Name))
          | _ -> None
        fn.Body <- Option.map (fun (e : Expr) -> e.SelfMap(findAmdExtractThem)) fn.Body

      for d in decls do
        match d with
          | Top.FunctionDecl f -> extractBvLemmas f
          | _ -> ()

      decls @ List.sortBy (fun top -> match top with | Top.FunctionDecl(fn) -> fn.Name | _ -> die()) !lemmaCheckFunctionDecls

    // ============================================================================================================

    let embedStackArrays decls =

      let embeddingStructs = ref []

      

      let findStackArrays stmts =
        let stackArrays = ref []
        let findStackArraysInBlock' self = function
          | Expr.Macro(_, "=", [Expr.Ref(_,var); Macro(_, "stack_allocate_array", [IntLiteral(_, size); BoolLiteral(_, isSpec)])]) -> 
            match var.Type with
              | Ptr t -> stackArrays := SAR(var, t, (int)size, isSpec) :: !stackArrays; true
              | _ -> false
          | Expr.Block _ -> false
          | _ -> true
        List.iter (fun (e:Expr) -> e.SelfVisit(findStackArraysInBlock')) stmts
        !stackArrays

      let doFunction (fn:Function) =
        let embCount = ref 0
        let varToDeclMap = new Dict<_,_>()

        let fillVarToDeclMap self = function
          | VarDecl(_,v,_) as decl -> varToDeclMap.Add(v, decl); false
          | _ -> true
             
        let findAndEmbedStackArrays self = function
          | Expr.Block(ec, stmts, bc) ->
            let stackArrays = findStackArrays stmts
            if stackArrays.Length = 0 then None
            else
              let fieldMap = new Dict<_,_>()
              let createField (td:TypeDecl) offset = function
                | SAR(var, t, size, isSpec) ->
                  let asArray =
                    match varToDeclMap.TryGetValue var with
                      | true, VarDecl(_,_,attr) when hasCustomAttr "as_array" attr -> [VccAttr("as_array", "")]
                      | _ -> []
                  let f = { Token = td.Token
                            Name = var.Name
                            Type = Type.Array(t, size)
                            Parent = td
                            IsSpec = isSpec
                            IsVolatile = false
                            Offset = FieldOffset.Normal(offset)
                            CustomAttr = asArray
                            UniqueId = CAST.unique() } : Field
                  fieldMap.Add(var, f)
                  f

              let rec createFields td offset acc = function
                | [] -> List.rev acc
                | sad :: sads ->
                  let f = createField td offset sad
                  createFields td (offset + f.Type.SizeOf) (f::acc) sads 

              let embTd = { 
                Token = fn.Token
                Kind = TypeKind.Struct
                Name = fn.Name + "stackframe#" + (!embCount).ToString()
                Fields = []
                Invariants = []
                CustomAttr = []
                SizeOf = 0
                IsNestedAnon = false
                GenerateEquality = StructEqualityKind.NoEq
                GenerateFieldOffsetAxioms = false
                IsSpec = false
                Parent = None
                IsVolatile = false
                UniqueId = CAST.unique() } : TypeDecl

              let fields = createFields embTd 0 [] (List.rev stackArrays)
              let sizeof = List.fold (fun size (f:Field) -> size + f.Type.SizeOf) 0 fields
              embTd.Fields <- fields
              embTd.SizeOf <- sizeof
              embeddingStructs := Top.TypeDecl(embTd) :: !embeddingStructs

              let embVar = { Name = "#stackframe#" + (!embCount).ToString()
                             Type = Type.Ref(embTd)
                             Kind = VarKind.Local
                             UniqueId = CAST.unique() } : Variable
              let embVarDecl = Expr.VarDecl(bogusEC, embVar, [])
              let embVarPtr = Expr.Macro({bogusEC with Type = Type.PhysPtr (embVar.Type)}, "&", [Expr.Ref({bogusEC with Type = embVar.Type}, embVar)])
              incr embCount

              let addAssignments self =  function
                | Expr.Macro(ec, "=", [Expr.Ref(_,var) as vr; Macro(_, "stack_allocate_array", _)]) ->
                  match fieldMap.TryGetValue var with
                    | true, f -> Some(Expr.Macro(ec, "=", [vr; Expr.MkDot(vr.Common, embVarPtr, f)]))
                    | _ -> None
                | _ -> None

              let stmts' = stmts |> List.map self |> List.map (fun (e:Expr) -> e.SelfMap(addAssignments))

              Some(Expr.Block(ec, embVarDecl :: stmts', bc))

          | _ -> None

        match fn.Body with
          | Some body -> 
            body.SelfVisit(fillVarToDeclMap)
            fn.Body <- Some(body.SelfMap(findAndEmbedStackArrays))
          | None -> ()
          

      for d in decls do
        match d with
          | Top.FunctionDecl fn -> doFunction fn
          | _ -> ()

      decls @ !embeddingStructs

    // ============================================================================================================

    helper.AddTransformer ("norm-begin", Helper.DoNothing)
    helper.AddTransformer ("norm-new-syntax", Helper.Decl normalizeNewSyntax)
    helper.AddTransformer ("norm-expand-contract-macros", Helper.Decl expandContractMacros)
    helper.AddTransformer ("norm-initializers", Helper.Expr normalizeInitializers)
    helper.AddTransformer ("norm-use", Helper.Expr normalizeUse)
    helper.AddTransformer ("norm-fixed-array-parms", Helper.Decl removeFixedSizeArraysAsParameters)
    helper.AddTransformer ("norm-inline-array-accesses", Helper.Expr normalizeInlineArrayAccesses)
    helper.AddTransformer ("norm-out-params", Helper.Expr removeDerefFromOutParams)
    helper.AddTransformer ("norm-comparison", Helper.Expr (doHandleComparison helper))
    helper.AddTransformer ("norm-conversions", Helper.Expr doHandleConversions)   
    helper.AddTransformer ("norm-ptr-comparison", Helper.Decl (handlePurePtrEq helper))
    helper.AddTransformer ("inline-spec-macros", Helper.Decl inlineSpecMacros)
    helper.AddTransformer ("norm-generic-errors", Helper.Decl reportGenericsErrors) 
    helper.AddTransformer ("add-assume-to-assert", Helper.Expr handleLemmas)    
    helper.AddTransformer ("fixup-old", Helper.ExprCtx fixupOld)    
    helper.AddTransformer ("fixup-claims", Helper.Expr handleClaims)    
    helper.AddTransformer ("add-explicit-return", Helper.Decl addExplicitReturn)
    helper.AddTransformer ("norm-embed-stack-arrays", Helper.Decl embedStackArrays)
    helper.AddTransformer ("norm-atomic-ops", Helper.Expr normalizeAtomicOperations)
    helper.AddTransformer ("norm-skinny-expose", Helper.Expr normalizeSkinnyExpose)
    helper.AddTransformer ("norm-bv_lemma", Helper.Decl normalizeBvLemma)
    helper.AddTransformer ("norm-misc", Helper.Decl miscNorm)
    helper.AddTransformer ("norm-quant-triggers", Helper.Expr handleTriggerHints)
    helper.AddTransformer ("deep-split-conjunctions", Helper.Expr deepSplitConjunctions)
    helper.AddTransformer ("split-assertions", Helper.Expr splitConjunctionsInAssertions)
    helper.AddTransformer ("norm-writes", Helper.Decl normalizeWrites)
    helper.AddTransformer ("norm-atomic-inline", Helper.Decl inlineAtomics)
    helper.AddTransformer ("norm-reintp", Helper.Expr normalizeReinterpretation)
    helper.AddTransformer ("norm-on-unwrap", Helper.Decl normalizeOnUnwrap)
    helper.AddTransformer ("norm-end", Helper.DoNothing)
