
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
 
 module ToCoreC =
 
  // ============================================================================================================
  let rec linearize (helper:Helper.Env) enclosing ctx _ (expr:Expr) = 
    let selfe prev (expr:Expr) =
      let enclosing = ref prev
      let res = expr.SelfCtxMap (false, linearize helper (Some enclosing))
      (!enclosing, res)
    let selfp (expr:Expr) = expr.SelfCtxMap (true, linearize helper None)
    let selfs (expr:Expr) = expr.SelfCtxMap (false, linearize helper None) 
    let pureCheck () =
      // this a low quality error, if we already given some error, giveup on this one
      if ctx.IsPure && not helper.ErrorReported then
        helper.Error (expr.Common.Token, 9614, "expression with side effects used in pure context " + expr.ToString(), None)
    let ret lst =
      pureCheck ()
      match enclosing with
        | Some enc -> 
          enc := lst @ !enc
          Some (Macro (voidBogusEC(), "ignore_me", []))
        | None -> Some (Expr.MkBlock (List.rev lst))

    match expr with
      | Macro(cmn, "inlined_atomic", [e]) ->
        let (prev, e1) = selfe [] e
        ret (Macro(cmn, "inlined_atomic", [e1]) :: prev)
      
      | Expr.Ref _
      | Prim _
      | IntLiteral _
      | BoolLiteral _
      | Deref _
      | Dot _
      | Index _
      | Cast _
      | Quant _
      | Result _
      | Old _
      | Call _
      | Pure _
      | UserData _
      | SizeOf _
      | This _
      | Macro _ -> None
      
      | If (c, cond, th, el) ->
        let (prev, cond) = selfe [] cond
        ret (If (c, cond, selfs th, selfs el) :: prev)
      
      | Stmt (c, e) ->
        let (prev, e) = selfe [] e
        ret (Stmt (c, e) :: prev) 
        
      | VarWrite (c, v, e) ->
        let (prev, e) = selfe [] e
        ret (VarWrite (c, v, e) :: prev)
        
      | MemoryWrite (c, e1, e2) ->
        let (prev, e1) = selfe []   e1
        let (prev, e2) = selfe prev e2
        ret (MemoryWrite (c, e1, e2) :: prev)
      
      | Return (c, Some e) ->
        let (prev, e) = selfe [] e
        ret (Return (c, Some e) :: prev)
        
      // expressions with only pure expressions or statements inside
      | Loop _
      | Goto _
      | Label _
      | Assert _
      | Assume _ 
      | VarDecl _
      | Atomic _
      | Comment _
      | Return (_, None) ->
        match enclosing with
          | None -> pureCheck (); None
          | Some _ -> ret [selfs expr]      
        
      | Block (c, stmts) ->
        pureCheck ()
        match enclosing with
          | None -> None
          | Some enc ->
            let self (expr:Expr) = expr.SelfCtxMap (false, linearize helper enclosing) 
            let rec loop = function
              | [last] -> Some (self last)
              | x :: xs ->
                match self x with
                  | Expr.Ref _
                  | Macro (_, "ignore_me", _) -> ()
                  | expr -> enc := expr :: !enc
                loop xs
              | [] -> Some (self (Macro (c, "ignore_me", [])))
            loop stmts

  let linearizeDecls helper decls =
    for d in decls do
      match d with
        | FunctionDecl ({ Body = Some b } as h) -> 
          h.Body <- Some (b.SelfCtxMap (false, linearize helper None))
        | _ -> ()
    decls
  // ============================================================================================================  

  let handlePureCalls (helper : Helper.Env) self = 
  
    if helper.Options.Vcc3 then
      Simplifier.alwaysPureCalls.["_vcc_span"] <- "Sp"    

    let ecState = { bogusEC with Type = Type.MathState }
    let nowState = Expr.Macro(ecState, "state", [])
    let oldState = Expr.Old(ecState, Macro(ecState, "prestate", []), nowState)
    
    let firstIsState = function
        | (arg : Expr) :: _ ->
          match arg.Type with
            | MathTypeRef "state_t" -> true
            | _ -> false
        | _ -> false  
    
    let stateArgs (sgn : string) =
      let rec stateArgs' = function
        | [] -> []
        | 's' :: sgn -> oldState :: stateArgs' sgn
        | 'S' :: sgn -> nowState :: stateArgs' sgn
        | _ :: sgn -> stateArgs' sgn
      stateArgs' (Seq.toList(sgn))
    function 
      | CallMacro (c, fn, _, args) ->
        match Simplifier.alwaysPureCalls.TryGetValue(fn) with
          | true, sgn -> 
            let args = List.map self args
            let args = if firstIsState args then args else (stateArgs sgn) @ args
            Some (Expr.Macro (c, fn, args))
          | false, _ -> None
      | _ -> None
            
 
 
  let init (helper:Helper.Env) =
  
    // ============================================================================================================
               
    /// Just copy each parameter that is being written to into a local
    /// (Boogie doesn't allow writing to parameters).
    (* void f(int x) { x = 2; }
       
       void f(int x) { int x_; x_ = x; x_ = 2; }
     *)
    let doRemoveWritesToParameters (f:Function) =
      let inits = ref []
      let mapping = new Dict<_,_>()

      let handleVar (v : Variable) =
        if not (mapping.ContainsKey v) then
          let v' = { v with Name = "local." + v.Name; Kind = Local }
          mapping.Add (v, v')
          inits := VarDecl (voidBogusEC(), v') :: VarWrite(voidBogusEC(), [v'], mkRef v) :: !inits
          
      let isInParKind = function
        | VarKind.Parameter | VarKind.SpecParameter -> true
        | _ -> false

      let identify _ = function
        | VarWrite (_, vs, _) ->
          vs |> List.filter (fun v -> isInParKind v.Kind) |> List.iter handleVar
          false
        | Macro(_, "out", [Expr.Ref(_, v)]) when isInParKind v.Kind -> handleVar v; false
        | _ -> true

      let replace self = function
        | Expr.Ref (c, v) -> 
          match mapping.TryGetValue v with
            | true, v' -> Some(Expr.Ref(c, v'))
            | false, _ -> None
        | VarWrite(c, vs, e) -> 
          let vMap (v: Variable) = 
            match mapping.TryGetValue(v) with
              | true, v' -> v'
              | false, _ -> v
          Some(VarWrite(c, List.map vMap vs, self e))
        | Old(_, Expr.Macro(_, "prestate", _), (Expr.Ref(_,v) as r)) as o ->
          if mapping.ContainsKey(v) then Some(r) else Some(o)
        | Old(_, Expr.Macro(_, "prestate", _), _) as  o -> Some(o)
        | _ -> None
        
      match f.Body with
        | Some body -> 
          body.SelfVisit identify
          f.Body <- Some(body.SelfMap replace |> addStmts !inits)
        | None -> ()
      f
          
    let removeWritesToParameters = mapFunctions doRemoveWritesToParameters    
    
    // ============================================================================================================
    
    /// Remove instance of Macro("=", [x;y]) by adding appriopriate statement
    /// (MemoryWrite or VarWrite).
    let assignmentDesugaring ctx self = function
      | Expr.Macro (c, "=", [e1; e2]) ->
        if ctx.IsPure then helper.Error (c.Token, 9611, "assignment used in pure context", None)
        let (inits, tmp, finalRef) =
          match e1, c.Type with
            | _, Void -> ([], e2, [])
            | Expr.Ref _ as dst, _ -> ([], e2, [dst])
            | _ ->
              if c.Type = Void then die()
              let (inits, tmp) = lateCache helper "assign" e2 VarKind.Local
              (inits, tmp, [tmp])
        let write = 
          match e1 with
            | Expr.Ref (_, v) 
            | Expr.Deref(_, Expr.Ref(_, ({ Kind = OutParameter} as v))) -> Expr.VarWrite ({ c with Type = Void }, [v], tmp)
            | Expr.Deref (_, l) -> Expr.MemoryWrite ({ c with Type = Void }, l, tmp)
            | e -> helper.Error (e1.Token, 9612, "don't know how to write to " + e1.ToString (), None); e
        Some (self (Expr.MkBlock (inits @ [write] @ finalRef))) 
      | _ -> None    
    
    // ============================================================================================================
    
    /// Replace calls to Boogie predicates/functions with Expr.Macro(...)
    
    let handleKeeps decls = 
    
      let handleKeeps' staticOwns self = function
        | CallMacro (c, "_vcc_keeps", _, this :: []) ->
          Some (Expr.True)
          
        | CallMacro (c, "_vcc_keeps", _, this :: keeps) ->
        
          let reportErrorForNonPtrArgument (e : Expr) =
            match e.Type with
              | ObjectT
              | Claim
              | Ptr _ -> ()
              | t -> helper.Error(e.Token, 9699, "'keeps' requires arguments of pointer type; '" + e.Token.Value + "' has type '" + t.ToString() + "' which is not allowed.")
        
          let build acc (e:Expr) =
            let eq = Macro (c, "keeps_stable", [Old (e.Common, Macro (bogusEC, "prestate", []), e); e])
            let keeps = Macro (c, "keeps", [this; e])
            let both = if staticOwns then Prim (c, Op ("&&", Processed), [eq; keeps]) else keeps
            match acc with
              | Some acc ->
                Some (Prim (c, Op ("&&", Processed), [acc; both]))
              | None -> Some both
          List.iter reportErrorForNonPtrArgument keeps
          Some (self (List.fold build None keeps).Value)
          
        | _ -> None
      
      for d in decls do
        match d with
          | Top.TypeDecl(td) as top -> deepMapExpressions (handleKeeps' (staticOwns td)) [top] |> ignore
          | _ -> ()
      decls
    
    // ============================================================================================================
    
    let handleRecords (decls:list<Top>) =
      for d in decls do
        match d with
          | Top.TypeDecl td when hasBoolAttr "record" td.CustomAttr ->
            td.Kind <- TypeKind.Record
          | _ -> ()
      let isRecField (f:Field) = f.Parent.Kind = Record
      let isRecType = function | Type.Ref({Kind = Record}) -> true | _ -> false
      let replNestedDots self = function
        | Dot (c, (Dot (_, _, f1) as inner), f2) when isRecField f1 && isRecField f2 ->
          Some (self (Dot (c, Deref (inner.Common, inner), f2)))
        | _ -> None
      
      let replNestedUpdates self = function
        | Macro(ec, "vs_updated", [Dot(_, Dot(_, _, f1), f2) as nestedDots; e]) when isRecField f1 && isRecField f2 -> 
          let rec collectDots acc = function
            | Dot(_, e, f) when isRecField f1 -> collectDots (f::acc) e
            | e -> acc,e 
          let dots, inner = collectDots [] nestedDots
          let rec construct (inner:Expr) source = function
            | [] -> die()
            | [f] -> Macro(inner.Common, "vs_updated", [Expr.MkDot(inner, f); source])
            | (f:Field) :: fs -> 
              let fetch = Macro ({inner.Common with Type = f.Type}, "rec_fetch", [inner; Expr.ToUserData(f)])
              Macro(inner.Common, "vs_updated", [Expr.MkDot(inner, f); construct fetch source fs])
          Some(construct inner e dots)
        | _ -> None
      
      let replAccess self = 
        
        let normalizeRecord rt = function
          | Macro(c, "vs_zero", []) -> Macro({c with Type = rt}, "rec_zero", [])
          | Macro(c, "&", [e]) -> e
          | Dot(c,_,_) as dot -> Deref({c with Type = rt}, dot)
          | e -> e
          
        function
        | VarDecl(ec, v) as decl when (v.Kind = VarKind.SpecLocal  || v.Kind = VarKind.Local) && isRecType v.Type ->
          Some(Expr.MkBlock([decl; Expr.VarWrite(ec, [v], Macro({ec with Type = v.Type}, "rec_zero", []))]))
        | Deref (c, (Dot (_, p, f) as dot)) when isRecField f ->
          Some (self (Macro (c, "rec_fetch", [normalizeRecord (Type.Ref(f.Parent)) p; Expr.ToUserData(f)])))
        | Macro (c, name, ((Dot (_, p, f)::args))) when name.StartsWith("vs_updated") && isRecField f ->
          let update = match name with | "vs_updated" -> "rec_update" | "vs_updated_bv" -> "rec_update_bv" | _ -> die()
          Some (self (Macro ({c with Type = Type.Ref f.Parent}, update, (normalizeRecord (Type.Ref(f.Parent)) p) :: Expr.ToUserData(f) :: args)))
        | MemoryWrite (c, (Dot (_, p, f) as dot), v) when isRecField f ->
          let upd = Macro ({c with Type = Type.Ref f.Parent}, "rec_update", [normalizeRecord (Type.Ref(f.Parent)) p; Expr.ToUserData(f); v])
          let res =
            match p with
              | Deref (_, p') ->
                MemoryWrite (c, p', upd)
              | Dot (_, _, _) ->
                MemoryWrite (c, p, upd)
              | Macro (_, "&", [Ref (_, v)]) ->
                VarWrite (c, [v], upd)
              | _ ->
                helper.Error (c.Token, 9686, "invalid record field in-place update")
                Expr.MkBlock []
          Some (self res)
        | _ -> None
        
      decls |> deepMapExpressions replNestedUpdates |> deepMapExpressions replNestedDots |> deepMapExpressions replAccess

    // ============================================================================================================

    
    /// Get rid of structs that are passed by value, or who are allocated on the stack and their address is never taken
    let removeStructsPassedByValue (decls:list<Top>) =
      let localsMapping = ref (new Dict<_,_>())

      let mapLocal v = 
        match (!localsMapping).TryGetValue(v) with
          | true, v' -> v'
          | _ -> v
      
      let isValStruct = function
        | MathTypeRef "struct" -> true
        | _ -> false
        
      let isRefToStruct = function
        | Type.Ref ({ Kind = (Struct|Union) }) -> true
        | _ -> false
      
      let doVar (v:Variable) =
        if isRefToStruct v.Type then
          let v' = { v with Type = Type.MathStruct; Name = "vs." + v.Name }
          (!localsMapping).Add (v, v')
          v'           
        else v
           
      let doParms = function
        | Top.FunctionDecl h as d ->
          h.Parameters <- List.map doVar h.Parameters
          h.RetType <- 
            if isRefToStruct h.RetType then
              Type.MathStruct 
            else h.RetType
          d
        | d -> d
      
      let construct (ptr:Expr) =
        Expr.Macro ({ ptr.Common with Type = Type.MathStruct }, "_vcc_vs_ctor", [ptr])
          
      let replacedQuantVars = new Dict<_,_>()
          
      let rec fixAccess self = function
        | Quant (c, qd) -> 
          let isAdmissibleFieldType = function
            | Type.Integer _
            | Type.PhysPtr _
            | Type.SpecPtr _ -> true
            | _ -> false
          let rec mapVars = function
            | [] -> []
            | (v:Variable) :: vs ->
              let vs'= mapVars vs
              match v.Type with 
                | Type.Ref({Kind = TypeKind.Struct; Fields = [f]}) when isAdmissibleFieldType f.Type ->
                  let v' = doVar v
                  let vField = getTmp helper (v.Name + "#" + f.Name) f.Type VarKind.QuantBound
                  replacedQuantVars.Add(v', (f, vField))
                  vField::vs'
                | Type.Ref({Kind = TypeKind.Struct|TypeKind.Union}) -> 
                  helper.Error(c.Token, 9696, "Cannot quantify over structured type '" + v.Type.ToString() + "' (bound variable is '" + v.Name + "').")
                  v::vs'
                | _ -> v::vs'
          let savedLocalsMapping = !localsMapping
          localsMapping := new Dict<_,_>(!localsMapping)
          let vars = mapVars qd.Variables
          let qd' = { 
                      Kind = qd.Kind 
                      Variables = vars
                      Triggers = List.map (List.map self) qd.Triggers
                      Condition = Option.map self qd.Condition
                      Body = self qd.Body
                    } : QuantData
          localsMapping := savedLocalsMapping
          Some(Quant(c, qd'))
        | VarDecl (c, ({ Kind = Parameter|SpecParameter|OutParameter } as v)) -> Some (VarDecl (c, mapLocal v))
        | VarDecl (c, ({ Kind = Local|SpecLocal } as v)) -> Some (VarDecl (c, doVar v))
        | Deref (c, e) as expr -> 
          match underDeref e with
            | Some e ->               
              Some (Expr.Macro (c, "vs_fetch", [e]))
            | None ->
              if isRefToStruct c.Type then Some(construct (self e))
              else None       
        | Expr.Ref (c, v)  ->
          let v = mapLocal v
          Some (Expr.Ref ({c with Type = v.Type}, v))
        | Expr.Result c ->
          if isRefToStruct c.Type then
            Some (Expr.Result ({ c with Type = Type.MathStruct }))
          else None
        | Expr.Call (c, fn, targs, args) when fn.RetType = Type.MathStruct -> 
          Some (Expr.Call ({ c with Type = Type.MathStruct }, fn, targs, List.map self args))
        
        | MemoryWrite (c, dst, src) ->
          match underDeref dst with
            | Some ptr ->
              let rec findLocal = function
                | Dot (_, e, _)
                | Index (_, e, _) -> findLocal e
                | Ref (_, v) -> v
                | e ->
                  helper.Panic ("the path for value struct field update doesn't end with a local: " + ptr.ToString() + " >>> " + e.ToString())
              let loc = findLocal ptr
              let (inits, tmp) = lateCache helper "vsAssign" (self src) VarKind.Local
              let updated = Macro ({c with Type = Type.MathStruct}, "vs_updated", [ptr; tmp])
              let assump = Macro ({c with Type = Type.Bool}, "vs_can_update", [updated])
              Some (Expr.MkBlock (inits @ [Expr.MkAssume assump; VarWrite (c, [loc], updated)]))
            | None ->
              handleMemWrite self (c, dst, src)
        
        | VarWrite (c, v, src)  ->
          Some (VarWrite (c, List.map mapLocal v, self src))
                  
        | Old(c, prestate, e) -> Some( Old(c, self prestate, self e))

        | Macro(c, "map_get", args) ->
          match c.Type with
            | Type.Ref { Kind = (Struct|Union) } -> Some (Macro({ c with Type = Type.MathStruct}, "map_get", List.map self args))
            | _ -> None
            
        | Block(_,_) -> None

        | e ->
          if isRefToStruct e.Type then
            helper.Error (e.Common.Token, 9687, "cannot handle val-struct " + e.ToString(), None)
          None
        
      and handleMemWrite self (c, dst, src) =
        let src = self src
        if isValStruct src.Type then  
          let cache pre expr =
            let (inits, tmp) = lateCache helper "vsAssign" expr VarKind.Local
            (pre @ inits, tmp)
        
          match dst.Type with
            | Ptr ((Type.Ref td) as t) ->
              let (pre, dst) = cache [] (self dst)
              let (pre, src) = cache pre (self src)
              TransType.setEqualityKind td DeepEq
              let stmts =
                    pre @ 
                    [Expr.Macro(c, "havoc", [dst; typeExpr t]);
                     Expr.MkAssume (Expr.Macro ({ src.Common with Type = Bool }, 
                                               "_vcc_deep_struct_eq." + td.Name,
                                               [construct dst; src]))] 
              Some (Expr.MkBlock stmts)
            | _ -> die()
        else None 
        
      and underDeref = function
        | Dot (c, e, f) ->
          match underDeref e with
            | Some e -> Some(Dot(c, e, f))
            | None -> None
        | Index (c, e, idx) ->
          match underDeref e with
            | Some e ->
              match c.Type with
                | Ptr t ->
                  Some (Expr.Index (c, e, idx.SelfMap fixAccess))
                | _ -> die()
            | None -> None
        | Cast (c, _, e) when c.Type = e.Type -> underDeref e        
        | Expr.Macro (_, "&", [e]) ->
          let e = e.SelfMap fixAccess
          if isValStruct e.Type then Some e
          else None
        | _ -> None
        
      and moveOldOutOfCtor self = function
        | CallMacro(c, "_vcc_vs_ctor", _, [Old(_, prestate, expr)]) -> Some(Old(c, prestate, Macro(c, "_vcc_vs_ctor", [self expr])))
        | _ -> None

      let replaceStructVarAccess _ = function
        | Macro(ec, "vs_fetch", [Dot(_, Ref(_,v) , f)]) ->
          match replacedQuantVars.TryGetValue(v) with
            | true, (f', v') when f' = f -> Some(Ref(ec, v'))
            | true, (f, _) -> die()
            | false, _ -> None
        | Ref(ec, v) when replacedQuantVars.ContainsKey(v) ->
          helper.Error(ec.Token, 9704, "Cannot replace reference to quantified variable'" + v.Name + "' of structured type.")
          None
        | _ -> None
      
      decls |> List.map doParms |> deepMapExpressions fixAccess |> deepMapExpressions moveOldOutOfCtor |> deepMapExpressions replaceStructVarAccess
      
    
    
    // ============================================================================================================
    
    // Make non-pure calls statements.        
    let rec pullOutCalls ctx self = 
      let reportErrorForCallToImpureFunction (call : Expr) (fn :Function) = 
        helper.Error (call.Token, 9635, "function '" + fn.Name + "' used in pure context, but not marked with 'ispure'", Some(fn.Token))
      function
        | VarWrite (c, v, (Call (c', fn, targs, args) as call)) -> 
          let processed = VarWrite (c, v, Call (c', fn, targs, List.map self args))
          if not fn.IsPure then 
            if ctx.IsPure then reportErrorForCallToImpureFunction call fn
            // prevent foldIteBack to trigger on this VarWrite, see bug 964
            addStmtsOpt [Comment(c, "non-pure function")] processed 
          else Some (processed)
        | Stmt (c, Call (c', fn, targs, args)) -> 
          Some (Stmt (c, Call (c', fn, targs, List.map self args)))
        | Call (c, fn, targs, args) as call ->
          let requireArgsToBePure = fn.Name.StartsWith("_vcc_")
          let checkArgs = List.map (fun (e : Expr) -> e.SelfCtxMap(requireArgsToBePure, pullOutCalls))
          if fn.RetType = Type.Void then
            if ctx.IsPure then helper.Error (call.Token, 9613, "void-call used in pure context", None)
            Some (Stmt (c, Call (c, fn, targs, checkArgs args)))
          else
            if ctx.IsPure then
              if not fn.IsPure then reportErrorForCallToImpureFunction call fn
              None
            else
              let rec isGenericType = function
                | TypeVar _ -> true
                | Ptr t
                | Volatile t
                | Array(t, _) -> isGenericType t
                | Map(t1, t2) -> isGenericType t1 || isGenericType t2
                | _ -> false
              
              let retType = if isGenericType fn.RetType then call.Type else fn.RetType
              let tmp = getTmp helper ("res_" + fn.Name) retType VarKind.Local
              let call' = Call (c, fn, targs, checkArgs args)
              let c' = { c with Type = Void }
              let tmpRef = Expr.Ref (c, tmp)
              addStmtsOpt [VarDecl (c', tmp); VarWrite (c', [tmp], call')] tmpRef
              
        | VarWrite (c, v, Macro (c', ("claim"|"upgrade_claim" as name), args)) ->
          Some (VarWrite (c, v, Macro (c', name, List.map self args)))
        
        | Macro (c, ("claim"|"upgrade_claim" as name), args) ->
          if ctx.IsPure then
            helper.Error (c.Token, 9652, "claim(...) used in pure context", None)
          let tmp = getTmp helper "res_claim" (SpecPtr Claim) VarKind.SpecLocal
          let call' = Macro (c, name, List.map self args)
          let c' = { c with Type = Void }
          let tmpRef = Expr.Ref (c, tmp)
          addStmtsOpt [VarDecl (c', tmp); VarWrite (c', [tmp], call')] tmpRef
    
        | Expr.Macro (_, "noop", _) -> Some (Expr.MkBlock [])
        | _ -> None

    // ============================================================================================================
      
    let handleOutParameters ctx self =
      let splitByOut vs = 
        let seenVars = new Dict<_,_>()
        let addAndComplainForDuplicateVar tok (v:Variable) =
          match seenVars.TryGetValue(v) with
            | true, relTok -> helper.Error(tok, 9676, "duplicate out parameter '" + v.Name + "'", relTok)
            | _ -> seenVars.Add(v, Some(tok))
          
        List.iter (fun v -> seenVars.Add(v, None)) vs
          
        let rec splitByOut' outAcc nonOutAcc decls assigns = function
          | [] -> List.rev outAcc, List.rev nonOutAcc, List.rev decls, List.rev assigns
          | Macro(_, "out", [Ref(ec, v)]) :: exprs -> 
            addAndComplainForDuplicateVar ec.Token v
            splitByOut' (v::outAcc) nonOutAcc decls assigns exprs
          | Macro(_, "out", [Deref(ec, mem)]) :: exprs ->
            let tmp = getTmp helper "outpar" ec.Type VarKind.Local
            let decl = VarDecl(voidBogusEC(), tmp)
            let assign = MemoryWrite(voidBogusEC(), mem, Ref(ec, tmp))
            splitByOut' (tmp::outAcc) nonOutAcc (decl::decls) (assign::assigns) exprs
          | e :: exprs -> splitByOut' outAcc (e::nonOutAcc) decls assigns exprs
        splitByOut' [] [] [] []

      let processCall vs = function
        | Call(c, fn, targs, args) -> 
          match splitByOut vs args with
            | [], _, _, _ -> None
            | outArgs, nonOutArgs, decls, assigns -> 
              Some(Expr.MkBlock(decls @ ( VarWrite(voidBogusEC(), vs @ outArgs, Call(c, fn, targs, nonOutArgs)) :: assigns)))
        | _ -> die()

      function
        | Stmt(_, (Call(_,_,_, _) as c)) -> processCall [] c
        | VarWrite(_, vs, (Call(_, _, _, _) as c)) -> processCall vs c
        | _ -> None
    
    // ============================================================================================================    

    let liftBlocksWithContracts decls = 
    
      let currentFunctionName = ref ""
      let blockFunctionDecls = ref []
    
      let reportErrorForJumpsOutOfBlock (block:Expr) =
        let internalLabels = new Dict<_,_>()
        let findLabels _ = function
          | Expr.Label(_, { Name = l }) -> internalLabels.[l] <- true; false
          | _ -> true
        let reportError _ = function
          | Expr.Goto(ec, { Name = l }) when not (internalLabels.ContainsKey l) ->
            helper.Error(ec.Token, 9705, "Block with explicit contract must not contain goto to external label '" + l + "'."); false
          | Expr.Return(ec, _) ->
            helper.Error(ec.Token, 9705, "Block with explicit contract must not contain return statement."); false
          | _ -> true
        block.SelfVisit(findLabels)
        block.SelfVisit(reportError)
        
      let findLocalsAndTurnIntoParameters fBefore fAfter (exprs: list<Expr>) =
        let inMap = new Dict<_,_>()
        let outMap = new Dict<_,_>()
        let localsThatGoIn = ref []
        let localsThatGoOut = ref []
        
        let vMap (d : Dict<_,_>) v =
          match d.TryGetValue(v) with
            | true, v' -> v'
            | false, _ -> v
          
        let addIn (v:Variable) =
          if fBefore v && not (inMap.ContainsKey(v)) then 
            let vp = Variable.CreateUnique v.Name v.Type Parameter
            inMap.Add(v, vp)
            localsThatGoIn := v :: !localsThatGoIn
            
        let addOut (v : Variable) =
          addIn v
          if fAfter v then
            if not (outMap.ContainsKey(v)) then 
              let vp = Variable.CreateUnique ("o#"+v.Name) v.Type OutParameter
              outMap.Add(v, vp)
              localsThatGoOut := v :: !localsThatGoOut

        let findLocals self = function
          | Expr.Ref(_, ({Kind = Parameter|SpecParameter|OutParameter|Local|SpecLocal} as v)) as vref -> addIn v; false
          | VarWrite(_, vs, expr) -> List.iter addOut vs; true
          | Macro(_, "out", [Ref(_, v)]) -> addOut v; false
          | _ -> true
                    
        let replLocals self = function
          | Expr.Macro(ec, "block_ensures", rqs) ->
            // inside of ensures, we need to replace references to in-parameters in the context of old
            // to their out-parameters counter part
            // if we find such an occurrence, we suppress warnings about old not referring to state
            // as we leave old in because we don't know if it contains other references to state
            let rec fixupLocalsInRequires seenOld varReplaced self = function
              | Expr.Old(ec, (Macro(_, "prestate", []) as ps), e) -> 
                let varReplaced = ref false
                let e' = e.SelfMap(fixupLocalsInRequires true varReplaced)
                let tok = if !varReplaced then new WarningSuppressingToken (e'.Token, 9106) :> Token else e'.Token
                Some(Old({ec with Token = tok}, ps, e'))
              | Expr.Ref(_, v) when not seenOld -> Some(v |> vMap outMap |> vMap inMap |> mkRef)
              | Expr.Ref(_, v) when     seenOld -> 
                let v' = vMap inMap v
                if v <> v' then
                  varReplaced := true
                  Some(mkRef v')
                else None
              | _ -> None
            let dummy = ref false
            Some(Expr.Macro(ec, "block_ensures", List.map (fun (e:Expr) -> e.SelfMap(fixupLocalsInRequires false dummy)) rqs))
          | Expr.Ref(ec, v) -> Some(Expr.Ref(ec, vMap inMap v))
          | Expr.VarWrite(ec, vs, e) -> Some(Expr.VarWrite(ec, List.map (vMap inMap) vs, self e))
          | _ -> None
          
        List.iter (fun (e : Expr) -> e.SelfVisit findLocals) exprs
        List.map (fun (e : Expr) -> e.SelfMap replLocals) exprs, !localsThatGoIn, vMap inMap, !localsThatGoOut, vMap outMap
          
      let findReferencesBeforeAndAfter (fn : Function) block =
        let before = new Dict<_,_>()
        let after = new Dict<_,_>()
        let seenBlock = ref false
        let fVar (d:Dict<_,_>) v = d.ContainsKey(v)

        let findThem _ = function
          | VarDecl(_, v) when not !seenBlock -> before.[v] <- true; false
          | Ref(_, v) when !seenBlock -> after.[v] <- true; false
          | e when e = block -> seenBlock := true; false
          | _ -> true
          
        let findThemInEnsures _ = function
          | Ref(_, v) -> after.[v] <- true; false
          | _ -> true
          
        List.iter (fun v -> before.Add(v, true)) fn.Parameters
        (Option.get fn.Body).SelfVisit(findThem)
        match block with
          | Expr.Macro(_, "block", [ _b; _r; Macro(_, "block_ensures", ensures); _w; _rd ]) ->
            List.iter (fun (e:Expr) -> e.SelfVisit(findThemInEnsures)) ensures
          | _ -> die()
        fVar before, fVar after
          
      let rec liftBlocks findRefs currentBlockId blockPrefix self = function
        | Expr.Macro(ec, "block", block :: blockContracts) as b ->
          let blockId = (!currentBlockId).ToString()
          incr currentBlockId
          reportErrorForJumpsOutOfBlock b
          let block' = block.SelfMap(liftBlocks findRefs (ref 0) (blockPrefix + blockId + "#"))
          let fBefore, fAfter = findRefs b
          match findLocalsAndTurnIntoParameters fBefore fAfter (block' :: blockContracts) with
            | [ body; 
                Expr.Macro(_, "block_requires", rqs); 
                Expr.Macro(_, "block_ensures", ens); 
                Expr.Macro(_, "block_writes", wrs);
                Expr.Macro(_, "block_reads", rds) ], localsThatGoIn, inMap, localsThatGoOut, outMap ->
              let mkSetOutPar (v : Variable) =
                Expr.VarWrite(voidBogusEC(), [outMap v], mkRef (inMap v))
              let stripInitialPure = List.map (function | Pure(_, e) -> e | e -> e)
                
              let fn = { Token = body.Token
                         IsSpec = false
                         OrigRetType = Type.Void
                         RetType = Type.Void
                         Parameters = List.map inMap localsThatGoIn @ List.map outMap localsThatGoOut
                         TypeParameters = []
                         Name = !currentFunctionName + "#block#" + blockPrefix + blockId
                         Requires = stripInitialPure rqs
                         Ensures = stripInitialPure ens
                         Writes = stripInitialPure wrs
                         Reads = stripInitialPure rds
                         CustomAttr = []
                         Body = Some (Expr.MkBlock(body :: List.map mkSetOutPar localsThatGoOut))
                         IsProcessed = true
                         UniqueId = CAST.unique() } : Function
              blockFunctionDecls := Top.FunctionDecl(fn) :: !blockFunctionDecls
              let call = Expr.Call(ec, fn, [], List.map mkRef localsThatGoIn)
              let result = if localsThatGoOut.Length = 0 then call else Expr.VarWrite(ec, localsThatGoOut, call)
              Some(result)
            | _ -> die()
        | _ -> None  
    
      for d in decls do
        match d with
          | Top.FunctionDecl({ Name = name; Body = Some body} as fn) ->
            currentFunctionName := name
            fn.Body <- Some(body.SelfMap (liftBlocks (findReferencesBeforeAndAfter fn) (ref 0) ""))
          | _ -> ()
      decls @ List.sortBy (fun top -> match top with | Top.FunctionDecl(fn) -> fn.Name | _ -> die()) !blockFunctionDecls

    
    // ============================================================================================================    

    let handleMapEquality self = function
      | Expr.Prim(ec, Op("==", _), [e1; e2]) when e1.Type._IsMap ->
        Some(Expr.Macro(ec, "map_eq", [self e1; self e2]))
      | Expr.Prim(ec, Op("!=", _), [e1; e2]) when e1.Type._IsMap ->
        Some(Expr.Prim(ec, Op("!", Unchecked), [Expr.Macro(ec, "map_eq", [self e1; self e2])]))
      | _ -> None

    // ============================================================================================================    

    let handleMapInit self = function
      | VarDecl(ec, v) as decl when (v.Kind = VarKind.SpecLocal  || v.Kind = VarKind.Local) && v.Type._IsMap ->
        Some(Expr.MkBlock([decl; Expr.VarWrite(ec, [v], Macro({ec with Type = v.Type}, "map_zero", []))]))
      | _ -> None

    // ============================================================================================================    

    helper.AddTransformer ("core-begin", Helper.DoNothing)
    
    // it's important if a transformer is before or after those two
    //   -- it affects how assignments and special calls are seen
    // this is why we're providing hooks here
    helper.AddTransformer ("pre-assignments", Helper.DoNothing)    
    helper.AddTransformer ("core-assignments", Helper.ExprCtx assignmentDesugaring)
    helper.AddTransformer ("core-blocks-with-contracts", Helper.Decl liftBlocksWithContracts) // must run after heapification and assignment processing
    helper.AddTransformer ("core-keeps", Helper.Decl handleKeeps)    
    helper.AddTransformer ("core-special-calls", Helper.Expr (handlePureCalls helper))    
    helper.AddTransformer ("post-assignments", Helper.DoNothing)    
    helper.AddTransformer ("core-records", Helper.Decl handleRecords)
    helper.AddTransformer ("core-value-structs", Helper.Decl removeStructsPassedByValue)
    helper.AddTransformer ("core-parm-writes", Helper.Decl removeWritesToParameters)
    helper.AddTransformer ("core-pull-out-calls", Helper.ExprCtx pullOutCalls)
    helper.AddTransformer ("core-out-parameters", Helper.ExprCtx handleOutParameters)
    helper.AddTransformer ("core-map-eq", Helper.Expr handleMapEquality)
    helper.AddTransformer ("core-map-init", Helper.Expr handleMapInit)
    helper.AddTransformer ("core-linearize", Helper.Decl (linearizeDecls helper))
    
    helper.AddTransformer ("core-end", Helper.DoNothing)
    
      
