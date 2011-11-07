//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

module Microsoft.Research.Vcc.Termination

open Microsoft.Research.Vcc
open Microsoft.Research.Vcc.Util
open Microsoft.Research.Vcc.TransUtil
open Microsoft.Research.Vcc.CAST

let checkTermination (helper:Helper.Env) (fn:Function) =
  if fn.CustomAttr |> hasCustomAttr AttrDefinition || fn.CustomAttr |> hasCustomAttr AttrAbstract then
    helper.Options.TerminationForPure
  // if explicit measure is given, and termination is not disabled, then check it 
  elif helper.Options.TerminationForPure && fn.Variants <> [] then
    true
  elif fn.IsSpec then
    helper.Options.TerminationForGhost
  else
    helper.Options.TerminationForAll

let checkCallCycles (helper:Helper.Env) decls = 
  let calls = gdict()
  let aux = function
    | Top.FunctionDecl { UniqueId = id; Body = Some b } ->
      let called = gdict()
      let calledList = glist[]
      let aux _ = function
        | Call (_, fn, _, _) when not (called.ContainsKey fn.UniqueId) ->
          called.[fn.UniqueId] <- true
          calledList.Add fn
          true
        | _ -> true
      b.SelfVisit aux
      calls.[id] <- calledList |> Seq.toList
    | _ -> ()
  List.iter aux decls

let setDecreasesLevel (helper:Helper.Env) decls =
  let aux = function
    | Top.FunctionDecl fn ->
      let checkDecr = function
        | CallMacro (ec, "_vcc_decreases_level", _, [arg]) ->
          match arg with
            | Expr.IntLiteral (_, n) ->
              match System.Int32.TryParse (n.ToString()) with
                | true, k ->
                  fn.DecreasesLevel <- k
                | _ ->
                  helper.Error (ec.Token, 9733, "_(level ...) needs to fit integer range")
            | _ -> 
              helper.Error (ec.Token, 9734, "_(level ...) needs a compile time constant")
          Expr.True
        | e -> e
      fn.Requires <- List.map checkDecr fn.Requires
      let lev =
        if fn.DecreasesLevel <> 0 || checkTermination helper fn then
          fn.DecreasesLevel
        else
          System.Int32.MaxValue
      let assump = Expr.MkAssume (Macro (boolBogusEC(), "decreases_level_is", [mkInt lev]))
      fn.Body <- Option.map (addStmts [assump]) fn.Body
    | _ -> ()
  List.iter aux decls
  decls

type PureTrCtx =
  {
    InStmt : bool
    SeenAssertFalse : bool
  }

let turnIntoPureExpression (helper:Helper.Env) topType (expr:Expr) =
  let rec aux (ctx:PureTrCtx) bindings (exprs:list<Expr>) =
    let expr, cont = exprs.Head, exprs.Tail
    //System.Console.WriteLine ("doing (cont={0}) e: {1}/{2}", cont.Length, expr, expr.GetType())

    let recExpr e = aux { ctx with InStmt = false } bindings [e]
    let self = aux ctx bindings

    let notsupp kind = 
      helper.Error (expr.Token, 9735, kind + " are not supported when turning statements into expressions")
      Macro ({ bogusEC with Type = topType }, "default", [])

    let valueShouldFollow (ctx:PureTrCtx) f =
      if cont.IsEmpty then
        if not ctx.SeenAssertFalse then
          helper.Error (expr.Token, 9736, "expecting value here")
        Macro ({ bogusEC with Type = topType }, "default", [])
      else
        f cont

    let returnValue () =
      if cont.IsEmpty then
        expr.ApplyToChildren recExpr
      else
        // warn about ignored expression?
        self cont

    match expr with
      // pure expressions
      | Prim _
      | IntLiteral _
      | BoolLiteral _
      | Deref _
      | Dot _
      | Index _
      | Cast _
      | Quant _
      | Result _
      | This _
      | Old _
      | SizeOf _ 
      | UserData _ ->
        returnValue()

      | Ref (_, v) ->
        if Map.containsKey v.UniqueId bindings && cont.IsEmpty then
          Map.find v.UniqueId bindings
        else
          returnValue()

      | Call (ec, fn, _, _) ->
        // TODO check if fn is OK
        returnValue()

      | Pure (_, e)
      | Stmt (_, e) ->
        self (e :: cont)

      | Assert (_, Expr.BoolLiteral (_, false), _) ->
        let ctx = { ctx with SeenAssertFalse = true }
        valueShouldFollow ctx (aux ctx bindings)

      // ignored statements
      | Label _
      | Assert _
      | Assume _ 
      | Comment _
      | VarDecl _ ->
        valueShouldFollow ctx self

      // errors
      | MemoryWrite _ -> notsupp "state updates"
      | Atomic _ -> notsupp "atomic blocks"
      | Loop _ -> notsupp "loops"
      | Goto _ -> notsupp "gotos"

      | VarWrite (ec, [v], e) ->
        let bindings = Map.add v.UniqueId (recExpr e) bindings
        valueShouldFollow ctx (aux ctx bindings)

      | If (ec, _, cond, thn, els) ->
        let rec noEffect = function
          | If (_, _, Macro (_, "check_termination", _), _, _) -> true
          | If (_, _, _, thn, els) -> noEffect thn && noEffect els
          | Assert _
          | Assume _ 
          | Comment _
          | Label _ 
          | VarDecl _ -> true
          | Block (_, lst, _) -> List.forall noEffect lst
          | _ -> false
        if noEffect expr then
          self cont
        else
          Macro (ec, "ite", [recExpr cond; self (thn :: cont); self (els :: cont)])

      | Block (ec, exprs, None) ->
        self (exprs @ cont)

      | Return (ec, Some e) ->
        if not ctx.InStmt then
          helper.Die()
        recExpr e

      | Macro _ ->
        // TODO?
        returnValue()

      | VarWrite _
      | Block _
      | Return _ -> helper.Die()

  let ctx =
    {
      InStmt = true
      SeenAssertFalse = false
    } 
  aux ctx Map.empty [expr]

let insertTerminationChecks (helper:Helper.Env) decls =
  let check (currFn:Function) decrRefs (labels:Dict<_,_>) self e =
    let rec computeCheck tok = function
      | ((s:Expr) :: ss, (c:Expr) :: cc) ->
        match s.Type, c.Type with
          | (MathInteger _ | Integer _), (MathInteger _ | Integer _) ->
            Expr.Macro (boolBogusEC(), "prelude_int_lt_or", [c; s; computeCheck tok (ss, cc)])
          | _ ->
            helper.GraveWarning (tok, 9314, "only integer arguments are currently accepted in _(decreases ...) clauses; consider using \\size(...)")
            Expr.False
      // missing elements are treated as Top, e.g. consider:
      // f(a) = ... f(a-1) ... g(a-1,b) ...
      // g(a,b) = ... g(a,b-1) ... f(a-1) ...
      | (_, []) -> Expr.False
      | ([], _) -> Expr.True 

    let rec justChecks = function
      | Pure (_, e) -> justChecks e

      | Call (ec, fn, tps, args) as e ->
        if fn.IsDatatypeOption then
          []
        elif fn.Name.StartsWith "lambda#" then
          []
        elif fn.DecreasesLevel < currFn.DecreasesLevel then
          []
        elif fn.DecreasesLevel > currFn.DecreasesLevel then
          helper.GraveWarning (e.Token, 9319, 
                               System.String.Format ("calling function '{0}' (level {1}) from lower-level function ('{2}' at level {3})", 
                                                     fn.Name, fn.DecreasesLevel, currFn.Name, currFn.DecreasesLevel))
          []
        elif checkTermination helper fn then
          let subst = fn.CallSubst args
          let assigns, callVariants = 
            fn.Variants 
              |> List.map (fun e -> e.Subst subst)
              |> cacheMultiple helper lateCache "callDecr" VarKind.SpecLocal
          let check = computeCheck e.Token (decrRefs, callVariants)
          if check = Expr.False then
            helper.GraveWarning (e.Token, 9321, "no measure to decrease when calling '" + e.ToString() + "'; consider using _(level ...)")
            assigns
          else
            let check = check.WithCommon (afmte 8029 "the call '{0}' might not terminate" [e])
            let check = Expr.MkAssert check
            assigns @ [check]
        else
          helper.GraveWarning (e.Token, 9315, "termination checking not enabled for function '" + fn.Name + "'; consider supplying _(decreases ...) clause")
          []
      | _ -> helper.Die()

    match e with
    | Call (_, { Name = "_vcc_stack_alloc" }, _, _) ->
      None

    | CallMacro (_, name, _, _) when helper.PureCallSignature name |> Option.isSome ->
      None

    | Call (ec, fn, tps, args) as e ->
      match justChecks e with
        | [] -> None
        | lst ->
          Some (Expr.MkBlock (lst @ [Call (ec, fn, tps, List.map self args)]))

    | Loop (ec, inv, wr, decr, body) ->
      if decr = [] then
        helper.GraveWarning (ec.Token, 9323, "failed to infer _(decreases ...) clause for the loop; please supply one")
        Some (Loop (ec, inv, wr, decr, self body))
      else
        let body = self body
        let assigns0, refs0 = cacheMultiple helper lateCacheRef "loopDecrBeg" VarKind.SpecLocal decr
        let assigns1, refs1 = cacheMultiple helper lateCacheRef "loopDecrEnd" VarKind.SpecLocal decr
        let check = computeCheck ec.Token (refs0, refs1)
        let check = check.WithCommon (afmte 8033 "the loop fails to decrease termination measure" [e])
        let body = Expr.MkBlock (assigns0 @ [body] @ assigns1 @ [Expr.MkAssert check])
        Some (Loop (ec, inv, wr, decr, body))

    | Label (_, { Name = n }) ->
      labels.[n] <- true
      None

    | Goto (ec, { Name = n }) ->
      if labels.ContainsKey n then
        helper.GraveWarning (e.Token, 9316, "backward gotos are currently not supported in termination checking")
      Some e

    | Assert _
    | Assume _ -> 
      // skip checking inside
      Some e 

    | Macro (_, "skip_termination_check", [e]) ->
      Some e

    | Block (_, _, Some _) ->
      helper.Die() // these should be gone, right?
      None

    | Macro (_, "dt_testhd", _) ->
      None

    | Macro (_, name, _) when name.StartsWith "DP#" ->
      None

    | Macro (_, ("rec_update"|"rec_fetch"|"map_zero"|"rec_zero"|"havoc_locals"|"_vcc_rec_eq"|"map_get"
                  |"vs_fetch"|"ite"|"size"|"check_termination"|"map_updated"|"stackframe"|"map_eq"
                  |"ignore_me"|"null"|"by_claim"|"vs_updated"|"state"|"ghost_atomic"
                  |"bv_update"|"bv_extract_unsigned"|"float_literal"|"_vcc_by_claim"
                  |"rec_update_bv"|"prestate"|"_vcc_ptr_eq_pure"|"bogus"
                  |"_vcc_ptr_eq_null"|"_vcc_ptr_neq_null"), _) ->
      None

    | Macro (_, ("_vcc_static_wrap"|"_vcc_static_unwrap"|"havoc"|"_vcc_unblobify"|"_vcc_blobify"
                  |"_vcc_wrap"|"_vcc_unwrap"|"_vcc_static_wrap_non_owns"
                  |"_vcc_havoc_others"|"_vcc_unwrap_check"|"inlined_atomic"
                  |"unclaim"|"claim"|"begin_update"|"upgrade_claim"), _) ->
      None

    | Macro (_, s, _) when s.StartsWith "prelude_" || s.StartsWith "unchecked_" -> 
      None

    | Macro (ec, s, args) as e ->
      helper.GraveWarning (e.Token, 9317, "macro '" + s + "' is not supported in termination checking")
      Some e

    | _ -> None

  let computeAllDefReads decls =
    let didSomething = ref true
    let computeDefReads = function
      | Top.FunctionDecl fn as decl when fn.CustomAttr |> hasCustomAttr AttrDefinition ->
        match fn.Reads, fn.Body with
          | [], Some b ->
            fn.Reads <- computeReads b
            if fn.Reads <> [] then
              didSomething := true
          | _ -> ()
      | _ -> ()
    while !didSomething do
      didSomething := false
      List.iter computeDefReads decls
   
  let setDefaultVariants = function
    | Top.FunctionDecl fn as decl when checkTermination helper fn ->
        if fn.Variants = [] then
          let aux acc (v:Variable) =
            let rf = Ref ({ bogusEC with Type = v.Type }, v)
            let sz = Macro ({ bogusEC with Type = Type.MathInteger Signed }, "size", [rf])
            match v.Type with
              | Type.MathInteger _
              | Type.Integer _ ->
                rf :: acc
              | Type.Ref td when td.IsDataType || td.IsRecord ->
                sz :: acc
              | _ -> acc
          fn.Variants <- fn.Parameters |> List.fold aux [] |> List.rev
    | _ -> ()
   
  let genDefAxiom = function
    | Top.FunctionDecl fn as decl when fn.CustomAttr |> hasCustomAttr AttrDefinition ->
      match fn.Body with
        | None ->
          helper.GraveWarning (fn.Token, 9318, "definition functions need to have body")
          [decl]
        | _ when fn.RetType = Void -> [decl]
        | Some body ->
          let expr = turnIntoPureExpression helper fn.RetType body
          if fn.Reads = [] then
            fn.Reads <- computeReads expr
          let suff = "#" + helper.UniqueId().ToString() 
          let vars, repl = Variable.UniqueCopies (fun v -> { v with Name = v.Name + suff ; Kind = QuantBound }) fn.Parameters
          let ec t = { body.Common with Type = t }
          let app = Call (ec fn.RetType, fn, [], vars |> List.map (fun v -> Expr.Ref (ec v.Type, v)))
          let eq = Prim (ec Type.Bool, Op ("==", Processed), [app; repl expr])
          let preconds = fn.Requires |> multiAnd |> repl
          let impl = Prim (ec Type.Bool, Op ("==>", Processed), [preconds; eq])
          let qd = 
            {
              Body = eq
              Variables = vars
              Kind = Forall
              Condition = None
              Triggers = [[app]]
            }
          let axiom = Top.Axiom (if vars = [] then eq else Quant (ec Type.Bool, qd))
          [decl; axiom]
    | decl -> [decl]
   
  let genChecks = function
    | Top.FunctionDecl ({ Body = Some body } as fn) when checkTermination helper fn ->
      let assigns, refs = cacheMultiple helper lateCacheRef "thisDecr" VarKind.SpecLocal fn.Variants 
      let body = Expr.MkBlock (assigns @ [body])
      let body = body.SelfMap (check fn refs (gdict()))
      fn.Body <- Some body
    | decl -> ()

  computeAllDefReads decls
  List.iter setDefaultVariants decls
  let decls = List.collect genDefAxiom decls
  List.iter genChecks decls
  decls

let terminationCheckingPlaceholder (helper:Helper.Env) decls =
  let rec skipChecks = function
    | Expr.Call _ as e ->
      Expr.Macro (e.Common, "skip_termination_check", [Expr.Pure (e.Common, e.ApplyToChildren skipChecks)])
    | e ->
      e.ApplyToChildren skipChecks
       
  let rec checks = function
    | Expr.Quant (ec, qd) as res ->
      let suff = "#" + helper.UniqueId().ToString() 
      let vars, repl = Variable.UniqueCopies (fun v -> { v with Name = v.Name + suff ; Kind = SpecLocal }) qd.Variables
      let decls = vars |> List.map (fun v -> Expr.VarDecl (bogusEC, v, []))
      match qd.Condition with
        | Some e ->
          let e = repl e
          let pref0 = checks e
          let pref1 = checks (repl qd.Body)
          decls @ pref0 @ [Expr.If (bogusEC, None, e, Expr.MkBlock pref1, Expr.MkBlock [])]
        | None ->
          let pref1 = checks (repl qd.Body)
          decls @ pref1 
    | Expr.Macro (ec, "ite", [cond; a; b]) ->
      checks cond @ [Expr.If (bogusEC, None, skipChecks cond, Expr.MkBlock (checks a), Expr.MkBlock (checks b))]
    | Expr.Call (ec, fn, targs, args) as e ->
      (args |> List.map checks |> List.concat) @ [e]

    // just the common stuff, so we know when we run into it
    | Expr.Assert _
    | Expr.Assume _
    | Expr.VarWrite _
    | Expr.Block _ 
    | Expr.Return _
    | Expr.MemoryWrite _
    | Expr.Goto _
    | Expr.Loop _ 
    | Expr.Stmt _ as e ->
      helper.Oops (e.Token, "expression not supported when checking termination of a binder: " + e.ToString())
      []

    | e ->
      let acc = glist[]
      let f e =
        acc.AddRange (checks e)
        e
      e.ApplyToChildren f |> ignore
      acc |> Seq.toList
      
  let termWrapper checks =
    Expr.If (voidBogusEC(), None, Expr.Macro (boolBogusEC(), "check_termination", [mkInt (helper.UniqueId())]), Expr.MkBlock(checks @ [Expr.MkAssume (Expr.False)]), Expr.MkBlock [])
     
  let rec addChecks = function
    | Quant _ as q ->
      Expr.MkBlock [termWrapper (checks q); skipChecks q]
    | e ->
      e.ApplyToChildren addChecks
       
  let aux = function
    | Top.FunctionDecl fn as decl when checkTermination helper fn ->
      fn.Body <- fn.Body |> Option.map addChecks
    | _ -> ()

  List.iter aux decls
  decls

let init (helper:Helper.Env) =
  if helper.Options.Vcc3 then
    helper.AddTransformer ("termination-set-level", Helper.Decl (setDecreasesLevel helper))
    helper.AddTransformer ("termination-add-checks", Helper.Decl (insertTerminationChecks helper))
    helper.AddTransformerBefore ("termination-insert-placeholders", Helper.Decl (terminationCheckingPlaceholder helper), "desugar-lambdas")
