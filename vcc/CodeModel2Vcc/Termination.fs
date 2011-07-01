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

let turnIntoPureExpression (helper:Helper.Env) topType (expr:Expr) =
  let rec aux stmtCtx bindings (exprs:list<Expr>) =
    let expr, cont = exprs.Head, exprs.Tail
    //System.Console.WriteLine ("doing (cont={0}) e: {1}/{2}", cont.Length, expr, expr.GetType())

    let recExpr e = aux false bindings [e]
    let self = aux stmtCtx bindings

    let notsupp kind = 
      helper.Error (expr.Token, 0, kind + " are not supported when turning statements into expressions")
      Macro (bogusEC, "null", [])

    let valueShouldFollow f =
      if cont.IsEmpty then
        helper.Error (expr.Token, 0, "expecting value here")
        Macro (bogusEC, "null", [])        
      else
        f cont

    let returnValue () =
      if cont.IsEmpty then
        //System.Console.WriteLine "recurse"
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
        Macro ({bogusEC with Type = topType}, "default", [])

      // ignored statements
      | Assert _
      | Assume _ 
      | Comment _
      | Label _ 
      | VarDecl _ ->
        valueShouldFollow self

      // errors
      | MemoryWrite _ -> notsupp "state updates"
      | Atomic _ -> notsupp "atomic blocks"
      | Loop _ -> notsupp "loops"
      | Goto _ -> notsupp "gotos"

      | VarWrite (ec, [v], e) ->
        let bindings = Map.add v.UniqueId (recExpr e) bindings
        valueShouldFollow (aux stmtCtx bindings)

      | If (ec, _, cond, thn, els) ->        
        Macro (ec, "ite", [recExpr cond; self (thn :: cont); self (els :: cont)])

      | Block (ec, exprs, None) ->
        self (exprs @ cont)

      | Return (ec, Some e) ->
        if not stmtCtx then
          helper.Die()
        recExpr e

      | Macro _ ->
        // TODO?
        returnValue()

      | VarWrite _
      | Block _
      | Return _ -> helper.Die()

  aux true Map.empty [expr]

let insertTerminationChecks (helper:Helper.Env) decls =
  let check decrRefs self e =
    match e with
    | Call (ec, fn, tps, args) as e ->
      if fn.IsDatatypeOption then
        None
      elif fn.Name.StartsWith "lambda#" then
        None 
      elif fn.CustomAttr |> hasCustomAttr AttrDefinition then        
        let subst = fn.CallSubst args
        let assigns, callVariants = 
          fn.Variants 
            |> List.map (fun e -> e.Subst subst)
            |> cacheMultiple helper lateCache "callDecr" VarKind.SpecLocal
        let rec computeCheck = function
          | ((s:Expr) :: ss, (c:Expr) :: cc) ->
            match s.Type, c.Type with
              | (MathInteger | Integer _), (MathInteger | Integer _) ->
                Expr.Macro (boolBogusEC(), "prelude_int_lt_or", [c; s; computeCheck (ss, cc)])
              | _ ->
                helper.GraveWarning (e.Token, 9314, "only integer arguments are currently accepted in _(decreases ...) clauses")
                Expr.False
          // missing elements are treated as Top, e.g. consider:
          // f(a) = ... f(a-1) ... g(a-1,b) ...
          // g(a,b) = ... g(a,b-1) ... f(a-1) ...
          | (_, []) -> Expr.False
          | ([], _) -> Expr.True 
        let check = computeCheck (decrRefs, callVariants)
        let check = check.WithCommon (afmte 8029 "the call '{0}' might not terminate" [e])
        let check = Expr.MkAssert check
        Some (Expr.MkBlock (assigns @ [check; Call (ec, fn, tps, List.map self args)]))
      else
        helper.GraveWarning (e.Token, 9315, "function '" + fn.Name + "' should by defined with _(def) for termination checking")
        None
        
    | Loop _
    | Goto _ ->
      // as funny as it may sound...
      helper.GraveWarning (e.Token, 9316, "loops and gotos are currently not supported in termination checking")
      Some e

    | Assert _
    | Assume _ -> 
      // skip checking inside
      Some e 

    | Block (_, _, Some _) ->
      helper.Die() // these should be gone, right?
      None

    | Macro (_, "dt_testhd", _) ->
      None

    | Macro (_, name, _) when name.StartsWith "DP#" ->
      None

    | Macro (_, ("rec_update"|"rec_fetch"|"map_zero"|"rec_zero"|"havoc_locals"|"_vcc_rec_eq"|"map_get"), _) ->
      None

    | Macro (ec, s, args) as e ->
      helper.GraveWarning (e.Token, 9317, "macro '" + s + "' is not supported in termination checking")
      Some e

    | _ -> None

  let aux = function
    | Top.FunctionDecl fn as decl when fn.CustomAttr |> hasCustomAttr AttrDefinition ->
      if fn.Body.IsNone then
        helper.GraveWarning (fn.Token, 9318, "definition functions need to have body")
        [decl]
      else
        if fn.Variants = [] then
          fn.Variants <- fn.Parameters |> List.map (fun v -> Ref ({ bogusEC with Type = v.Type }, v))
        let assigns, refs = cacheMultiple helper lateCacheRef "thisDecr" VarKind.SpecLocal fn.Variants 
        let body = Expr.MkBlock (assigns @ [fn.Body.Value])
        let body = body.SelfMap (check refs)
        fn.Body <- Some body
        if fn.RetType <> Type.Void then
          let expr = turnIntoPureExpression helper fn.RetType body
          let vars, repl = Variable.UniqueCopies (fun v -> { v with Kind = QuantBound }) fn.Parameters
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
        else
          [decl]
    | decl -> [decl]
  List.collect aux decls

let init (helper:Helper.Env) =
  helper.AddTransformer ("termination-add-checks", Helper.Decl (insertTerminationChecks helper))
