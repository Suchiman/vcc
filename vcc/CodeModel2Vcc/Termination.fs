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

let insertTerminationChecks (helper:Helper.Env) decls =
  let check decrRefs self e =
    match e with
    | Call (ec, fn, tps, args) as e ->
      if fn.CustomAttr |> hasCustomAttr AttrDefinition then        
        let subst = fn.CallSubst args
        let assigns, callVariants = 
          fn.Variants 
            |> List.map (fun e -> e.Subst subst)
            |> cacheMultiple helper lateCache "thisDecr" VarKind.SpecLocal
        let rec computeCheck = function
          | ((s:Expr) :: ss, (c:Expr) :: cc) ->
            match s.Type, c.Type with
              | (MathInteger | Integer _), (MathInteger | Integer _) ->
                Expr.Macro (boolBogusEC(), "_vcc_int_lt_or", [c; s; computeCheck (ss, cc)])
              | _ ->
                helper.GraveWarning (e.Token, 0, "only integer arguments are currently accepted in _(decreases ...) clauses")
                Expr.False
          | ([], _) -> Expr.False
          | (_, []) -> Expr.True
        let check = computeCheck (decrRefs, callVariants)
        let check = check.WithCommon (afmte 0 "the call '{0}' might not terminate" [e])
        let check = Expr.MkAssert check
        Some (Expr.MkBlock (assigns @ [check; Call (ec, fn, tps, List.map self args)]))
      else
        helper.GraveWarning (e.Token, 0, "function '" + fn.Name + "' should by defined with _(def) for termination checking")
        None
        
    | Loop _
    | Goto _ ->
      // as funny as it may sound...
      helper.GraveWarning (e.Token, 0, "loops and gotos are currently not supported in termination checking")
      Some e

    | Assert _
    | Assume _ -> 
      // skip checking inside
      Some e 

    | Block (_, _, Some _) ->
      helper.Die() // these should be gone, right?
      None

    | Macro (ec, s, args) as e ->
      helper.GraveWarning (e.Token, 0, "macro '" + s + "' is not supported in termination checking")
      Some e

    | _ -> None

  let aux = function
    | Top.FunctionDecl fn when fn.CustomAttr |> hasCustomAttr AttrDefinition ->
      let assigns, refs = cacheMultiple helper lateCacheRef "thisDecr" VarKind.SpecLocal fn.Variants 
      let body = Expr.MkBlock (assigns @ [fn.Body.Value])
      fn.Body <- Some (body.SelfMap (check refs))
      ()
    | _ -> ()
  List.iter aux decls
  decls

let init (helper:Helper.Env) =
  helper.AddTransformer ("termination-add-checks", Helper.Decl (insertTerminationChecks helper))
