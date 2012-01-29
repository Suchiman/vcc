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
 
 module ExtraWarnings =

  // ============================================================================================================          

  let init (helper:Helper.Env) =

    let warnForUncheckedGhostLoops decls = 

      let rec warnForUncheckedGhostLoops fn withinSpec self = function
        | Loop(ec, _, _, _, _) 
        | Macro(ec, ("while"|"doUntil"|"for"), _) when withinSpec -> helper.GraveWarning (ec.Token, 9323, "ghost loop not checked for termination"); true
        | CallMacro(_, "spec", _, args) -> List.iter (fun (e:Expr) -> e.SelfVisit(warnForUncheckedGhostLoops fn true)) args; false
        | _ -> true    

      for d in decls do
        match d with
          | Top.FunctionDecl({Body = Some body} as fn) when not (checkTermination helper fn) ->
            body.SelfVisit(warnForUncheckedGhostLoops fn fn.IsSpec)
            ()
          | _ -> ()

      decls

    // ============================================================================================================          

    let warnForOldWithoutVolatiles decls =
      
      let rec isVolatileType = function
        | Type.Ref({IsVolatile = true}) | Volatile(_) -> true
        | Ptr(t) | Array(t, _) -> isVolatileType t
        | _ -> false
      
      let mentionsVolatile (expr:Expr) =
        let foundVolatile = ref false     
        let continueIfNotFound() = not !foundVolatile
        let found() = foundVolatile := true; false
        let findVolatile _ = function
          | Expr.Dot(_,_,f) when f.IsVolatile || isVolatileType f.Type -> found()
          | CallMacro(_, "_vcc_current_state", _, _) -> found()
          | CallMacro(_, "_vcc_closed", _, _) -> found()
          | CallMacro(_, "_vcc_owns", _, [Cast(_,_, expr)|expr]) ->
            match expr.Type with 
              | Ptr(Type.Ref(td)) ->
                if hasCustomAttr AttrVolatileOwns td.CustomAttr then found() else continueIfNotFound()
              | _ -> continueIfNotFound()
          | _ -> continueIfNotFound()
        expr.SelfVisit findVolatile 
        !foundVolatile

      let doWarn self = function
        | Expr.Old(_, Macro (_, "prestate", []), e) as expr when not (mentionsVolatile e) ->
          helper.Warning(expr.Token, 9115, "'old' in invariant does not refer to volatile state")
          true
        | _ -> true
      
      forEachInvariant doWarn decls
          
      decls

  // ============================================================================================================          

    helper.AddTransformer ("warn-begin", Helper.DoNothing)
    helper.AddTransformer ("warn-two-state-inv-without-volatile", Helper.Decl warnForOldWithoutVolatiles)
    helper.AddTransformer ("warn-unchecked-ghost-loops", Helper.Decl warnForUncheckedGhostLoops)
    helper.AddTransformer ("warn-end", Helper.DoNothing)

