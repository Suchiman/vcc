//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc

open Microsoft.FSharp.Math
open Microsoft.Research.Vcc
open Microsoft.Research.Vcc.TranslatorUtils
open Microsoft.Research.Vcc.Util
open System
  
module C = Microsoft.Research.Vcc.CAST
module B = Microsoft.Research.Vcc.BoogieAST

type TriggerInference(helper:Helper.Env, quantTok:Token, invMapping:Dict<B.Expr,list<C.Expr>>, quantVars:list<B.Var>) =
  let dbg = helper.Options.DumpTriggers >= 4
  let maxQuality = 3
  let quantVars = List.map fst quantVars // we don't care about the type
  let quantVarSet = gdict()
  do for v in quantVars do quantVarSet.[v] <- true

  // -1 : never use it
  //  0 : uses arithmetic (see below)
  //  1 : only as last resort
  //  2 : normal
  //  3 : good pattern
  let topTriggerQuality = function
    // list of bad patterns: 
    | B.Expr.FunctionCall ("$ref", [B.Expr.FunctionCall ("$ptr", [_; B.Expr.Ref _])]) -> -1
    | B.Expr.FunctionCall ("$ptr", [_; B.Expr.Ref _]) -> 1 

    // good patterns:
    | B.Expr.FunctionCall ("$set_in", _) -> 3

    | B.Expr.FunctionCall (name, _) when name.StartsWith "$select.$map" ->
      if name.EndsWith ".^^bool" then 3
      else 2

    // all the rest:
    | expr ->
      let isUserFun = function
        | C.Expr.Call _ -> true
        | _ -> false
      let orig = lookupWithDefault invMapping [] expr
      if List.exists isUserFun orig then 3
      else 2

  let isForbiddenInTrigger = function
    | B.Expr.Exists _
    | B.Expr.Forall _
    | B.Expr.Lambda _
    | B.Expr.Ite _
    | B.Expr.Primitive (("&&"|"||"|"=="|"!="|"==>"|"<==>"), _) -> true

    // Boogie doesn't allow comparisons
    | B.Expr.Primitive (("<"|">"|"<="|">="), _) -> true
    // We don't want this one.
    | B.Expr.FunctionCall (name, _) when name.StartsWith "$in_range" -> true

    | B.Expr.Primitive _ 
    | B.Expr.ArrayIndex _
    | B.Expr.ArrayUpdate _
    | B.Expr.BoolLiteral _
    | B.Expr.BvConcat _
    | B.Expr.BvExtract _ 
    | B.Expr.FunctionCall _ 
    | B.Expr.Ref _ 
    | B.Expr.Old _
    | B.Expr.BvLiteral _
    | B.Expr.SecLabelLiteral _
    | B.Expr.IntLiteral _ -> false

  let isArithmetic = function
    | B.Expr.Primitive (("<"|">"|"<="|">="|"+"|"-"|"*"), _) -> true
    | _ -> false

  let getTriggerScore = function
    | B.Expr.Ref _ -> None
    | expr when isForbiddenInTrigger expr -> None
    | expr ->
      let allowed = ref true
      let hasArithmetic = ref false
      let res = gdict()
      let aux expr =
        if not !allowed then Some expr
        elif isForbiddenInTrigger expr then
          allowed := false
          Some expr
        else
          if isArithmetic expr then
            hasArithmetic := true
          match expr with
            | B.Expr.Ref s when quantVarSet.ContainsKey s ->
              res.[s] <- true
            | _ -> ()
          None
      expr.Map aux |> ignore
      if !allowed then        
        if !hasArithmetic then Some (res, 0)
        else Some (res, topTriggerQuality expr)
      else None
        
  let rec matchExpr (sub : Map<_,_>) = function
    | B.Expr.FunctionCall ("$mem", [s1; p1]), B.Expr.FunctionCall ("$mem", [s2; p2]) when matchExpr sub (s1, s2) = None -> 
      matchExpr sub (p1, p2) // ignore failure in state
    | B.Expr.FunctionCall (id1, e1), B.Expr.FunctionCall (id2, e2) when id1 = id2 -> matchExprs sub e1 e2
    | B.Expr.Primitive (id1, e1), B.Expr.Primitive (id2, e2) when id1 = id2 -> matchExprs sub e1 e2
    | B.Expr.ArrayIndex (e1, es1), B.Expr.ArrayIndex (e2, es2) -> matchExprs sub (e1 :: es1) (e2 :: es2)
    | B.Expr.ArrayUpdate (e1, es1, ee1), B.Expr.ArrayUpdate (e2, es2, ee2) -> matchExprs sub (ee1 :: e1 :: es1) (ee2 :: e2 :: es2)
    | B.Expr.BvConcat (e1, ee1), B.Expr.BvConcat (e2, ee2) -> matchExprs sub [e1; ee1] [e2; ee2]
    | B.Expr.BvExtract (e1, x1, y1), B.Expr.BvExtract (e2, x2, y2) when x1 = x2 && y1 = y2 -> matchExpr sub (e1, e2)
    | B.Expr.Old e1, e2 -> matchExpr sub (e1, e2)
    | e1, B.Expr.Old e2 -> matchExpr sub (e1, e2)
    | B.Expr.Ref i1, e2 when sub.ContainsKey i1 ->
      if sub.[i1] = e2 then Some sub else None
    | B.Expr.Ref i1, e2 when quantVarSet.ContainsKey i1 ->
      Some (sub.Add (i1, e2))
    | e1, e2 -> if e1 = e2 then Some sub else None
  and matchExprs sub e1 e2 = 
    let rec aux s = function
      | e1 :: ee1, e2 :: ee2 ->
        match matchExpr s (e1, e2) with
          | Some s -> aux s (ee1, ee2)
          | None -> None
      | [], [] -> Some s
      | _ -> die()
    if List.length e1 <> List.length e2 then None
    else aux sub (e1, e2)
  
  let doInfer (body:B.Expr) =
    let resTrig = glist[]

    let candidates = glist[]
    let candidatesSuperterms = gdict()
    let rec checkOne bigger (expr:B.Expr) =
      if candidatesSuperterms.ContainsKey expr then
        None
      else
        candidatesSuperterms.[expr] <- bigger
        let newTr = expr :: bigger
        expr.Map (checkOne newTr) |> ignore
        match getTriggerScore expr with
          | Some (vs, q) -> candidates.Add ((expr, vs, q))
          | None -> ()
        Some expr

    let willLoop tr =
      let isHigh = function
        | Some m -> Map.exists (fun _ -> function B.Expr.Ref _ -> false | _ -> true) m
        | None -> false
      Seq.exists (fun (t, _, _) -> isHigh (matchExpr Map.empty (tr, t))) candidates
      
    let rec inferAtQual qual =
      body.Map (checkOne []) |> ignore
      if dbg then Console.WriteLine ("infer: {0} @{1}", quantTok.Value, qual)
      let disabled = gdict()
      for (tr, vs, q) in candidates do
        if List.forall vs.ContainsKey quantVars && q >= qual then
          if dbg then Console.WriteLine ("cand: {0} {1} sub:{2}", tr, disabled.ContainsKey tr, candidatesSuperterms.[tr] |> listToString)
          if not (disabled.ContainsKey tr) then
            if willLoop tr then ()
            else
              resTrig.Add [tr]
              for superTerm in candidatesSuperterms.[tr] do
                disabled.[superTerm] <- true
      if resTrig.Count = 0 && qual > 1 then // exclude arithmetic here
        inferAtQual (qual - 1)
    inferAtQual maxQuality

    let usedQualZero = ref false
    if resTrig.Count = 0 then
      let coveredVars = gdict()

      let rec loop mtrig =
        if coveredVars.Count = quantVarSet.Count then
          resTrig.Add (List.rev mtrig)
        else
          let checkCandidate cur (tr, (vs:Dict<_,_>), q) =
            let addedVars = Seq.filter (fun v -> not (coveredVars.ContainsKey v)) vs.Keys |> Seq.length
            if dbg then Console.WriteLine ("  check multi: {0} q:{1} v:{2}", tr, q, addedVars)
            if addedVars > 0 then
              match cur with
                | None -> Some (tr, q, addedVars, vs)
                | Some (_, q', av', _) when q' < q || (q' = q && av' < addedVars) -> Some (tr, q, addedVars, vs)
                | _ -> cur
            else cur
          match Seq.fold checkCandidate None candidates with
            | Some (tr, q, av, vs) ->
              if q = 0 then usedQualZero := true
              if dbg then Console.WriteLine ("add multi: {0} q:{1} v:{2}", tr, q, av)
              for k in vs.Keys do coveredVars.[k] <- true
              loop (tr :: mtrig)
            | None -> ()
      loop []

    let finalRes = resTrig |> Seq.toList

    // if the multi-trigger used some bad term inside, try to see if there are any single-triggers
    // with bad terms
    if !usedQualZero then
      resTrig.Clear()
      inferAtQual 0
      if resTrig.Count > 0 then
        resTrig |> Seq.toList
      else
        finalRes
    else
      finalRes

  let dumpTriggers = function
    | [] ->
      helper.Warning (quantTok, 9121, "failed to infer triggers for '" + quantTok.Value + "'")
    | newTrig when helper.Options.DumpTriggers >= 1 ->
      let trToRawString tr = "{" + (List.map (fun t -> t.ToString ()) tr |> String.concat ", ") + "}"
      let trToString tr = 
        let exprToStr e =
          match invMapping.TryGetValue e with
            | true, x :: _ ->
              match e with 
                | B.Expr.FunctionCall ("$idx", _) ->
                  // this is going to be wrong when user writes *(a+i) instead of a[i]
                  "(&)" + x.Token.Value
                | _ -> x.Token.Value
            | _ -> "<<" + e.ToString() + ">>" // this shouldn't happen
        "{" + (List.map exprToStr tr |> String.concat ", ") + "}"
      // this isn't really a warning, but this way it will automatically show up in VS and friends
      helper.Warning (quantTok, 9122, "inferred triggers: " + String.concat " " (Seq.map trToString newTrig) + " for '" + quantTok.Value + "'")
      if helper.Options.DumpTriggers >= 3 then
        helper.Warning (quantTok, 9123, "raw inferred triggers: " + String.concat " " (Seq.map trToRawString newTrig))
    | _ -> ()
  
  // TODO we probably want to run that on triggers only, not before inference
  // the nested ptr_cast(ptr(...)) gets into triggers and causes trouble later
  let rec stripPtrCast (expr:B.Expr) = 
    let aux = function
      | B.FunctionCall ("$ptr_cast", [B.FunctionCall ("$ptr", [_; r]); t]) ->
        Some (stripPtrCast (bCall "$ptr" [t; r]))
      | _ -> None
    expr.Map aux
   
  let isntSkHack = function
    | [B.Expr.FunctionCall ("sk_hack", _)] -> false
    | _ -> true

  member this.Run (body, triggers) = 
    let body = stripPtrCast body
    let triggers =
      if helper.Options.InferTriggers then
        if (List.filter isntSkHack triggers).IsEmpty then
          let newTrig = doInfer body
          dumpTriggers newTrig
          triggers @ newTrig
        else
          if helper.Options.DumpTriggers >= 2 then
            // still infer and ignore the results
            dumpTriggers (doInfer body)
          triggers
      else
        triggers
    body, triggers
          
