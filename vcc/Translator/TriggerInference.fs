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
module IF = Microsoft.Research.Vcc.InformationFlow

module TriggerInference =
  let allowedInTrigger = function
    | B.Expr.Exists _
    | B.Expr.Forall _
    | B.Expr.Lambda _
    | B.Expr.Ite _ -> false
    | B.Expr.Primitive (("&&"|"||"|"=="|"!="|"==>"|"<==>"), _) -> false
    | B.Expr.Primitive (("<"|">"|"<="|">="|"+"|"-"|"*"), _) -> false // TODO add option to allow?
    | B.Expr.FunctionCall (name, _) when name.StartsWith "$in_range" -> false
    | B.Expr.Primitive _ -> true
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
    | B.Expr.IntLiteral _ -> true

  let isAllowedTrigger vars = function
    | B.Expr.Ref _ -> false
    | expr when not (allowedInTrigger expr) -> false
    | expr ->
      let allowed = ref true
      let res = gdict()
      let aux expr =
        if not !allowed then Some expr
        elif allowedInTrigger expr then
          match expr with
            | B.Expr.Ref s -> res.[s] <- true
            | _ -> ()
          None
        else
          allowed := false
          Some expr
      expr.Map aux |> ignore
      !allowed && List.forall res.ContainsKey vars
        
  let inferTriggers (helper:Helper.Env) tok (invMapping:Dict<B.Expr,list<C.Expr>>) vars body triggers = 
    let vars = List.map fst vars // we don't care about the type
    // the nested ptr_cast(ptr(...)) gets into triggers and causes trouble later
    let rec stripPtrCast (expr:B.Expr) = 
      let aux = function
        | B.FunctionCall ("$ptr_cast", [B.FunctionCall ("$ptr", [_; r]); t]) ->
          Some (stripPtrCast (bCall "$ptr" [t; r]))
        | _ -> None
      expr.Map aux

    let candidates = glist[]
    let candidatesSuperterms = gdict()

    let rec checkOne bigger expr =
      if isAllowedTrigger vars expr then
        if candidatesSuperterms.ContainsKey expr then Some expr
        else
          candidatesSuperterms.[expr] <- bigger
          let newTr = expr :: bigger
          expr.Map (checkOne newTr) |> ignore
          candidates.Add expr
          Some expr
      else None

    let varSet = gdict()
    for v in vars do varSet.[v] <- true

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
      | B.Expr.Ref i1, e2 when varSet.ContainsKey i1 ->
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
    
    let willLoop tr =
      let isHigh = function
        | Some m -> Map.exists (fun _ -> function B.Expr.Ref _ -> false | _ -> true) m
        | None -> false
      Seq.exists (fun t -> isHigh (matchExpr Map.empty (tr, t))) candidates
      
    let body = stripPtrCast body
    let triggers =
      if helper.Options.InferTriggers then
        let isntSkHack = function
          | [B.Expr.FunctionCall ("sk_hack", _)] -> false
          | _ -> true
        if (List.filter isntSkHack triggers).IsEmpty then
          body.Map (checkOne []) |> ignore
          let resTrig = glist[]
          for tr in candidates do
            if candidatesSuperterms.ContainsKey tr then
              if willLoop tr then ()
              else
                resTrig.Add [tr]
                for superTerm in candidatesSuperterms.[tr] do
                  candidatesSuperterms.Remove superTerm |> ignore
          if helper.Options.DumpTriggers then
            let trToRawString tr = "{" + (List.map (fun t -> t.ToString ()) tr |> String.concat ", ") + "}"
            let trToString tr = 
              let exprToStr e =
                match invMapping.TryGetValue e with
                  | true, x :: _ ->
                    x.Token.Value
                  | _ -> "???"
              "{" + (List.map exprToStr tr |> String.concat ", ") + "}"
            helper.Warning (tok, 0, "inferred triggers: " + String.concat " " (Seq.map trToString resTrig) + " for " + tok.Value)
            helper.Warning (tok, 0, "raw inferred triggers: " + String.concat " " (Seq.map trToRawString resTrig))
          triggers @ Seq.toList resTrig
        else
          triggers
      else
        triggers
    body, triggers
          
