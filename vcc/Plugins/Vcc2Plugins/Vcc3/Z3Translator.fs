//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

open Microsoft.Research.Vcc
open Util
open Ast
open Microsoft // for Boogie and Z3


type Z3Translator() =
  let cfg = new Z3.Config()
  let z3 = new Microsoft.Z3.Context(cfg)
  let mutable disposed = false
  let sorts = gdict()
  let functions = gdict()
  let consts = gdict()
  let mutable boundVars = Map.empty
  let cache (dict:Dict<_,_>) f id =
    match dict.TryGetValue id with
      | true, res -> res
      | false, _ ->
        let v = f ()
        dict.Add (id, v)
        v
       
  interface System.IDisposable with
    member this.Dispose() =
      if not disposed then
        disposed <- true
        z3.Dispose()
        cfg.Dispose()
        
  member this.InitCfg() =
    cfg.SetParamValue ("MODEL_PARTIAL", "true")
    cfg.SetParamValue ("MODEL_VALUE_COMPLETION", "false")
    cfg.SetParamValue ("MODEL_HIDE_UNUSED_PARTITIONS", "false")
    cfg.SetParamValue ("MODEL_V1", "true")
    cfg.SetParamValue ("ASYNC_COMMANDS", "false")
    cfg.SetParamValue ("PHASE_SELECTION", "0")
    cfg.SetParamValue ("RESTART_STRATEGY", "0")
    cfg.SetParamValue ("RESTART_FACTOR", "1.5")
    cfg.SetParamValue ("NNF_SK_HACK", "true")
    cfg.SetParamValue ("QI_EAGER_THRESHOLD", "100")
    cfg.SetParamValue ("ARITH_RANDOM_INITIAL_VALUE", "true")
    cfg.SetParamValue ("SORT_AND_OR", "false")
    cfg.SetParamValue ("CASE_SPLIT", "3")
    cfg.SetParamValue ("DELAY_UNITS", "true")
    cfg.SetParamValue ("DELAY_UNITS_THRESHOLD", "16")
    cfg.SetParamValue ("TYPE_CHECK", "true")
    cfg.SetParamValue ("BV_REFLECT", "true")
  
  member this.Sort tp = 
    let aux () = 
      match tp with
        | Type.Bool -> z3.MkBoolSort()
        | Type.Bv n -> z3.MkBvSort ((uint32)n)
        | Type.Int -> z3.MkIntSort()
        | Type.Map (_, _) as m -> z3.MkSort(m.ToString())
        | Type.Named td -> z3.MkSort(td.Name)
    cache sorts aux tp
  
  member this.Function (fn:FuncDecl) =
    let aux () =
      z3.MkFuncDecl (fn.Name + fn.Qualifier, [| for t in fn.ArgTypes -> this.Sort t |], this.Sort fn.RetType)
    cache functions aux fn.Id
  
  member this.Const (v:Var) =
    let aux () =
      z3.MkConst (v.Name, this.Sort v.Typ)
    cache consts aux v.Id
  
  member this.BgAssumptions (pass:FromBoogie.Passyficator) =
    let res = glist[]
    let sorts = glist[]
    let globalsBySort = gdict()
    for g in pass.Globals do
      match globalsBySort.TryGetValue g.Typ with
        | false, _ ->
          sorts.Add g.Typ
          globalsBySort.Add (g.Typ, glist[g])
        | true, l -> l.Add g
    for s in sorts do
      res.Add (z3.MkDistinct [| for g in globalsBySort.[s] -> this.Const g |])
    for a in pass.Axioms do
      res.Add (this.Expr a.Body)
      (*
      TODO:
    for f in pass.Functions do
      if f.Name = "select@" then
        let m = z3.MkConst ("@M", this.Sort f.ArgTypes.[0])
        let i = z3.MkConst ("@i", this.Sort f.ArgTypes.[1])
        let j = z3.MkConst ("@j", this.Sort f.ArgTypes.[1])
        let v = z3.MkConst ("@v", this.Sort f.RetType)
        z3.MkEq (
        z3.MkIte (z3.MkEq
        *)
    res.ToArray()
      
  member this.App (f:FuncDecl, args) =
    let a0, a1 =
      match args with
        | [a0; a1] -> this.Expr a0, this.Expr a1
        | _ -> null, null
    // TODO bitvector stuff
    match f.Name with
      | "+" -> z3.MkAdd (a0, a1)
      | "-" -> z3.MkSub (a0, a1)
      | "*" -> z3.MkMul (a0, a1)
      // % and / remain uninterperted following Boogie (at least for now)
      | "<=" -> z3.MkLe (a0, a1)
      | ">=" -> z3.MkGe (a0, a1)
      | "<" -> z3.MkLt (a0, a1)
      | ">" -> z3.MkGt (a0, a1)
      | "==" -> z3.MkEq (a0, a1)
      | "!=" -> z3.MkNot (z3.MkEq (a0, a1))
      | "&&" -> z3.MkAnd (a0, a1)
      | "||" -> z3.MkOr (a0, a1)
      | "==>" -> z3.MkImplies (a0, a1)
      | "<==>" -> z3.MkIff (a0, a1)
      | _ ->
        let args = [| for a in args -> this.Expr a |]
        match f.Name with
          | "ite@" -> z3.MkIte (args.[0], args.[1], args.[2])
          | "!" -> z3.MkNot args.[0]
          | _ -> z3.MkApp (this.Function f, args)
          
  member this.Expr e =
    match e with
      | Ref v when boundVars.ContainsKey v.Id -> boundVars.[v.Id]
      | Ref v -> this.Const v
      | Lit (Lit.Bool true) -> z3.MkTrue()
      | Lit (Lit.Bool false) -> z3.MkFalse()
      | Lit (Lit.Int n) -> z3.MkIntNumeral (n.ToString())
      | Lit (Lit.Bv (n, sz)) as e -> z3.MkNumeral (n.ToString(), this.Sort e.Type)
      | App (f, args) ->
        this.App (f, args)
      | Binder q ->
        let backup = boundVars
        boundVars <- q.Vars |> List.fold (fun b v -> Map.add v.Id (z3.MkConst (v.Name + "!" + v.Id.ToString(), this.Sort v.Typ)) b) boundVars
        let patterns = [| for t in q.Triggers -> z3.MkPattern [| for tt in t -> this.Expr tt |] |]
        let body = this.Expr q.Body
        let bound = [| for v in q.Vars -> boundVars.[v.Id] |]
        boundVars <- backup
        
        match q.Kind with
          | Lambda -> failwith "Z3 : found lambda"
          | Forall -> z3.MkForall (0u, bound, patterns, body)
          | Exists -> z3.MkExists (0u, bound, patterns, body)
        
      
  member this.Init (pass:FromBoogie.Passyficator) =
    this.InitCfg()
    z3.AssertCnstr (z3.MkAnd (this.BgAssumptions pass))
  
  member this.Assume expr =
    z3.AssertCnstr (this.Expr expr)
    
  member this.Assert expr =
    z3.AssertCnstr (z3.MkNot (this.Expr expr))
    z3.Check () = Z3.LBool.False
  
  member this.Push() =
    z3.Push()
  
  member this.Pop() =
    z3.Pop()
    
