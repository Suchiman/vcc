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


type TranslatorState =
  {
    assumedExprs : list<Z3.Term>
    tempFunctions : list<FuncDecl>
  }
  
type Z3Translator(helper:Helper.Env, pass:FromBoogie.Passyficator) =
  let cfg = new Z3.Config()
  let mutable z3 = null
  let mutable disposed = false
  let sorts = gdict()
  let functions = gdict()
  let invFunctions = gdict()
  let consts = gdict()
  let locals = gdict()
  let mutable tempFunctions = []
  let mutable boundVars = Map.empty
  let mutable stack = []
  let mutable assumedExprs = []
  let mutable smtFileIdx = 0
  let mutable numDeclaredFunctions = 0
  
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
    cfg.SetParamValue ("MODEL", "true")
    z3 <- new Microsoft.Z3.Context(cfg)
  
  member this.Sort tp = 
    match tp with
      | Type.Bool -> z3.MkBoolSort()
      | Type.Bv n -> z3.MkBvSort ((uint32)n)
      | Type.Int -> z3.MkIntSort()
      | Type.Map (_, _) as m -> z3.MkSort(m.ToString())
      | Type.Named td -> z3.MkSort(td.Name)
  
  member this.Function (fn:FuncDecl) = functions.[fn.Id]
  member this.Const (v:Var) =
    match locals.TryGetValue v.Id with
      | true, t -> t
      | _ ->
        match consts.TryGetValue v.Id with
          | true, t -> t
          | false, _ ->
            failwith ("undeclared global " + v.ToString())
  
  member private this.DeclareFunction (fn:FuncDecl) =
    let decl = z3.MkFuncDecl (fn.Name + fn.Qualifier, [| for t in fn.ArgTypes -> this.Sort t |], this.Sort fn.RetType)
    functions.Add (fn.Id, decl)
    invFunctions.Add (decl, fn)
  
  member this.DeclareTempFunction (fn:FuncDecl) =
    tempFunctions <- fn :: tempFunctions
    this.DeclareFunction fn
        
  member private this.DeclareFunctions () =
    for i = numDeclaredFunctions to pass.Functions.Count - 1 do
      this.DeclareFunction pass.Functions.[i]
    
  member this.BgAssumptions () =
    let res = glist[]
    let sorts = glist[]
    let globalsBySort = gdict()
    for g in pass.Globals do
      consts.Add (g.Id, z3.MkConst (g.Name, this.Sort g.Typ))
      match g.Kind with
        | VarKind.ConstUnique ->
          match globalsBySort.TryGetValue g.Typ with
            | false, _ ->
              sorts.Add g.Typ
              globalsBySort.Add (g.Typ, glist[g])
            | true, l -> l.Add g
        | _ -> ()
    for s in sorts do
      res.Add (z3.MkDistinct [| for g in globalsBySort.[s] -> this.Const g |])
    this.DeclareFunctions()
    numDeclaredFunctions <- pass.Functions.Count
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
      //| "&&" -> z3.MkAnd (a0, a1)
      //| "||" -> z3.MkOr (a0, a1)
      | "==>" -> z3.MkImplies (a0, a1)
      | "<==>" -> z3.MkIff (a0, a1)
      | _ ->
        let args = [| for a in args -> this.Expr a |]
        match f.Name with
          | "ite@" -> z3.MkIte (args.[0], args.[1], args.[2])
          //| "!" -> z3.MkNot args.[0]
          | _ -> z3.MkApp (this.Function f, args)
          
  member this.Expr e =
    match e with
      | Ref v when boundVars.ContainsKey v.Id -> boundVars.[v.Id]
      | Ref v -> this.Const v
      | Lit (Lit.Bool true) -> z3.MkTrue()
      | Lit (Lit.Bool false) -> z3.MkFalse()
      | Lit (Lit.Int n) -> z3.MkIntNumeral (n.ToString())
      | Lit (Lit.Bv (n, sz)) as e -> z3.MkNumeral (n.ToString(), this.Sort e.Type)
      | PAnd (a, b) -> z3.MkAnd (this.Expr a, this.Expr b)
      | POr (a, b) -> z3.MkOr (this.Expr a, this.Expr b)
      | PNot (a) -> z3.MkNot (this.Expr a)
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
        
      
  member private this.Save term =
    assumedExprs <- term :: assumedExprs
      
  member this.BeginProc (p:BlockProc) =
    this.Push()
    this.DeclareFunctions()
    for l in p.Locals do
      if l.Kind <> VarKind.Local then failwith ""
      locals.Add (l.Id, z3.MkConst (l.Name, this.Sort l.Typ))
  
  member this.FinishProc () =
    this.Pop()
    
  member this.Init () =
    this.InitCfg()
    let term = z3.MkAnd (this.BgAssumptions ())
    this.Save term
    z3.AssertCnstr term
  
  member this.Assume expr =
    let term = this.Expr expr
    this.Save term
    z3.AssertCnstr term
    
  member this.Assert expr =
    let term = z3.MkNot (this.Expr expr)
    this.Save term
    z3.AssertCnstr term    
    z3.Check () = Z3.LBool.False
  
  member this.AssertOrModel expr =
    let term = z3.MkNot (this.Expr expr)
    this.Save term
    z3.AssertCnstr term
    let model = ref null
    match z3.CheckAndGetModel model with
      | Z3.LBool.False -> None
      | _ ->
        if !model = null then failwith ""
        Some !model
  
  member this.Push() =
    z3.Push()
    let st =
      {
        assumedExprs = assumedExprs
        tempFunctions = tempFunctions
      }
    tempFunctions <- []
    stack <- st :: stack
  
  member this.Pop() =
    let chk b = if not b then failwith ""
    for f in tempFunctions do
      let decl = functions.[f.Id]      
      functions.Remove f.Id |> chk
      invFunctions.Remove decl |> chk
    z3.Pop()
    let st = stack.Head
    stack <- stack.Tail
    assumedExprs <- st.assumedExprs
    tempFunctions <- st.tempFunctions
  
  member this.Reflect (t:Z3.Term) =
    match t.GetKind() with
      | Z3.TermKind.Numeral ->
        match z3.GetSortKind (t.GetSort()) with
          | Z3.SortKind.Int ->
            Expr.Lit (Lit.Int (bigint.Parse (t.GetNumeralString())))
          | Z3.SortKind.BitVector ->
            Expr.Lit (Lit.Bv (bigint.Parse (t.GetNumeralString()), (int)(z3.GetBvSortSize (t.GetSort()))))
          | _ -> failwith ""
      | _ -> failwith ""
  
  member this.GetAppsExcluding (model:Z3.Model, fn, excl) =
    let res = glist[]
    let graphs = model.GetFunctionGraphs()
    match graphs.TryGetValue (this.Function fn) with
      | true, graph ->
        for e in graph.Entries do
          let maybeDone = model.Eval (this.Function excl, e.Arguments)
          if maybeDone <> null then
            res.Add [for a in e.Arguments -> this.Reflect a]
      | _ -> ()
    res
        
  member this.SaveModel(filename) =
    let model = ref null
    let res = z3.CheckAndGetModel model
    if res = Z3.LBool.False then failwith ""    
    use f = System.IO.File.CreateText(filename)
    (!model).Display f
    (!model).Dispose()
    
  member this.Smt() =
    //System.Diagnostics.Debugger.Launch() |> ignore
    let assumptions = glist[]
    let rec add (t:Z3.Term) =
      if t.GetKind() = Z3.TermKind.App && t.GetAppDecl().GetKind() = Z3.DeclKind.And then
        Seq.iter add (t.GetAppArgs())
      else
        assumptions.Add t
    match assumedExprs with
      | form :: rest ->
        List.iter add rest
        assumptions.Reverse()
        z3.BenchmarkToSmtlib ("vcc3bench", "UFNIA", "unknown", null, assumptions.ToArray(), form)
      | _ -> failwith ""
    
  member this.SmtToFile() =
    let fn = sprintf "b%04d.smt" smtFileIdx
    smtFileIdx <- smtFileIdx + 1
    System.IO.File.WriteAllText (fn, this.Smt())
    fn
