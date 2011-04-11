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

type TempBinding =
  | TempApp of FuncDecl * array<Z3.Term>
  | TempRef of Var
  
type Z3Translator(helper:Helper.Env, pass:FromBoogie.Passyficator) =
  let cfg = new Z3.Config()
  let mutable z3 = null
  let mutable disposed = false
  let sorts = gdict()
  let functions = gdict()
  let invFunctions = gdict()
  let namedSorts = gdict()
  let mutable tempFunctions = []
  let mutable boundVars = Map.empty
  let mutable stack = []
  let mutable assumedExprs = []
  let mutable smtFileIdx = 0
  let mutable numDeclaredFunctions = 0
  let mutable id = 1500000000
  let mutable modelInterpretation : option<Z3.Term -> option<Expr>> = None
  let xassert b = if not b then failwith "assertion failed"
  
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
      | Type.Map (_, _) as m ->
        let name = m.ToString() 
        let s = z3.MkSort name
        if not (namedSorts.ContainsKey name) then
          namedSorts.Add (name, m)
        s
      | Type.Named td as t ->
        let name = td.Name
        let s = z3.MkSort name
        if not (namedSorts.ContainsKey name) then
          namedSorts.Add (name, t)
        s
  
  member this.Function (fn:FuncDecl) = functions.[fn.Id]
  
  member private this.NextId () =
    id <- id + 1
    id
  
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
    this.DeclareFunctions()
    for g in pass.Functions do
      if g.ArgTypes.IsEmpty && g.Attrs |> hasAttr "unique" then
        let c = z3.MkConst (g.Name, this.Sort g.RetType)
        match globalsBySort.TryGetValue g.RetType with
          | false, _ ->
            sorts.Add g.RetType
            globalsBySort.Add (g.RetType, glist[g])
          | true, l -> l.Add g
    for s in sorts do
      res.Add (z3.MkDistinct [| for g in globalsBySort.[s] -> this.App (g, []) |])
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
      | Ref v -> 
        match boundVars.TryFind v.Id with
          | Some e -> e
          | None -> failwith ("undeclared q-variable " + v.Name)        
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
  
  member this.AssumeSub (sub, expr) =
    let add sub (k, v) =
      Map.add k v sub
    let back = boundVars
    boundVars <- List.fold add boundVars sub
    let term = this.Expr expr
    boundVars <- back
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
        modelInterpretation <- None
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
  
  member this.ReflectSort (s:Z3.Sort) =
    match z3.GetSortKind s with
      | Z3.SortKind.Int -> Type.Int
      | Z3.SortKind.Bool -> Type.Bool
      | Z3.SortKind.Uninterpreted ->
        namedSorts.[z3.GetSymbolString (z3.GetSortName s)]
      | Z3.SortKind.BitVector -> Type.Bv ((int)(z3.GetBvSortSize s))
      | _ -> failwith ""
      
  member this.PopulateModel (model:Z3.Model) =
    let definitions = gdict()
    
    let adddef t def =
      match definitions.TryGetValue t with
        | true, l -> definitions.[t] <- def :: l
        | _ -> definitions.[t] <- [def]
    
    
    for c in model.GetModelConstants() do
      let t = model.Eval (c, [| |])
      adddef t (c, [])
      
    let graphs = model.GetFunctionGraphs()
    for f in graphs.Keys do
      for e in graphs.[f].Entries do
        let r = e.Result
        adddef r (f, [for a in e.Arguments -> a])
    
    let fnFor f = 
      {
        Id = this.NextId()
        Name = z3.GetSymbolString (z3.GetDeclName f)
        Qualifier = ""
        RetType = this.ReflectSort (z3.GetRange f)
        ArgTypes = [for s in z3.GetDomain f -> this.ReflectSort s]
        Attrs = []
        Body = Uninterpreted
      }
      
    let possiblyDeclare f =
      match invFunctions.TryGetValue f with
        | true, bf -> bf
        | _ ->
          let fn = fnFor f
          this.DeclareTempFunction fn
          fn
    
    let properDefs = gdict()
    
    let getApp f (args:list<_>) =
      if args.IsEmpty then
        match properDefs.TryGetValue (z3.MkConst f) with
          | true, v -> v
          | _ -> App (possiblyDeclare f, [])
      else
        App (possiblyDeclare f, args)
    
    let isDefined t =
      match z3.GetTermKind t with
        | Z3.TermKind.Numeral -> true
        | Z3.TermKind.App -> properDefs.ContainsKey t
        | _ -> false
        
    let rec getDefinition t =
      match properDefs.TryGetValue t with
        | true, v -> v
        | _ ->
          match z3.GetTermKind t with
            | Z3.TermKind.Numeral ->
              match z3.GetSortKind (t.GetSort()) with
                | Z3.SortKind.Int ->
                  Expr.Lit (Lit.Int (bigint.Parse (t.GetNumeralString())))
                | Z3.SortKind.BitVector ->
                  Expr.Lit (Lit.Bv (bigint.Parse (t.GetNumeralString()), (int)(z3.GetBvSortSize (t.GetSort()))))
                | _ -> failwith ""
            | _ -> failwith ""
        
    let rec aux() =
      let prevCount = properDefs.Count
      let keys = definitions.Keys |> Seq.toList
      for d in keys do
        match List.tryFind (fun (_, args) -> List.forall properDefs.ContainsKey args) (List.rev definitions.[d]) with
          | Some (f, args) ->
            properDefs.Add (d, getApp f (List.map getDefinition args))
            definitions.Remove d |> ignore
          | None -> ()
      if prevCount < properDefs.Count then aux()
      elif definitions.Count > 0 then
        let d = definitions.Keys |> Seq.head
        if isDefined d then
          properDefs.Add (d, getDefinition d)
        else
          properDefs.Add (d, getApp (d.GetAppDecl()) [])          
        definitions.Remove d |> ignore
        aux()
      
    properDefs.Add (z3.MkTrue(), Expr.True)
    properDefs.Add (z3.MkFalse(), Expr.False)
    definitions.Remove (z3.MkTrue()) |> ignore
    definitions.Remove (z3.MkFalse()) |> ignore
    aux()
    
    let interpret d =
      if isDefined d then Some (getDefinition d)
      else None
    
    interpret      
                                    
  member this.GetAppsExcluding (model:Z3.Model, fn, excl) =
    let interp = 
      match modelInterpretation with
        | Some f -> f
        | None ->
          let f = this.PopulateModel model
          modelInterpretation <- Some f
          f
          
    let res = glist[]
    let graphs = model.GetFunctionGraphs()
    match graphs.TryGetValue (this.Function fn) with
      | true, graph ->
        let excl = this.Function excl
        //printf "valexcl %O\n" fn
        let exclDict = gdict()
        match graphs.TryGetValue excl with
         | true, g ->
           for e in g.Entries do
             //printf "  existing %s\n" (objConcat ", " e.Arguments)           
             exclDict.Add (Seq.toList e.Arguments, true)
         | _ -> ()
        for e in graph.Entries do
          if exclDict.ContainsKey (Seq.toList e.Arguments) then 
            ()
          else
            let rec check acc = function
              | x :: xs ->
                match interp x with
                  | Some y -> check (y :: acc) xs
                  | None -> None
              | [] -> Some (List.rev acc)
            e.Arguments |> Seq.toList |> check [] |> Option.iter res.Add
      | _ -> ()
    res
        
  member this.SaveModel(filename) =
    let model = ref null
    let res = z3.CheckAndGetModel model
    if res = Z3.LBool.False then failwith ""
    use f = System.IO.File.AppendText(filename)
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
