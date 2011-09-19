//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

open Microsoft.Research.Vcc
open Util
open Ast
open Microsoft // for Boogie


module FromBoogie =

  let cache (lst:GList<_>) (dict:Dict<_,_>) name f =
    match dict.TryGetValue name with
      | true, res -> res
      | false, _ ->
        let v = f ()
        lst.Add v
        dict.Add (name, v)
        v
    
  type Ctx() =
    let types = gdict()
    let funs = gdict()
    let funList = glist[]
    let typList = glist[]
    let mutable vars = Map.empty
    let mutable id = 100
    
    member this.Init () =
      this.DeclareFuncDecl Expr.Ite
      
    member this.NextId () =
      id <- id + 1
      id
    
    member this.Functions = funList
    member this.Types = typList
    
    member this.Push () = vars
    member this.Pop v = vars <- v
    
    member this.DeclareVar (v:Boogie.Variable) (vr:Var) =
      vars <- vars.Add (v.UniqueId, vr)
    
    //member this.GetVars () =
    //  vars |> Map.fold (fun acc _ e -> e :: acc) []
      
    member this.DeclareFuncDecl (f:FuncDecl) =
      funList.Add f
      funs.Add (f.Name + f.Qualifier, f)
    
    member this.TryGetFuncDecl name =
      match funs.TryGetValue name with
        | true, f -> Some f
        | _ -> None
      
    member this.GetVar (v:Boogie.Variable) =
      match funs.TryGetValue v.Name with
        | true, f ->
          if not f.ArgTypes.IsEmpty then failwith ""
          Expr.App (f, [])
        | _ ->
          match vars.TryFind v.UniqueId with
            | Some v -> Expr.Ref v
            | None ->
              failwith ("unregistered variable " + v.Name + " [" + v.UniqueId.ToString() + "] : " + v.TypedIdent.Type.ToString())
      
    member this.GetType name = 
      cache typList types name 
        (fun () -> {
                     Id = this.NextId()
                     Name = name
                   } : TypeDecl)
    
    member this.NewFunc name args ret =
      {
        Id = this.NextId()
        Name = name
        Qualifier = ""
        RetType = ret
        ArgTypes = args
        Attrs = []
        Body = Uninterpreted
      }                

      
  let rec unType (ctx:Ctx) (t:Boogie.Type) = 
    if t.IsBool then Type.Bool
    elif t.IsInt then Type.Int
    elif t.IsBv then Type.Bv(t.BvBits)
    else
      match t with
        | :? Boogie.CtorType as u ->
          Type.Named (ctx.GetType u.Decl.Name)
        | :? Boogie.UnresolvedTypeIdentifier as u ->
          Type.Named (ctx.GetType u.Name)
        | :? Boogie.TypeSynonymAnnotation as s ->
          unType ctx s.ExpandedType
        | :? Boogie.MapType as a ->
          Type.Map ([for a in a.Arguments -> unType ctx a], unType ctx a.Result)
        | _ ->
          failwith ("cannot handle boogie type " + t.ToString() + " : " + t.GetType().ToString())
   
  let addVar (ctx:Ctx) (v:Boogie.Variable) =
    let vr =
      {
        Id = ctx.NextId()
        Name = v.Name
        Typ = unType ctx v.TypedIdent.Type
      }
    ctx.DeclareVar v vr
    vr
  
  let mkFun0 (ctx:Ctx) (v:Boogie.Variable) =
    ctx.NewFunc v.Name [] (unType ctx v.TypedIdent.Type)
  
  let addFun0 (ctx:Ctx) v =
    ctx.DeclareFuncDecl (mkFun0 ctx v)
    
  let rec doUnparse (ctx:Ctx) (expr:Microsoft.Boogie.Expr) =
    match expr with
      | :? Microsoft.Boogie.IdentifierExpr as id -> 
        ctx.GetVar id.Decl
      | :? Microsoft.Boogie.LiteralExpr as lit ->
        if lit.isBigNum then
          Expr.Lit (Lit.Int (bigint.Parse (lit.asBigNum.ToString())))
        elif lit.IsFalse then
          Expr.Lit (Lit.Bool false)
        elif lit.IsTrue then
          Expr.Lit (Lit.Bool true)
        else match lit.Val with
               | :? Microsoft.Boogie.BvConst as bvConst ->
                 Expr.Lit(Lit.Bv (bigint.Parse(bvConst.Value.ToString()), bvConst.Bits))
               | _ -> failwith ("cannot unparse lit " + lit.ToString())
               
      | :? Boogie.NAryExpr as nary when (nary.Fun :? Boogie.TypeCoercion) ->
        doUnparse ctx nary.Args.[0]

      | :? Boogie.NAryExpr as nary ->
        let args = [for e in nary.Args -> doUnparse ctx e]
        let name, getfn =
          let predef name = name, fun () -> failwith (name + " should be predefined")
          let getSelStore sel = fun () ->
            match args.Head.Type with
              | Type.Map (from, ret) as t ->
                let fnc =
                  if sel then ctx.NewFunc "select@" (t :: from)         ret
                  else        ctx.NewFunc "store@"  (t :: from @ [ret]) t
                { fnc with Qualifier = t.ToString() }
              | t -> failwith ("wrong type in sel/store " + t.ToString())
          match nary.Fun with
            | :? Boogie.FunctionCall as fcall ->
              let func = fcall.Func
              if (ctx.TryGetFuncDecl func.Name).IsNone && func.Name.StartsWith "lambda@" then
                declareFunction ctx func
              predef fcall.FunctionName
            | :? Boogie.BinaryOperator as binop ->
              match binop.FunctionName with
                | "<==>" | "==" | "!=" as n ->
                  let n = if n = "<==>" then "==" else n
                  let t = args.[0].Type
                  let getEq () =
                    { ctx.NewFunc n [t; t] Type.Bool with Qualifier = "@" + t.ToString() }
                  n + "@" + t.ToString(), getEq                
                | n -> predef n
            | :? Boogie.UnaryOperator as unop -> predef unop.FunctionName
            | :? Boogie.MapSelect as sel ->
              "select@" + (args.Head.Type.ToString()), getSelStore true
            | :? Boogie.MapStore ->            
              "store@" + (args.Head.Type.ToString()), getSelStore false
            | :? Boogie.IfThenElse ->
              let t = args.Tail.Head.Type
              let getIte () =
                { ctx.NewFunc "ite@" [Type.Bool; t; t] t with Qualifier = t.ToString() }
              "ite@" + (t.ToString()), getIte
              
            | _ -> failwith ("wrong nary " + nary.ToString() + " : " + nary.Fun.GetType().ToString())
            
        match ctx.TryGetFuncDecl name with
          | Some f -> Expr.App (f, args)
          | None ->
            let f = getfn ()
            ctx.DeclareFuncDecl f
            Expr.App (f, args)
                
      | :? Boogie.BvConcatExpr as bvConcat ->
        match [for e in bvConcat.Arguments -> doUnparse ctx (e :?> Microsoft.Boogie.Expr)] with
          | [arg1; arg2] as args ->
            match arg1.Type, arg2.Type with
              | Type.Bv l1, Type.Bv l2 ->
                let name = "concat@" + l1.ToString() + "." + l2.ToString()
                match ctx.TryGetFuncDecl name with
                  | Some f -> Expr.App (f, args)
                  | None ->
                    let f =
                      { ctx.NewFunc "concat@" [Type.Bv l1; Type.Bv l2] (Type.Bv (l1 + l2)) 
                        with Qualifier = l1.ToString() + "." + l2.ToString() }
                    ctx.DeclareFuncDecl f
                    Expr.App (f, args)
              | _ -> failwith ("unexpected argument list of BvConcatExpr")
          | _ -> failwith ("unexpected argument list of BvConcatExpr")
          
      | :? Boogie.BinderExpr as quant ->
        let backup = ctx.Push()
        try 
          let vars = [for v in quant.Dummies -> addVar ctx v]
          
          let rec doTrig (t:Boogie.Trigger) =
            if t = null then []
            elif not t.Pos then
              failwith "negative triggers unsupported at this time"
            else [for e in t.Tr -> doUnparse ctx e] :: doTrig t.Next
          let triggers = 
            match quant with
              | :? Boogie.QuantifierExpr as quant -> doTrig quant.Triggers
              | _ -> []
          let kind =
            match quant with
              | :? Boogie.ExistsExpr -> QuantKind.Exists
              | :? Boogie.ForallExpr -> QuantKind.Forall
              | :? Boogie.LambdaExpr -> QuantKind.Lambda
              | _ -> failwith ""
          let q =
            {
              Vars = vars
              Triggers = triggers
              Body = doUnparse ctx quant.Body
              Attrs = unparseAttr ctx quant.Attributes
              Kind = kind
            }
          Expr.Binder q
        finally
          ctx.Pop backup
        
      | s -> 
        //System.Console.WriteLine ("cannot unparse " + s.ToString())
        failwith ("cannot unparse " + s.ToString())
  
  and unparse (ctx:Ctx) (expr:Microsoft.Boogie.Expr) =
    let nf self = function
      | PApp (_, "&&", [a; b]) ->
        Some (Expr.MkAnd (self a, self b))
      | PApp (_, "||", [a; b]) ->
        Some (Expr.MkOr (self a, self b))
      | PApp (_, "==>", [a; b]) ->
        Some (Expr.MkImpl (self a, self b))
      | PApp (_, "!", [a]) -> Some (Expr.MkNot (self a))
      | _ -> None
      
    (doUnparse ctx expr).SelfMap nf
      
  and unparseAttr (ctx:Ctx) (q:Boogie.QKeyValue) =
    if q = null then []
    else
      let cur =
        match q.Params.[0] with
          | :? string as s -> Attribute.StringAttr (q.Key, s)
          | :? Boogie.Expr as e -> Attribute.ExprAttr (q.Key, unparse ctx e)
          | x -> failwith ("wrong attribute value " + x.ToString())
      cur :: unparseAttr ctx q.Next
          
  and declareFunction (ctx:Ctx) (func:Boogie.Function) =
    let retType = unType ctx (func.OutParams.[0].TypedIdent.Type)
    let argTypes = [for v in func.InParams -> unType ctx v.TypedIdent.Type]
    let fundecl =
      { ctx.NewFunc func.Name argTypes retType
        with Attrs = unparseAttr ctx func.Attributes }
    ctx.DeclareFuncDecl fundecl
  
  let declareBuiltins (ctx:Ctx) =
    let makeBinary name t =
      ctx.NewFunc name [t;t] t
    let addBinary name t = ctx.DeclareFuncDecl (makeBinary name t)
    let addRel name t = ctx.DeclareFuncDecl { makeBinary name t with RetType = Type.Bool }
    addBinary "+" Type.Int
    addBinary "-" Type.Int
    addBinary "/" Type.Int
    addBinary "*" Type.Int
    addBinary "%" Type.Int
    
    addRel ">" Type.Int
    addRel "<" Type.Int
    addRel ">=" Type.Int
    addRel "<=" Type.Int
    
    addRel "&&" Type.Bool
    addRel "||" Type.Bool
    addRel "==>" Type.Bool
    
    ctx.DeclareFuncDecl { makeBinary "!" Type.Bool with ArgTypes = [Type.Bool] }

  let doCommand (ctx:Ctx) (cmd:obj) =
    match cmd with
      | :? Boogie.AssumeCmd as assu ->
        Command.Assume (BoogieToken.Strip assu.tok, unparse ctx assu.Expr)
      | :? Boogie.AssertCmd as asrt ->
        let getCE retTok =
          let trace = new Boogie.BlockSeq()
          match asrt with
            | :? Boogie.AssertEnsuresCmd as a ->
              let tok =
                match retTok with
                  | Some t -> BoogieToken t :> Boogie.IToken
                  | None -> asrt.tok
              new Boogie.ReturnCounterexample (trace, new Boogie.ReturnCmd(tok), a.Ensures) :> Boogie.Counterexample
            | :? Boogie.AssertRequiresCmd as a ->
              new Boogie.CallCounterexample (trace, a.Call, a.Requires) :> Boogie.Counterexample
            | _ ->
              new Boogie.AssertCounterexample (trace, asrt) :> Boogie.Counterexample
        let body = unparse ctx asrt.Expr
        Command.Assert (new CounterexampleToken (BoogieToken.Strip asrt.tok, getCE), body)
      | _ -> failwith ("unexpected boogie command in passified program " + cmd.ToString())
            
  let doBlock (ctx:Ctx) (b:Boogie.Block) =
    { Label = b.Label 
      Exits = [] 
      Cmds = [ for c in b.Cmds -> doCommand ctx c ] }
   
  let resolveExits (names:Dict<_,_>) (b:Boogie.Block) =
    match b.TransferCmd with
      | :? Boogie.ReturnCmd -> []
      | :? Boogie.GotoCmd as goto ->
        [ for t in goto.labelTargets -> names.[t.Label] ]
      | _ -> failwith ("unexpected transfer cmd " + if b.TransferCmd = null then "(null)" else b.TransferCmd.ToString())
    
      
  type Passyficator(prog:Boogie.Program, helper:Helper.Env, options:list<string>) =
    inherit VC.VCGen(prog, null, false)
    
    let ctx = Ctx()
    do ctx.Init()
    let axioms = glist[]
    let globals = glist[]
    
    member this.Init () =
      declareBuiltins ctx
      
      for d in prog.TopLevelDeclarations do
        match d with
          | :? Boogie.TypeCtorDecl as td ->
            if td.Arity <> 0 then failwith "polymorphic types not supported at this time"
            ctx.GetType td.Name |> ignore
          | _ -> ()
    
      for d in prog.TopLevelDeclarations do
        match d with
          | :? Boogie.Constant as v ->
            let f = mkFun0 ctx v
            let f = if v.Unique then { f with Attrs = Attribute.StringAttr ("vcc3", "unique") :: f.Attrs } else f
            ctx.DeclareFuncDecl f
          | :? Boogie.GlobalVariable as v ->
            // it behaves like a local variable at this level
            addFun0 ctx v
          | :? Boogie.Function as func ->
            declareFunction ctx func
          | _ -> ()
    
      for d in prog.TopLevelDeclarations do
        match d with
          | :? Boogie.Axiom as ax ->
            axioms.Add ({ Body = unparse ctx ax.Expr; Attrs = unparseAttr ctx ax.Attributes })
          | :? Boogie.Function as func when func.Body <> null ->            
            let fundecl = (ctx.TryGetFuncDecl func.Name).Value
            let backup = ctx.Push()
            try
              let vars = [for v in func.InParams -> addVar ctx v]
              let body = unparse ctx func.Body                 
              fundecl.Body <- Expand (vars, body)
            finally
              ctx.Pop backup
          | _ -> ()
    
    member this.Axioms = axioms :> RoList<_>
    member this.Globals = globals :> RoList<_>
    member this.Types = ctx.Types :> RoList<_>
    member this.Functions = ctx.Functions :> RoList<_>
    
    member this.Passify (impl:Boogie.Implementation) =
      this.ConvertCFG2DAG (impl, prog)
      this.PassifyImpl (impl, prog) |> ignore
      
      let addVars (vs:Boogie.VariableSeq) =
        for v in vs do
          addFun0 ctx v 
          
      let backup = ctx.Push ()
      let blocks =
        try
          addVars impl.InParams
          addVars impl.OutParams      
          addVars impl.LocVars              
          [ for b in impl.Blocks -> doBlock ctx b ]
        finally
          ctx.Pop backup
        
      let names = new Dict<_,_>()
      for b in blocks do
        names.Add (b.Label, b)
      for b in impl.Blocks do
        names.[b.Label].Exits <- resolveExits names b
      
            
      let rec handleStartHere (block:Block) =
        let found = ref false
        let doCmd = function 
          | Assume (_, App ({ Name = "$start_here" }, [])) as c -> found := true; c
          | c when not !found -> c.ToAssume()
          | c -> c
        block.Cmds <- block.Cmds |> List.map doCmd
        if not !found then
          List.iter handleStartHere block.Exits            
          
      let res = ref false
      if impl.Proc.CheckBooleanAttribute ("has_start_here", res) && !res then
        handleStartHere blocks.Head
      
      {
        Blocks = blocks
        Name = impl.Name
      }
   
