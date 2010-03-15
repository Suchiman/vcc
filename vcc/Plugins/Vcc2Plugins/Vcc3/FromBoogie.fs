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
    
  type RoList<'a> = System.Collections.Generic.IList<'a>
  
  type Ctx() =
    let types = gdict()
    let funs = gdict()
    let funList = glist[]
    let typList = glist[]
    let mutable vars = Map.empty
    let mutable id = 0
    
    member this.NextId () =
      id <- id + 1
      id
    
    member this.Functions = funList
    member this.Types = typList
    
    member this.Push () = vars
    member this.Pop v = vars <- v
    
    member this.DeclareVar (v:Boogie.Variable) vr =
      vars <- vars.Add (v.UniqueId, vr)
      
    member this.DeclareFuncDecl (f:FuncDecl) =
      funList.Add f
      funs.Add (f.Name + f.Qualifier, f)
    
    member this.TryGetFuncDecl name =
      match funs.TryGetValue name with
        | true, f -> Some f
        | _ -> None
      
    member this.GetVar (v:Boogie.Variable) =
      (vars.TryFind v.UniqueId).Value
      
    member this.GetType name = 
      cache typList types name 
        (fun () -> {
                     Id = this.NextId()
                     Name = name
                   } : TypeDecl)
      
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
   
  let addVar (ctx:Ctx) k (v:Boogie.Variable) =
    let vr =
      {
        Id = ctx.NextId()
        Name = v.Name
        Typ = unType ctx v.TypedIdent.Type
        Kind = k
      }
    ctx.DeclareVar v vr
    vr
  
  let rec unparse (ctx:Ctx) (expr:Microsoft.Boogie.Expr) =
    match expr with
      | :? Microsoft.Boogie.IdentifierExpr as id -> 
        Expr.Ref (ctx.GetVar id.Decl)
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
        unparse ctx nary.Args.[0]

      | :? Boogie.NAryExpr as nary ->
        let args = [for e in nary.Args -> unparse ctx e]
        let name, getfn =
          let predef name = name, fun () -> failwith (name + " should be predefined")
          let getSelStore sel = fun () ->
            match args.Head.Type with
              | Type.Map (from, ret) as t ->
                {
                  Id = ctx.NextId()
                  Name = if sel then "select@" else "store@"
                  Qualifier = t.ToString()
                  RetType = if sel then ret else t
                  ArgTypes = if sel then t :: from else t :: from @ [ret]
                  Attrs = []
                }
              | t -> failwith ("wrong type in sel/store " + t.ToString())
          match nary.Fun with
            | :? Boogie.FunctionCall as fcall ->
              predef fcall.FunctionName
            | :? Boogie.BinaryOperator as binop ->
              match binop.FunctionName with
                | "==" | "!=" as n ->
                  let t = args.[0].Type
                  let getEq () =
                    {
                      Id = ctx.NextId()
                      Name = n
                      Qualifier = "@" + t.ToString()
                      RetType = Type.Bool
                      ArgTypes = [t; t]
                      Attrs = []
                    }                
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
                {
                  Id = ctx.NextId()
                  Name = "ite@"
                  Qualifier = t.ToString()
                  RetType = t
                  ArgTypes = [Type.Bool; t; t]
                  Attrs = []
                }                
              "ite@" + (t.ToString()), getIte
              
            | _ -> failwith ("wrong nary " + nary.ToString() + " : " + nary.Fun.GetType().ToString())
            
        match ctx.TryGetFuncDecl name with
          | Some f -> Expr.App (f, args)
          | None ->
            let f = getfn ()
            ctx.DeclareFuncDecl f
            Expr.App (f, args)
                
      | :? Boogie.BvConcatExpr as bvConcat ->
        match [for e in bvConcat.Arguments -> unparse ctx (e :?> Microsoft.Boogie.Expr)] with
          | [arg1; arg2] as args ->
            match arg1.Type, arg2.Type with
              | Type.Bv l1, Type.Bv l2 ->
                let name = "concat@" + l1.ToString() + "." + l2.ToString()
                match ctx.TryGetFuncDecl name with
                  | Some f -> Expr.App (f, args)
                  | None ->
                    let f =
                      {
                        Id = ctx.NextId()
                        Name = "concat@"
                        Qualifier = l1.ToString() + "." + l2.ToString()
                        RetType = Type.Bv (l1 + l2)
                        ArgTypes = [Type.Bv l1; Type.Bv l2]
                        Attrs = []
                      }
                    ctx.DeclareFuncDecl f
                    Expr.App (f, args)
              | _ -> failwith ("unexpected argument list of BvConcatExpr")
          | _ -> failwith ("unexpected argument list of BvConcatExpr")
          
      | :? Boogie.BinderExpr as quant ->
        let backup = ctx.Push()
        try 
          let vars = [for v in quant.Dummies -> addVar ctx VarKind.Bound v]
          
          let rec doTrig (t:Boogie.Trigger) =
            if t = null then []
            elif not t.Pos then
              failwith "negative triggers unsupported at this time"
            else [for e in t.Tr -> unparse ctx e] :: doTrig t.Next
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
              Body = unparse ctx quant.Body
              Attrs = unparseAttr ctx quant.Attributes
              Kind = kind
            }
          Expr.Binder q
        finally
          ctx.Pop backup
        
      | s -> 
        //System.Console.WriteLine ("cannot unparse " + s.ToString())
        failwith ("cannot unparse " + s.ToString())
  
  and unparseAttr (ctx:Ctx) (q:Boogie.QKeyValue) =
    if q = null then []
    else
      let cur =
        match q.Params.[0] with
          | :? string as s -> Attribute.StringAttr (q.Key, s)
          | :? Boogie.Expr as e -> Attribute.ExprAttr (q.Key, unparse ctx e)
          | x -> failwith ("wrong attribute value " + x.ToString())
      cur :: unparseAttr ctx q.Next
          
  let declareBuiltins (ctx:Ctx) =
    let makeBinary name t =
      {
        Id = ctx.NextId()
        Name = name
        Qualifier = ""
        RetType = t
        ArgTypes = [t; t]
        Attrs = []
      }                
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
    addRel "<==>" Type.Bool
    
    ctx.DeclareFuncDecl { makeBinary "!" Type.Bool with ArgTypes = [Type.Bool] }
  
  let parse ctx s =
    match BoogiePL.Parser.ParseProposition s with
      | (0, e) -> unparse ctx e
      | _ -> failwith "cannot parse boogie expression"

  type AddCmdInfo =
    | AddEnsures of Boogie.Ensures
    | AddRequires of Boogie.CallCmd * Boogie.Requires
    | AddNothing

  type TokenWithAddCmdInfo (t:Token, ai:AddCmdInfo) =
    inherit ForwardingToken(t, fun () -> t.Value)
    member this.GetAddInfo () = ai           
      
  let doCommand (ctx:Ctx) (cmd:obj) =
    match cmd with
      | :? Boogie.PredicateCmd as asrt ->
        let tok =
          match asrt with
            | :? Boogie.AssertEnsuresCmd as a -> new TokenWithAddCmdInfo (BoogieToken.Strip asrt.tok, AddEnsures a.Ensures) :> Token
            | :? Boogie.AssertRequiresCmd as a -> new TokenWithAddCmdInfo (BoogieToken.Strip asrt.tok, AddRequires (a.Call, a.Requires)) :> Token
            | _ -> BoogieToken.Strip asrt.tok
        { Token = tok
          IsAssert = (asrt :? Boogie.AssertCmd)
          Condition = unparse ctx asrt.Expr }
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
            globals.Add (addVar ctx (if v.Unique then VarKind.ConstUnique else VarKind.Const) v)
          //| :? Boogie.GlobalVariable as v ->
          //  globals.Add (addVar ctx VarKind.Global v)
          | :? Boogie.Function as func ->
            let fundecl =
              {
                Id = ctx.NextId()
                Name = func.Name
                Qualifier = ""
                RetType = unType ctx (func.OutParams.[0].TypedIdent.Type)
                ArgTypes = [for v in func.InParams -> unType ctx v.TypedIdent.Type]
                Attrs = unparseAttr ctx func.Attributes
              }        
            ctx.DeclareFuncDecl fundecl
          | _ -> ()
    
      for d in prog.TopLevelDeclarations do
        match d with
          | :? Boogie.Axiom as ax ->
            axioms.Add ({ Body = unparse ctx ax.Expr; Attrs = unparseAttr ctx ax.Attributes })
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
          addVar ctx VarKind.Local v |> ignore
      
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
      for b in blocks do names.Add (b.Label, b)
      for b in impl.Blocks do
        names.[b.Label].Exits <- resolveExits names b
      
            
      let rec handleStartHere (block:Block) =
        let found = ref false
        let doCmd = function 
          | { Condition = App ({ Name = "$start_here" }, []) } as c -> found := true; c
          | c when not !found -> { c with IsAssert = false }
          | c -> c
        block.Cmds <- block.Cmds |> List.map doCmd
        if not !found then
          List.iter handleStartHere block.Exits            
          
      let res = ref false
      if impl.Proc.CheckBooleanAttribute ("has_start_here", res) && !res then
        handleStartHere blocks.Head
      
      blocks  
   
