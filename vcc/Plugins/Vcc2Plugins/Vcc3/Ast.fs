//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3
  open Microsoft.Research.Vcc
  open Util

  module Ast =
    let objConcat sep (l : seq<'a>) =
      String.concat sep [for e in l -> e.ToString()]
      
    type TypeDecl =
      {
        Id : int
        Name : string
      }
      
    type Type =
      | Bool
      | Int
      | Map of list<Type> * Type
      | Named of TypeDecl
      | Bv of int
      
      override this.ToString() =
        match this with
          | Bool -> "bool"
          | Int -> "int"
          | Map (f, t) ->
            "[" + String.concat ", " [for t in f -> t.ToString()] + "]" + t.ToString()
          | Named td -> td.Name
          | Bv n -> "bv" + n.ToString()

    type VarKind =
      | Const
      | ConstUnique
      | Bound
      | Local
      
    type Var =
      {
        Id : int
        Name : string
        Typ : Type
        Kind : VarKind
      }
      
      override this.ToString() =
        this.Name + ":" + this.Typ.ToString()
    
    type Lit =
      | Bool of bool
      | Int of bigint
      | Bv of bigint * int
      
      override this.ToString() =
        match this with
          | Bool true -> "true"
          | Bool false -> "false"
          | Int n -> n.ToString()
          | Bv (n, sz) -> n.ToString() + "bv" + sz.ToString()
      
    
    type Expr =
      | Ref of Var
      | Lit of Lit
      | App of FuncDecl * list<Expr>
      | Binder of QuantData
      
      member this.Type =
        match this with
          | Ref v -> v.Typ
          | Lit (Lit.Int _) -> Type.Int
          | Lit (Lit.Bool _) -> Type.Bool
          | Lit (Lit.Bv (_, n)) -> Type.Bv n
          | App (f, _) -> f.RetType
          | Binder { Kind = (Forall|Exists) } -> Type.Bool
          | Binder ({ Kind = Lambda } as q) ->
            Type.Map (q.Vars |> List.map (fun e -> e.Typ), q.Body.Type)
          
      member this.WriteTo sb =
        let wr = wrb sb
        let self (p:Expr) = p.WriteTo sb
        match this with
          | Ref n -> wr n.Name
          | Lit l -> wr (l.ToString())
          | App (f, [a;b]) when not (String.exists System.Char.IsLetterOrDigit f.Name) ->
            wr "("; self a; wr (" " + f.Name + " "); self b; wr ")"
          | App (n, args) ->
            doArgsb sb self n.Name args
          | Binder q ->
            wr "("
            wr (q.Kind.ToString())
            wr " "
            q.Vars |> commas sb (fun v -> wr (v.ToString()))
            wr (objConcat " " q.Attrs)
            wr " :: "
            // TODO triggers
            self q.Body
            wr ")"
        
      override this.ToString() = toString (this.WriteTo)

    and QuantData =
      {
        Kind : QuantKind
        Vars : list<Var>
        Triggers : list<list<Expr>>
        Attrs : list<Attribute>
        Body : Expr
      }
      
    and Attribute =
      | ExprAttr of string * Expr
      | StringAttr of string * string
      
      override this.ToString() =
        match this with
          | ExprAttr (s, e) ->
            "{:" + s + " " + e.ToString() + "}"
          | StringAttr (s, e) ->
            "{:" + s + " \"" + e + "\"}"
      
    and QuantKind =
      | Forall
      | Exists
      | Lambda
      
      override this.ToString() =
        match this with
          | Forall -> "forall"
          | Lambda -> "lambda"
          | Exists -> "exists"
      
    and FuncDecl =
      {
        Id : int
        Name : string           // e.g. bvconcat
        Qualifier : string      // e.g. @32.16; empty for normal functions
        RetType : Type
        ArgTypes : list<Type>
        Attrs : list<Attribute>
        mutable Body : FuncBody
      }
      
      override this.ToString() =
        let pref = "function " + objConcat " " this.Attrs + " " + this.Name         
        match this.Body with
          | Uninterpreted -> 
            pref + "(" + objConcat ", " this.ArgTypes + ") : " + this.RetType.ToString()
          | Expand (vars, body) ->
            pref + "(" + objConcat ", " vars + ") : " + this.RetType.ToString() + "\n" +
              "{ " + body.ToString() + " }"              
    
    and FuncBody =
      | Uninterpreted
      | Expand of list<Var> * Expr
          
    type Axiom =
      {
        Attrs : list<Attribute>
        Body : Expr
      }
      
      override this.ToString() =
        "axiom " + objConcat " " this.Attrs + this.Body.ToString()
      
    type Command =
      {
        Token : Token
        IsAssert : bool
        Condition : Expr
      }
      
      member this.WriteTo sb =        
        wrb sb (if this.IsAssert then "assert " else "assume ")
        this.Condition.WriteTo sb
      
      override this.ToString() = toString (this.WriteTo)
      
    type 
      [<NoComparison>] [<ReferenceEquality>] 
      Block =
      {
        mutable Label : string
        mutable Cmds : list<Command>
        mutable Exits : list<Block>
      }
      
      member this.WriteTo sb =
        let wr = wrb sb
        wr this.Label
        wr ":\n"
        for c in this.Cmds do
          wr "  "
          c.WriteTo sb
          wr "\n"
        wr "  goto {"
        wr (String.concat ", " (this.Exits |> List.map (fun b -> b.Label)))
        wr "}\n"
        
      override this.ToString() = toString (this.WriteTo)
            
    type BlockProc =
      {
        Name : string
        Locals : list<Var>
        Blocks : list<Block>
      }
          
    
    
    type Expr with
      member this.Map (f : Expr -> option<Expr>) : Expr =
        let self (e:Expr) = e.Map f
        let selfs = List.map self
        match f this with
          | Some e -> e
          | None ->
            match this with
              | Ref _
              | Lit _ -> this
              | App (f, args) -> App (f, selfs args)
              | Binder q ->
                Binder { q with Body = self q.Body ; Triggers = List.map selfs q.Triggers }
     
      member this.Expand () =
        let expanding = gdict()
        let rec aux subst = function
          | App ({ Body = Expand (formals, body) } as f, args) ->
            let args = args |> List.map (fun e -> e.Map (aux subst))
            if expanding.ContainsKey f.Id then
              failwith ("recursive expansion of " + f.Name)
            expanding.Add (f.Id, true)
            let subst = List.fold2 (fun subst (f:Var) a -> Map.add f.Id a subst) subst formals args
            let res = body.Map (aux subst)
            expanding.Remove f.Id |> ignore
            Some res
          | Ref v when subst.ContainsKey v.Id ->
            Some (subst.[v.Id])
          | _ -> None
          
        this.Map (aux Map.empty)
                

