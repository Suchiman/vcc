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
      }
      
      override this.ToString() =
        "function " + objConcat " " this.Attrs + " " + this.Name + "(" + objConcat ", " this.ArgTypes + ") : " + this.RetType.ToString()
            
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
      
    type 
      [<NoComparison>] [<ReferenceEquality>] 
      Block =
      {
        mutable Label : string
        mutable Cmds : list<Command>
        mutable Exits : list<Block>
      }
            
    type BlockProc =
      {
        Name : string
        mutable Locals : list<Var>
        mutable Blocks : list<Block>
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
                

