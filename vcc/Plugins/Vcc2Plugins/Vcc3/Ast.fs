//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3
  open Microsoft.Research.Vcc
  open Util

  module Ast =
    type TypeDecl =
      {
        name : string;
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
          | Named td -> td.name
          | Bv n -> "bv" + n.ToString()

    type VarKind =
      | Const
      | ConstUnique
      | Bound
      | Formal
      | Local
      
    type Var =
      {
        id : int
        name : string
        typ : Type
        kind : VarKind
      }
    
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
      
    
    type FuncDecl =
      {
        id : int
        name : string
        retType : Type
        argTypes : list<Type>
        isPolymorphic : bool
      }
    
    type Expr =
      | Ref of Var
      | Lit of Lit
      | App of FuncDecl * list<Expr>
      | Binder of QuantData
      
      member this.WriteTo sb =
        let wr = wrb sb
        let self (p:Expr) = p.WriteTo sb
        match this with
          | Ref n -> wr n.name
          | Lit l -> wr (l.ToString())
          | App (f, [a;b]) when not (String.exists System.Char.IsLetterOrDigit f.name) ->
            wr "("; self a; wr (" " + f.name + " "); self b; wr ")"
          | App (n, args) ->
            doArgsb sb self n.name args
          | Binder q ->
            wr "("
            wr (q.kind.ToString())
            wr " "
            q.vars |> commas sb (fun v -> wr (v.ToString()))
            wr " :: "
            // TODO triggers
            // TODO attrs
            self q.body
            wr ")"
        
      override this.ToString() = toString (this.WriteTo)

            
    and QuantData =
      {
        kind : QuantKind
        vars : list<Var>
        triggers : list<list<Expr>>
        attrs : list<Attribute>
        body : Expr
      }
      
    and Attribute =
      | ExprAttr of string * Expr
      | StringAttr of string * string    
      
    and QuantKind =
      | Forall
      | Exists
      | Lambda
      
      override this.ToString() =
        match this with
          | Forall -> "forall"
          | Lambda -> "lambda"
          | Exists -> "exists"
      

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
                Binder { q with body = self q.body ; triggers = List.map selfs q.triggers }
                

