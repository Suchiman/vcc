//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

open Ast
open Microsoft.Research.Vcc
open Microsoft // for Boogie


module ToBoogie =


  let Equals x y = (x = y)

  let rec Fold f xs z =
    match xs with
      | [] -> z
      | y :: ys -> Fold f ys (f y z)

  let Cons x xs = x :: xs
  let Append xs = Fold Cons xs

  let rec SplitLast = function
    | [] -> failwith "empty list"
    | [x] -> ([], x)
    | x :: xs ->
        let (ys, y) = SplitLast xs
        (x :: ys, y)


  let Rename (name: string) = name.Replace ("@", "_")


  let rec private trType = function
    | Type.Bool -> BoogieAST.Bool
    | Type.Int -> BoogieAST.Int
    | Type.Map (atys, rty) -> BoogieAST.Map (List.map trType atys, trType rty)
    | Type.Named td -> BoogieAST.Ref (Rename td.Name)
    | Type.Bv size -> BoogieAST.Bv size


  let private mkBuiltin fd =
    match fd.ArgTypes.Length with
      | 1 when List.exists (fst >> Equals fd.Name) BoogieAST.unaryOps ->
        Some (fun es -> BoogieAST.Primitive (fd.Name, es))
      | 2 when List.exists (fst >> Equals fd.Name) BoogieAST.binaryOps ->
        Some (fun es -> BoogieAST.Primitive (fd.Name, es))
      | _ ->
        match fd.Name with
          | "ite@" -> Some (function
            | [e1; e2; e3] -> BoogieAST.Ite (e1, e2, e3)
            | _ -> failwith "bad number of arguments for ite@")
          | "select@" -> Some (fun es -> BoogieAST.ArrayIndex (es.Head, es.Tail))
          | "store@" ->
            Some (fun es ->
              let (es', e') = SplitLast (es.Tail)
              BoogieAST.ArrayUpdate (es.Head, es', e'))
          | "concat@" -> Some (function
            | [e1; e2] -> BoogieAST.BvConcat (e1, e2)
            | _ -> failwith "bad number of arguments for concat@")
          | _ -> None

  let private IsBuiltin fd = Option.isSome (mkBuiltin fd)

  let rec private trExpr = function
    | Ref var -> BoogieAST.Expr.Ref (Rename var.Name)
    | Lit (Lit.Bool b) -> BoogieAST.BoolLiteral b
    | Lit (Lit.Int i) -> BoogieAST.IntLiteral i
    | Lit (Lit.Bv (bv, sz)) -> BoogieAST.BvLiteral (bv, sz)
    | PNot e -> BoogieAST.Primitive ("!", [trExpr e])
    | PAnd (e1, e2) -> BoogieAST.Primitive ("&&", [trExpr e1; trExpr e2])
    | POr (e1, e2) -> BoogieAST.Primitive ("||", [trExpr e1; trExpr e2])
    | App (f, es) ->
        match mkBuiltin f with
          | Some mkapp -> mkapp (List.map trExpr es)
          | None ->
            if f.ArgTypes.IsEmpty then BoogieAST.Expr.Ref (Rename f.Name)
            else BoogieAST.FunctionCall (Rename f.Name, List.map trExpr es)
    | Binder qd ->
        let vars = List.map (fun (v:Var) -> (Rename v.Name, trType v.Typ)) qd.Vars
        let trigs = List.map (List.map trExpr) qd.Triggers
        let attrs = List.map trAttribute qd.Attrs
        let body = trExpr qd.Body
        match qd.Kind with
          | Forall -> BoogieAST.Forall (Token.NoToken, vars, trigs, attrs, body)
          | Exists -> BoogieAST.Exists (Token.NoToken, vars, trigs, attrs, body)
          | Lambda -> BoogieAST.Lambda (Token.NoToken, vars, attrs, body)

  and private trAttribute = function
    | ExprAttr (name, e) -> BoogieAST.ExprAttr (Rename name, trExpr e)
    | StringAttr (name, value) -> BoogieAST.StringAttr (Rename name, value)


  let private trCommand = function
    | Assume (_, e) -> BoogieAST.Assume (trExpr e)
    | Assert (tok, e) -> BoogieAST.Assert (tok, trExpr e)

  let private trExit = function
    | [] -> BoogieAST.Return (Token.NoToken)
    | bs -> BoogieAST.Goto (Token.NoToken, List.map (fun b -> Rename b.Label) bs)

  let private trBlock b =
    [BoogieAST.Label (Token.NoToken, Rename b.Label)]
    |> Fold (trCommand >> Cons) b.Cmds
    |> Cons (trExit b.Exits)
    |> List.rev
    |> BoogieAST.Block


  let private trTypeDecl (td: TypeDecl) = BoogieAST.TypeDef (Rename td.Name)

  let private IsUnique = function
    | Attribute.StringAttr ("vcc3", "unique") -> true
    | _ -> false

  let private trFuncDecl fd =
    if fd.ArgTypes.IsEmpty then
      BoogieAST.Const {
        Unique = List.exists IsUnique fd.Attrs
        Name = Rename fd.Name
        Type = trType fd.RetType
      }
    else
      let dest_var (v: Var) = (Rename v.Name, trType v.Typ)
      let mk_var i ty = ("x" + (i+1).ToString(), trType ty)
      let vars, body =
        match fd.Body with
          | Expand (vars, body) -> List.map dest_var vars, Some (trExpr body)
          | _ -> List.mapi mk_var fd.ArgTypes, None
      BoogieAST.Function (
        trType fd.RetType,
        List.map trAttribute fd.Attrs,
        Rename fd.Name,
        vars,
        body)

  let private trAxiom ax = BoogieAST.Axiom (trExpr ax.Body)

  let private trProc proc =
    BoogieAST.Proc {
      Name = Rename proc.Name
      InParms = []
      OutParms = []
      Contracts = []
      Locals = []
      Body = Some (BoogieAST.Block (List.map trBlock proc.Blocks))
      Attributes = []
    }


  let private TranslateDeclarations (pass: FromBoogie.Passyficator) =
    []
    |> Append [for d in pass.Types -> trTypeDecl d]
    |> Append [for d in pass.Functions do if not (IsBuiltin d) then yield (trFuncDecl d)]
    |> Append [for d in pass.Axioms -> trAxiom d]
    |> List.rev

  let Translate (pass: FromBoogie.Passyficator, proc: BlockProc, dump: option<string>) =
    let prog = BoogieAST.trProgram (TranslateDeclarations pass)
    prog.TopLevelDeclarations.AddRange (BoogieAST.trDecl (trProc proc))

    prog.Resolve () |> ignore    // FIXME: check outcome for errors
    prog.Typecheck () |> ignore  // FIXME: check outcome for errors

    if dump.IsSome then
      using (new System.IO.StreamWriter (dump.Value)) (fun tw ->
        using (new Microsoft.Boogie.TokenTextWriter (dump.Value, tw, false)) (fun wr ->
          prog.Emit (wr)))

    let impl = prog.TopLevelDeclarations.Find (function
      | :? Boogie.Implementation as impl -> true
      | _ -> false) :?> Boogie.Implementation

    prog, impl
    