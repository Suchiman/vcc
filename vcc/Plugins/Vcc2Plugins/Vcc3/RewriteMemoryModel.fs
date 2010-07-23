//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

open Ast

module RewriteMemoryModel =

  let (|PMem|_|) = function
    | App ({ Name = "$mem" }, [h; App ({ Name = "$dot" }, [p; f])]) -> Some (h, p, f)
    | _ -> None

(*
  let (|PWriteInt|_|) = function
    | App ({ Name = "$mem" }, [h; App ({ Name = "$dot", [p; f])]) -> Some (h, p, f)
    | _ -> None
*)

  let MkContext (pass: FromBoogie.Passyficator) =
    [for d in pass.Functions do if d.Name = "$rd" then yield d].Head


  let Rewrite (rdDecl: FuncDecl) = 

    let rec rwExpr = function
      | PMem (h, p, f) -> App (rdDecl, [rwExpr h; rwExpr p; rwExpr f])
      | App (fd, es) -> App (fd, List.map rwExpr es)
      | Binder qd ->
          Binder {
            Kind = qd.Kind
            Vars = qd.Vars
            Triggers = List.map (List.map rwExpr) qd.Triggers
            Attrs = List.map rwAttr qd.Attrs
            Body = rwExpr qd.Body
          }
      | e -> e

    and rwAttr = function
      | ExprAttr (n, e) -> ExprAttr (n, rwExpr e)
      | attr -> attr

    let rwCmd = function
      | Assume (t, e) -> Assume (t, rwExpr e)
      | Assert (t, e) -> Assert (t, rwExpr e)

    let rwBlock (block: Block) =
      {
        Label = block.Label
        Cmds = List.map rwCmd block.Cmds
        Exits = block.Exits
      }

    fun (proc: BlockProc) ->
      {
        Name = proc.Name
        Blocks = List.map rwBlock proc.Blocks
      }
