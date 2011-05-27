//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

module Microsoft.Research.Vcc.DataTypes

open Microsoft.Research.Vcc
open Microsoft.Research.Vcc.Util
open Microsoft.Research.Vcc.TransUtil
open Microsoft.Research.Vcc.CAST

// for datatype definition check that
// - the induction is grounded (there is a non-recursive option)
let checkDatatypeDefinitions (helper:Helper.Env) decls =
  let cache = gdict()
  let rec grounded (td:TypeDecl) =
    match cache.TryGetValue td with
      | true, res -> res
      | _ ->
        cache.Add (td, false)
        let res = td.DataTypeOptions |> List.exists (fun fd -> fd.Parameters |> List.forall (fun p -> groundedType p.Type))
        cache.[td] <- res
        res
  and groundedType = function
    | Type.Ref td ->
      not td.IsDataType || grounded td
    | _ -> true

  let aux = function 
    | Top.TypeDecl td when td.IsDataType ->
      if not (grounded td) then
        helper.Error (td.Token, 9725, "a finite instance of datatype '" + td.Name + "' could never be constructed")
    | _ -> ()
  List.iter aux decls
  decls

// for match stmt check that
// - each branch uses the same type
// - each branch ends with break or return
// - add assert(false) for unused options
// - each datatype option is used at most once

let handleMatchStatement (helper:Helper.Env) desugarSwitch labels expr =
  let usedCases = gdict() 
  let testHd expr fn =
    Expr.Macro (boolBogusEC(), "dt_testhd", [expr; Expr.UserData (boolBogusEC(), fn)])
 
  let rec fallOff nm expr = 
    let self = fallOff nm 
    match expr with
    | Expr.Block (_, stmts, _) -> List.forall self stmts
    | Expr.If (_, _, _, th, el) -> self th || self el
    | Expr.Return _ -> false
    | Expr.Goto (_, nm') -> nm <> nm'
    | _ -> true

  let (|Case|_|) = function
    | Macro (_, "case", [Block (blockEc, stmts, None)]) ->
      Some (blockEc, stmts)
    | Macro (_, "case", [Call _ as call]) ->
      Some (call.Common, [call])
    | _ -> None

  let compileCases expr (dtTd:TypeDecl) cases =
    let rec aux = function
      | Case (blockEc, stmts) :: rest ->
        let unique = helper.UniqueId()
        let case_end = { Name = "match_end_" + unique.ToString() } : LabelId
        let labels = 
          match labels with
            | Some(_, continue_lbl) -> Some(case_end, continue_lbl)
            | None -> Some(case_end, ({ Name = "dummy_label"} : LabelId))
        let rec findPattern acc = function
          | Call (ec, fn, _, args) :: rest ->
            ec, fn, args, List.rev acc, rest
          | x :: rest ->
            findPattern (x :: acc) rest
          | [] -> die()
        let ec, fn, args, pref, suff = findPattern [] stmts
        match usedCases.TryGetValue fn.UniqueId with
          | true, loc ->
            helper.Error (ec.Token, 9726, "the datatype case '" + fn.Name + "' is used more than once", Some loc)
          | false, _ ->
            if dtTd.DataTypeOptions |> _list_mem fn then
              usedCases.Add (fn.UniqueId, ec.Token)
            else
              helper.Error (ec.Token, 9727, "case '" + fn.Name + "' is not a member of " + dtTd.Name)
        let mkAssign (n:int) (e:Expr) =
          let fetch = Macro ({ bogusEC with Type = e.Type }, ("dtp_" + n.ToString()), [expr])
          Macro (voidBogusEC(), "=", [e; fetch])
        let assignments = args |> List.mapi mkAssign
        let body = pref @ assignments @ suff @ [Expr.Label (bogusEC, case_end)]
        let body = desugarSwitch labels (Expr.MkBlock body)
        if fallOff case_end body then
          helper.Error (blockEc.Token, 9728, "possible fall-off from a match case")
        Expr.If ({ ec with Type = Void }, None, testHd expr fn, body, (aux rest))
      | [] ->
        let asserts = 
          dtTd.DataTypeOptions 
            |> List.filter (fun f -> usedCases.ContainsKey f.UniqueId)
            |> List.map (fun f -> 
                  let err = afmte 8030 ("case " + f.Name + " is unhandled when matching {0}") [expr]
                  Expr.MkAssert ((mkNot (testHd expr f)).WithCommon err))
        Expr.MkBlock (asserts @ [Expr.MkAssume (Expr.False)])
      | _ -> die()
    aux cases

  match expr with
    | Macro (ec, "match", expr :: cases) ->
      match expr.Type with
        | Type.Ref dtTd when dtTd.IsDataType ->
          let (save, expr) = cache helper "matched" expr VarKind.SpecLocal
          Some (Expr.MkBlock (save @ [compileCases expr dtTd cases]))
        | tp ->
          helper.Error (ec.Token, 9729, "cannot match on non-datatype " + tp.ToString())
          None
    | _ -> None

let init (helper:Helper.Env) =
  helper.AddTransformer ("datatype-check-defs", Helper.Decl (checkDatatypeDefinitions helper))