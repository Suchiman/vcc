﻿//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

module Microsoft.Research.Vcc.DataTypes

open Microsoft.Research.Vcc
open Microsoft.Research.Vcc.Util
open Microsoft.Research.Vcc.TransUtil
open Microsoft.Research.Vcc.CAST

// for match stmt check that
// - each branch uses the same type
// - each branch ends with break or return
// - add assert(false) for unused options
// - each datatype option is used at most once

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

let init (helper:Helper.Env) =
  helper.AddTransformer ("datatype-check-defs", Helper.Decl (checkDatatypeDefinitions helper))