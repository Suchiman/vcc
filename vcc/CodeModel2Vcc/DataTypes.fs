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

// for match stmt check that
// - each branch uses the same type
// - each branch ends with break or return
// - add assert(false) for unused options
// - each datatype option is used at most once

// for datatype definition check that
// - attach constructors to type
// - the induction is grounded (there is an non-recursive option)

// this needs to be done early, otherwise pruning will get rid of our constructors if they are not explicitly used, which would be bad
let attachDatatypeCtors (helper:Helper.Env) decls =
  let aux = function
    | Top.FunctionDecl fd when fd.CustomAttr |> hasCustomAttr AttrAsArray ->
      match fd.RetType with
        | Type.Ref td when td.Kind = MathType ->
          td.DataTypeOptions <- fd :: td.DataTypeOptions
        | _ -> die()
    | _ -> ()
  List.iter aux decls
  decls
