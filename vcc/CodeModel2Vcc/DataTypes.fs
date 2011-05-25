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
// - the induction is grounded (there is an non-recursive option)

let x = 0
