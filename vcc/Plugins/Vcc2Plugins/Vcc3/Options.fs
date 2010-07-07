//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

open Microsoft.Research.Vcc
open Microsoft.Research.Vcc.Util
open Microsoft.Research.Vcc3.Ast

type Options =
  {
    mutable smt_dump : bool
    mutable custom_inst : bool
    mutable time : bool
    mutable model_file : string
    mutable trace : int
    mutable dump_boogie : bool
  }
  
  static member Create () =
    {
      smt_dump = false
      custom_inst = false
      time = false
      model_file = "error.vccmodel"
      trace = 1
      dump_boogie = false
    }
    
  member private this.Lower (s:string) =
    ((s.ToLower (System.Globalization.CultureInfo.InvariantCulture)).Replace ("_", "")).Replace ("-", "")
  
  member this.Set (opt:string) =
    let idx = opt.IndexOf '='
    let name, value =
      if idx >= 0 then
        this.Lower (opt.Substring (0, idx)), opt.Substring (idx + 1)
      else
        this.Lower opt, "true"
    let did = ref false
    let stringOpt n f =
      if n = name then 
        did := true
        f value
    let boolOpt n f =
      if n = name then
        did := true      
        let v =
          match this.Lower value with
            | "true" | "t"
            | "yes" | "y"
            | "1" -> true
            | "false" | "f"
            | "no" | "n"
            | "0" -> false
            | _ -> failwith ("expecting Boolean argument to option " + name)
        f v
    let intOpt n f =
      if n = name then
        did := true
        let v =
          match System.Int32.TryParse value with
            | true, r -> r
            | _ -> failwith ("expecting integer argument to option " + name)
        f v
    
    boolOpt "time" (fun v -> this.time <- v)
    boolOpt "smtdump" (fun v -> this.smt_dump <- v)
    boolOpt "custominst" (fun v -> this.custom_inst <- v)
    boolOpt "boogie" (fun v -> this.dump_boogie <- v)
    stringOpt "modelfile" (fun v -> this.model_file <- v)
    intOpt "trace" (fun v -> this.trace <- v)
    
    if this.smt_dump then this.time <- true
    
    if not !did && name <> "" then
      failwith ("unknown option '" + name + "'")
