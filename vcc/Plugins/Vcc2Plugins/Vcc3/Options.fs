//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace Microsoft.Research.Vcc3

open Microsoft.Research.Vcc
open Microsoft.Research.Vcc.Util
open Microsoft.Research.Vcc3.Ast

type Options() =
  let opts = gdict()
  let lower (s:string) = s.ToLower (System.Globalization.CultureInfo.InvariantCulture)
  
  member this.GetString name defl =
    match opts.TryGetValue (lower name) with
      | true, v -> v
      | _ -> defl
  
  member this.GetInt name defl =
    match opts.TryGetValue (lower name) with
      | true, v ->
        match System.Int32.TryParse v with
          | true, r -> r
          | _ -> failwith ("expecting integer argument to option " + name)
      | _ -> defl
 
  member this.GetBool name defl =
    match opts.TryGetValue (lower name) with
      | true, v ->
        match lower v with
          | "true" | "t"
          | "yes" | "y"
          | "1" -> true
          | "false" | "f"
          | "no" | "n"
          | "0" -> false
          | _ -> failwith ("expecting Boolean argument to option " + name)
      | _ -> defl

  member this.Set (opt:string) =
    let idx = opt.IndexOf '='
    if idx >= 0 then
      let name = opt.Substring (0, idx)
      let value = opt.Substring (idx + 1)
      opts.[lower name] <- value
    else
      opts.[lower opt] <- "true"