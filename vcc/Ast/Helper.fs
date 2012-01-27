//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.Util

  module Helper =

    [<AbstractClass>]
    type public Env() =

      let currentId = ref 0

      abstract PointerSizeInBytes : int with get

      abstract member Oops : Token * string -> unit

      abstract member Die : unit -> 'a

      abstract member Die : Token -> 'a

      abstract member Error : Token * int * string * Token option -> unit

      // 9100 <= code <= 9199; First available: 9127
      abstract member Warning : Token * int * string * Token option -> unit

      // 9601 <= code <= 9799; First available: 9746
      member this.Error(token, code, msg) = this.Error(token, code, msg, None)

      // 9300 <= code <= 9399; First available: 9325
      member this.GraveWarning(token, code, msg) = this.GraveWarning(token, code, msg, None)

      member this.GraveWarning(token, code, msg, related) = this.Warning(token, code, "[possible unsoundness]: " + msg, related)

      member this.Warning(token, code, msg) = this.Warning(token, code, msg, None)

      member this.Panic (msg:string) =
        this.Oops (Token.NoToken, msg)
        this.Die ()

      member this.UniqueId () =
        incr currentId
        !currentId
