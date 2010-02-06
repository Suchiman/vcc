namespace Microsoft.Research.Vcc.SyntaxConverter
open Microsoft.FSharp.Text

module Ast =
  type P = PreToken.PreTok
  
  type Pos = 
    { 
      line : int; 
      column : int 
    }
    
    override this.ToString() =
      (this.line+1).ToString() + ":" + (this.column+1).ToString()
  
  let fakePos = { line = 0; column = 0 }
  
  exception SyntaxError of Pos * string
  
  let closing = function
    | "(" -> ")"
    | "[" -> "]"
    | "{" -> "}"
    | "" -> ""
    | _ -> failwith ""

  type Tok =
    | Id of Pos * string
    | Literal of Pos * string
    | Op of Pos * string
    | Comment of Pos * string
    | Directive of Pos * string
    | Whitespace of Pos * string
    | Group of Pos * string * list<Tok>
    
    override this.ToString() =
      let sb = System.Text.StringBuilder()
      let wr (s:string) = sb.Append s |> ignore
      let rec pr = function
        | Id (_, s)
        | Literal (_, s)
        | Op (_, s)
        | Comment (_, s)
        | Directive (_, s)
        | Whitespace (_, s) -> wr s
        | Group (_, c, toks) ->
          wr c
          List.iter pr toks
          wr (closing c)
      pr this
      sb.ToString()
        
    member this.Pos =
      match this with
        | Id (p, _)
        | Literal (p, _)
        | Op (p, _)
        | Comment (p, _)
        | Directive (p, _)
        | Whitespace (p, _)
        | Group (p, _, _) -> p

