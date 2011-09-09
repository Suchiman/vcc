namespace SyntaxHighlighting

open Microsoft.FSharp.Text.Lexing

module Ast =

  type Span = 
    | Spec of int * int
    | Keyword of int * int


