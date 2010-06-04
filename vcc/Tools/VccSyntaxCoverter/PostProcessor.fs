namespace Microsoft.Research.Vcc.SyntaxConverter
open Microsoft.FSharp.Text
open Microsoft.Research.Vcc.SyntaxConverter.Ast

module PostProcessor =

  let rec apply acc = function
    | Tok.Id(p0, "_") :: Tok.Group(p1, "(", Tok.Id(p2, "ghost") :: Tok.Whitespace _ :: Tok.Id(p3, "out") :: gRest) :: rest ->
      apply acc (Tok.Id(p0, "_") :: Tok.Group(p1, "(", Tok.Id(p3, "out") :: gRest) :: rest)
    | Tok.Group (p, s, toks) :: rest -> apply (Tok.Group (p, s, apply [] toks) :: acc) rest  
    | t :: rest -> apply (t::acc) rest
    | [] -> List.rev acc

      