namespace Microsoft.Research.Vcc.SyntaxConverter
open Microsoft.FSharp.Text
open Microsoft.Research.Vcc.SyntaxConverter.Ast

module PostProcessor =

  let apply addCompilerOptionForTestSuite toks = 
    let nl = Tok.Whitespace(fakePos, "\n") 
    let rec apply' acc = function
      | Tok.Id(p0, "_") :: Tok.Group(p1, "(", Tok.Id(p2, "ghost") :: Tok.Whitespace _ :: Tok.Id(p3, "out") :: gRest) :: rest ->
        apply' acc (Tok.Id(p0, "_") :: Tok.Group(p1, "(", Tok.Id(p3, "out") :: gRest) :: rest)
      | Tok.Comment(p, s) as tok :: rest when s.StartsWith("`") && s.[s.Length - 1] = '`' ->
        match Rules.eatWs rest with
         | [] -> apply' (tok :: acc) []
         | _ -> apply' (Comment(fakePos, "`/newsyntax") :: nl :: tok :: acc) rest
      | Tok.Group (p, s, toks) :: rest -> apply' (Tok.Group (p, s, apply' [] toks) :: acc) rest  
      | t :: rest -> apply' (t::acc) rest
      | [] -> List.rev acc
    let toks = apply' [] toks
    match toks with
      | Comment(p, s) :: rest when addCompilerOptionForTestSuite && s.StartsWith("`/") 
        -> Comment(p, s + ";/newsyntax") :: Tok.Whitespace(fakePos, "\n") :: rest
      | _ when addCompilerOptionForTestSuite -> Comment(fakePos, "`/newsyntax") :: nl :: toks
      | _ -> toks
      