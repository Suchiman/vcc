open Microsoft.Research.Vcc.SyntaxConverter
open Microsoft.Research.Vcc.SyntaxConverter.Ast

let go filename = 
  try
    let toks = Tokenizer.fromFile filename
    let toks = Rules.apply [] toks
    let toks = PostProcessor.apply [] toks
    let outf = System.IO.File.CreateText (filename + ".out")
    let outs = (Tok.Group (fakePos, "", toks)).ToString()
    let outs = outs.Replace ("\n", "\r\n")
    outf.Write (outs)
    outf.Close()
  with SyntaxError (p, s) ->
    System.Console.WriteLine ("{0}:{1}: {2}", filename, p, s)
    
let main() =
  Rules.init()
  let args = System.Environment.GetCommandLineArgs()
  for i = 1 to args.Length - 1 do
    go args.[i]
      

main()
