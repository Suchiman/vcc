module Microsoft.Research.Vcc.TokenCounter.Driver

open Microsoft.Research.Vcc.TokenCounter.Token
open Microsoft.Research.Vcc.TokenCounter.Counter

type Tok0 = Token.Tok

let err pos s = raise (SyntaxError (pos, s))

let dbg = ref false
let needHeader = ref true
let total = ref (Count.Zero())
let toks = ref false


let tokenize lexbuf =
  let rec getToks acc = 
    let tok0 = Lexer.token lexbuf
    let pos = { line = lexbuf.StartPos.Line; column = lexbuf.StartPos.Column }
    match tok0 with
      | Tok0.Invalid s ->
        err pos ("invalid character in input '" + s + "'")
      | Tok0.Eof -> List.rev acc, pos.line
      | _ -> getToks (tok0 :: acc)
  getToks []
   
let lexFile filename = 
  let path = System.IO.Path.GetDirectoryName filename
  tokenize (Lexing.LexBuffer<char>.FromTextReader (System.IO.File.OpenText filename))    

let dump fn (c:Count) =
  let perc a b =
    if b = 0 then 999
    else a * 100 / b
  if !toks then
    if !needHeader then
      printf "  SPEC /   PHYS =  OVERHEAD   FILENAME\n"
      needHeader := false
    printf "%6d / %6d = %8d%%   %s\r\n" c.spec.tokens c.phys.tokens (perc c.spec.tokens c.phys.tokens) fn
  else
    if !needHeader then
      printf "  SPEC /   PHYS =  OVERHEAD  +BLANK  FILENAME\n"
      needHeader := false
    let blanks = c.real_lines - c.spec.lines - c.phys.lines
    printf "%6d / %6d = %8d%%  +%-6d %s\r\n" c.spec.lines c.phys.lines (perc c.spec.lines c.phys.lines) blanks fn

let go filename = 
  try
    if not (System.IO.File.Exists filename) then
      raise (SyntaxError (fakePos, "file does not exists"))
    let toks, lines = lexFile filename
    let count, specStr, physStr = Counter.compute toks
    let count = { count with real_lines = lines }
    if !dbg then
      System.IO.File.WriteAllText (filename + "-spec", specStr)
      System.IO.File.WriteAllText (filename + "-phys", physStr)
    dump filename count
    total := (!total).Add count
  with SyntaxError (p, s) ->
    System.Console.WriteLine ("{0}:{1}: {2}", filename, p, s)
    
let main() =
  let args = System.Environment.GetCommandLineArgs()
  let mutable fileCount = 0
  for i = 1 to args.Length - 1 do
    match args.[i] with
      | "-d" -> dbg := true
      | "-t" -> toks := true
      | x -> 
        fileCount <- fileCount + 1
        go x

  if fileCount = 0 then
    printf "Usage: %s [-d] [-t] files.c....\r\n" args.[0]
  elif fileCount > 1 then
    dump "TOTAL" !total

main()