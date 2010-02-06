namespace Microsoft.Research.Vcc.SyntaxConverter
open Microsoft.FSharp.Text
open Microsoft.Research.Vcc.SyntaxConverter.Ast

module Rules =
  let gdict() = new System.Collections.Generic.Dictionary<_,_>()
  
  type rule =
    {
      keyword : string
      replFn : list<Tok> -> list<Tok> * list<Tok>
    }
  
  let rec eatWs = function
    | Tok.Whitespace (_, _) :: rest -> eatWs rest
    | x -> x
    
  let rules = gdict()
  
  let rev_append a b =
    let rec aux acc = function
      | x :: xs -> aux (x :: acc) xs
      | [] -> acc
    aux b a
    
  let rec apply acc = function
    | Tok.Id (_, s) as t :: rest ->
      match rules.TryGetValue s with
        | true, r ->
          let (res, rest) = (r:rule).replFn (t :: rest)
          apply (rev_append res acc) rest
        | _ -> apply (t :: acc) rest
    | Tok.Group (p, s, toks) :: rest -> apply (Tok.Group (p, s, apply [] toks) :: acc) rest    
    | t :: rest -> apply (t :: acc) rest
    | [] -> List.rev acc

  let replRule kw fn =
    let repl = function
      | id :: toks ->
        fn (), toks
      | _ -> failwith ""
    { keyword = kw
      replFn = repl }
    
  let parenRuleExt kw fn =
    let repl = function
      | id :: toks ->
        match eatWs toks with
          | Tok.Group (_, "(", toks) :: rest ->
            let toks = apply [] toks
            fn (toks, rest)
          | _ -> [id], toks
      | _ -> failwith ""
    { keyword = kw
      replFn = repl }
  
  let parenRule eatSemi kw fn =
    let repl (toks, rest) =
      match eatWs rest with
        | Tok.Op (_, ";") :: rest when eatSemi -> fn toks, rest
        | _ -> fn toks, rest            
    parenRuleExt kw repl
  
  let addRule (r:rule) = rules.Add (r.keyword, r)
  let poss = function
    | [] -> fakePos
    | (x:Tok) :: _ -> x.Pos
    
  let fnApp fnName toks = 
    let p = poss toks 
    [Tok.Id (p, fnName); Tok.Group (p, "(", toks)]
    
  let spec kw toks = 
    let p = poss toks 
    fnApp "_" (Tok.Id (p, kw) :: Tok.Whitespace (p, " ") :: toks)
    
  let addStmtKwRule kw newKw =    
    addRule (parenRule true kw (fun toks -> spec newKw toks))
    
  let addKwRule kw newKw =    
    addRule (parenRule false kw (fun toks -> spec newKw toks))
  
  let addFnRule fnName newFnName =
    addRule (parenRule false fnName (fun toks -> fnApp newFnName toks))
  
  let addKwRepl oldKw newKw =
    addRule (replRule oldKw (fun () -> [Tok.Id (fakePos, newKw)]))
    
  let countSemicolons toks =
    let isSemi = function
      | Tok.Op (_, ";") -> true
      | _ -> false
    List.length (List.filter isSemi toks)
  
  let makeBlock toks =
    if countSemicolons toks > 1 then
      [Tok.Group (poss toks, "{", toks)]
    else
      toks

  let splitAt op toks =
    let rec aux acc locAcc = function
      | Tok.Op (_, n) :: rest when n = op ->
        aux (List.rev locAcc :: acc) [] rest
      | x :: rest ->
        aux acc (x :: locAcc) rest
      | [] -> List.rev (List.rev locAcc :: acc)
    aux [] [] toks
  
  let joinWith op defs =
    let rec aux acc = function
      | [x] -> List.rev (rev_append x acc)
      | x :: rest -> aux (Tok.Op (fakePos, op) :: rev_append x acc) rest
      | [] -> List.rev acc
    aux [] defs
      
  let looksLikeDecl toks =
    let isDeclTok = function
      | Tok.Id _
      | Tok.Whitespace _
      | Tok.Comment _ -> true
      | Tok.Op (_, ("*"|"^")) -> true
      | _ -> false
    List.forall isDeclTok toks
    
  let quantRule name guardOp =
    let repl (toks, rest) =
      let p = poss toks
      let body =
        if countSemicolons toks > 1 then
          let body, defs =
            match List.rev (splitAt ";" toks) with
              | body :: guard :: defs ->
                if looksLikeDecl guard then
                  body, List.rev (guard :: defs)
                else
                  guard @ [Tok.Whitespace (p, " "); Tok.Op (p, guardOp)] @ body, List.rev defs
              | _ -> failwith ""
          joinWith "," defs @ [Tok.Op (p, ";")] @ body
        else
          toks
      let body = Tok.Id (p, "\\" + name) :: Tok.Whitespace (p, " ") :: body
      // stylistic: always place () around \forall ..., or only when neccessary?
      if (eatWs rest).IsEmpty then
        body, rest
      else
        [Tok.Group (p, "(", body)], rest
    parenRuleExt name repl    
    
  let init() =
    addStmtKwRule "assert" "assert"
    addStmtKwRule "assume" "assume"
    addStmtKwRule "wrap" "pack"
    addStmtKwRule "unwrap" "unpack"
    addStmtKwRule "spec" "ghost"
    
    addKwRepl "true" "\\true"
    addKwRepl "false" "\\false"
    addKwRepl "mathint" "\\integer"
    
    addRule (parenRule false "speconly" (fun toks -> spec "ghost" (makeBlock toks)))
    addRule (quantRule "forall" "==>")
    addRule (quantRule "exists" "&&")
    
    addKwRule "expose" "expose"
    addKwRule "invariant" "invariant"
    addKwRule "ensures" "ensures"
    addKwRule "requires" "requires"
    addKwRule "reads" "reads"
    addKwRule "writes" "writes"
    
    addFnRule "mutable" "\\mutable"
    addFnRule "wrapped" "\\packed"
    addFnRule "thread_local" "\\thread_local"
    
    
