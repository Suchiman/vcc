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
  
  let splitAt op toks =
    let rec aux acc locAcc = function
      | Tok.Op (_, n) :: rest when n = op ->
        aux (List.rev locAcc :: acc) [] rest
      | x :: rest ->
        aux acc (x :: locAcc) rest
      | [] ->
        if locAcc.IsEmpty then List.rev acc
        else List.rev (List.rev locAcc :: acc)
    aux [] [] toks
  
  let parenRuleN kw n fn =
    let repl = function
      | id :: toks ->
        match eatWs toks with
          | Tok.Group (_, "(", toks) :: rest when (splitAt "," (eatWs toks)).Length = n ->
            let args = splitAt "," (apply [] toks)
            fn args, rest
          | _ -> [id], toks
      | _ -> failwith ""
    { keyword = kw
      replFn = repl }
    
  let addRule (r:rule) = rules.Add (r.keyword, r)
  let poss = function
    | [] -> fakePos
    | (x:Tok) :: _ -> x.Pos
  
  let trim toks =
    toks |> List.rev |> eatWs |> List.rev |> eatWs
    
  let paren p toks =
    Tok.Group (fakePos, p, trim toks)
    
  let fnApp fnName toks = 
    let p = poss toks 
    [Tok.Id (p, fnName); paren "(" toks]
    
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
        [paren "" body], rest
      else
        [paren "(" body], rest
    parenRuleExt name repl    
    
  let init() =
    addStmtKwRule "assert" "assert"
    addStmtKwRule "bv_lemma" "assert {bv}" // this is stretching it
    addStmtKwRule "assume" "assume"
    addStmtKwRule "wrap" "wrap"
    addStmtKwRule "unwrap" "unwrap"
    addStmtKwRule "spec" "ghost"
    
    //addKwRepl "true" "\\true"
    //addKwRepl "false" "\\false"
    addKwRepl "mathint" "\\integer"
    addKwRepl "obj_t" "\\object"
    addKwRepl "threadid_t" "\\thread"
    addKwRepl "result" "\\result"
    addKwRepl "this" "\\this"
    
    addRule (parenRule false "speconly" (fun toks -> spec "ghost" (makeBlock toks)))
    addRule (parenRule false "sk_hack" (fun toks -> [Tok.Id (fakePos, "hint: "); paren "" toks]))
    addRule (quantRule "forall" "==>")
    addRule (quantRule "exists" "&&")
    addRule (quantRule "lambda" "###")
    
    addKwRule "expose" "unwrapping"
    addKwRule "atomic" "atomic"
    addKwRule "invariant" "invariant"
    addKwRule "ensures" "ensures"
    addKwRule "requires" "requires"
    addKwRule "reads" "reads"
    addKwRule "writes" "writes"
    
    addFnRule "is_fresh" "\\fresh"
    addFnRule "mutable" "\\unwrapped"
    addFnRule "wrapped" "\\wrapped"
    addFnRule "closed" "\\consistent"
    addFnRule "typed" "\\valid"
    addFnRule "owner" "\\owner"
    addFnRule "thread_local" "\\thread_local"
    addFnRule "span" "\\span"
    addFnRule "unchecked" "_(unchecked)"
    addFnRule "inv" "\\inv"
    addFnRule "inv2" "\\inv2"
    addFnRule "keeps" "\\mine"
    addFnRule "owns" "\\owns"
    addFnRule "old" "\\old"
    addFnRule "set_union" "\\union"
    addFnRule "set_difference" "\\diff"
    addFnRule "set_intersection" "\\inter"
    addFnRule "claims_obj" "\\claims_obj"
    addRule (parenRule false "SET" (fun toks -> [paren "{" toks]))
    addRule (parenRule false "claimp" (fun toks -> spec "ghost" (Tok.Id (fakePos, "\claim ") :: toks)))
    
    addRule (parenRuleN "me" 0 (fun _ -> [Tok.Id (fakePos, "\\me")]))
    
    let as_array = function
      | [arr; sz] ->
        let arr =
          match arr with
            | [Tok.Id _] -> arr
            | _ -> [paren "(" arr]
        [paren "(" [Tok.Id (fakePos, "\\any"); paren "[" sz]] @ arr
      | _ -> failwith ""
    addRule (parenRuleN "as_array" 2 as_array)
        
    let set_in = function
      | [e; s] ->
        e @ [Tok.Id (fakePos, " \\in ")] @ eatWs s
      | _ -> failwith ""
    addRule (parenRuleN "set_in" 2 set_in)
    
    let set_owns = function
      | [e; s] ->
        fnApp "\\set" (fnApp "\\owns" e @ [Tok.Op (fakePos, ",")] @ s)
      | _ -> failwith ""
    addRule (parenRuleN "set_owns" 2 set_owns)
    
    let set_closed_owner = function
      | [ob; owner] ->
        let owns = fnApp "\\owns" owner
        //fnApp "\\set" (owns @ [Tok.Op (fakePos, ",")] @ (fnApp "\\union" (owns @ [Tok.Op(fakePos, ","); paren "{" ob])))
        fnApp "\\union_with" (owns @ [Tok.Op (fakePos, ",")] @ [paren "{" ob])
      | _ -> failwith ""
    addRule (parenRuleN "set_closed_owner" 2 set_closed_owner)
        
    let giveup_closed_owner = function
      | [ob; owner] ->
        let owns = fnApp "\\owns" owner
        //fnApp "\\set" (owns @ [Tok.Op (fakePos, ",")] @ (fnApp "\\union" (owns @ [Tok.Op(fakePos, ","); paren "{" ob])))
        fnApp "\\diff_with" (owns @ [Tok.Op (fakePos, ",")] @ [paren "{" ob])
      | _ -> failwith ""
    addRule (parenRuleN "giveup_closed_owner" 2 giveup_closed_owner)
            
    let struct_rule = function
      | hd :: rest ->
        match eatWs rest with
          | Tok.Id (_, "vcc") :: args ->
            match eatWs args with
              | Tok.Group (_, "(", toks) :: rest ->
                fnApp "_" toks @ [Tok.Whitespace (fakePos, " ")], hd :: rest
              | _ -> [hd], rest
          | _ -> [hd], rest
      | _ -> failwith ""
    
    addRule { keyword = "struct"; replFn = struct_rule }
    addRule { keyword = "union"; replFn = struct_rule }
    
      
