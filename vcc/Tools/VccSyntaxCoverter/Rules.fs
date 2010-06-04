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

  let eatWsEx l = 
    let rec aux acc = function
      | Tok.Whitespace (_, _) as w :: rest -> aux (w :: acc) rest
      | x -> (List.rev acc, x)
    aux [] l
    
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
  
  let rec getTriggers acc = function
    | Tok.Op (_, ";") as h :: rest ->
      match eatWs rest with
        | (Tok.Group (_, "{", _) :: _) as lst ->
          let rec aux acc l = 
            match eatWsEx l with
              | ws, (Tok.Group (_, "{", _) as t) :: rest ->
                aux (acc @ ws @ [t]) rest
              | _ -> (acc, l)
          let (trig, lst) = aux [] lst
          Some (List.rev acc, trig, lst)
        | _ -> getTriggers (h :: acc) rest
    | h :: rest -> getTriggers (h :: acc) rest
    | [] -> None

  let quantRule name guardOp =
    let repl (toks, rest) =
      let p = poss toks
      let body =
        match getTriggers [] toks with
          | Some (bindings, triggers, body) ->
            bindings @ [Tok.Op (p, ";")] @ [Tok.Whitespace (p, " ")] @ triggers  @ body
          | None ->
            if countSemicolons toks > 1 then
              let body, defs =
                match List.rev (splitAt ";" toks) with
                  | body :: guard :: defs ->
                    if looksLikeDecl guard then
                      body, List.rev (guard :: defs)
                    else
                      guard @ [Tok.Whitespace (p, " "); Tok.Op (p, guardOp)] @ body, List.rev defs
                  | _ -> failwith ""
              joinWith ";" defs @ [Tok.Op (p, ";")] @ body
            else
              toks
      let body = Tok.Id (p, "\\" + name) :: Tok.Whitespace (p, " ") :: body
      // stylistic: always place () around \forall ..., or only when neccessary?
      if (eatWs rest).IsEmpty then
        [paren "" body], rest
      else
        [paren "(" body], rest
    parenRuleExt name repl    
    
  let addInfixRule name op =
    let repl = function 
      | [e1;e2] -> e1 @ [Tok.Op(fakePos, " " + op + " ")] @ eatWs e2
      | _ -> failwith ""
    addRule (parenRuleN name 2 repl)

  let init() =
    addStmtKwRule "assert" "assert"
    addStmtKwRule "bv_lemma" "assert {:bv}" // this is stretching it
    addStmtKwRule "assume" "assume"
    addStmtKwRule "wrap" "wrap"
    addStmtKwRule "unwrap" "unwrap"
    addStmtKwRule "spec" "ghost"
    addStmtKwRule "axiom" "axiom"
    
    addKwRepl "mathint" "\\integer"
    addKwRepl "obj_t" "\\object"
    addKwRepl "claim_t" "\\claim"
    addKwRepl "thread_id" "\\thread"
    addKwRepl "result" "\\result"
    addKwRepl "this" "\\this"
    addKwRepl "ispure" "_(pure)"
    addKwRepl "true" "\\true"
    addKwRepl "false" "\\false"
    addKwRepl "spec_malloc" "\\alloc"             // cannot use fnRule because of template paramters
    addKwRepl "spec_alloc_array" "\\alloc_array"  // cannot use fnRule because of template paramters
    
    addRule (parenRule false "speconly" (fun toks -> spec "ghost" (makeBlock toks)))
    addRule (parenRule false "sk_hack" (fun toks -> [Tok.Id (fakePos, "hint: "); paren "" toks]))
    addRule (quantRule "forall" "==>")
    addRule (quantRule "exists" "&&")
    addRule (quantRule "lambda" "###")
    
    let canonicalKeywords = [
                              "atomic";
                              "invariant";
                              "decreases";
                              "ensures";
                              "requires";
                              "reads";
                              "writes";
                              "always";
                              "maintains";
                              "returns";
                            ]

    for cw in canonicalKeywords do addKwRule cw cw

    addKwRule "expose" "unwrapping"

    let canonicalFn = [
                        "wrapped";
                        "wrapped0";
                        "thread_local";
                        "span";
                        "extent";
                        "inv";
                        "inv2";
                        "old";
                        "match_long";
                        "match_ulong";                  
                        "array_range";      
                      ]

    for cfn in canonicalFn do addFnRule cfn ("\\" + cfn)

    addFnRule "valid_claim" "\\active_claim"
    addFnRule "ref_cnt" "\\claim_count"
    addFnRule "is_claimable" "\\claimable"
    addFnRule "is_fresh" "\\fresh"
    addFnRule "mutable" "\\unwrapped"
    addFnRule "closed" "\\consistent"
    addFnRule "typed" "\\valid"
    addFnRule "is_thread_local_array" "\\thread_local_array"
    addFnRule "is_mutable_array" "\\mutable_array"
    addFnRule "unchecked" "_(unchecked)"
    addFnRule "keeps" "\\mine"
    addFnRule "set_universe" "\\universe"
    addFnRule "claims_obj" "\\claims_object"

    addRule (parenRule false "SET" (fun toks -> [paren "{" toks]))
    addRule (parenRule false "claimp" (fun toks -> spec "ghost" (Tok.Id (fakePos, "\claim ") :: toks)))
    
    addRule (parenRuleN "me" 0 (fun _ -> [Tok.Id (fakePos, "\\me")]))
  
    addRule (parenRule false "vcc" (fnApp "_"))
    addInfixRule "set_union" "\\union"
    addInfixRule "set_difference" "\\diff"
    addInfixRule "set_intersection" "\\inter"
    addInfixRule "set_in" "\\in"
    addInfixRule "is" "\\is"


    let as_array = function
      | [arr; sz] ->
        let arr =
          match arr with
            | [Tok.Id _] -> arr
            | _ -> [paren "(" arr]
        [paren "(" [Tok.Id (fakePos, "\\any"); paren "[" sz]] @ arr
      | _ -> failwith ""
    addRule (parenRuleN "as_array" 2 as_array)
        
    let owns = function
      | [e] -> e :: [Tok.Op(fakePos, "->"); Tok.Id(fakePos, "\\owns")]
      | _ -> failwith ""
    addRule (parenRule false "owns" owns)

    let owner = function
      | [e] -> e :: [Tok.Op(fakePos, "->"); Tok.Id(fakePos, "\\owner")]
      | _ -> failwith ""
    addRule (parenRule false "owner" owns)
    
    let set_owns = function
      | [e; s] ->
        e @ [Tok.Op(fakePos, "->"); Tok.Id(fakePos, "\owns"); Tok.Op(fakePos, " = ")] @ s
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
    
    let reify toks = 
      match List.rev (splitAt "," toks) with 
        | prop :: objs ->
          fnApp "\\make_claim" (paren "{" (joinWith "," (List.rev objs)) :: Tok.Op (fakePos, ",") :: prop)
        | _ -> failwith ""
    addRule (parenRule false "claim" reify)
            
    let unclaim toks = 
      match splitAt "," toks with 
        | cl :: objs ->
          spec "ghost" (fnApp "\\destroy_claim" (cl @ [Tok.Op (fakePos, ", "); paren "{" (joinWith "," objs)]))
        | _ -> failwith ""
    addRule (parenRule false "unclaim" unclaim)
            
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
