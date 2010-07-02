namespace Microsoft.Research.Vcc.SyntaxConverter
open Microsoft.FSharp.Text
open Microsoft.Research.Vcc.SyntaxConverter.Ast

module Rules =
  let gdict() = new System.Collections.Generic.Dictionary<_,_>()
  
  type ctx = 
    {
      in_ensures : bool
    }

  type rule =
    {
      keyword : string
      replFn : ctx -> list<Tok> -> ctx * list<Tok> * list<Tok>

    }

  let ctxFreeRule rule ctx toks = let (l1, l2) = rule toks in ctx, l1, l2
    
  let space = Tok.Whitespace(fakePos, " ")

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
    
  let rec apply' ctx acc = function
    | Tok.Id (_, s) as t :: rest ->
      match rules.TryGetValue s with
        | true, r ->
          let ctx', res, rest = (r:rule).replFn ctx (t :: rest)
          apply' ctx' (rev_append res acc) rest
        | _ -> apply' ctx (t :: acc) rest
    | Tok.Group (p, s, toks) :: rest -> apply' ctx (Tok.Group (p, s, apply' ctx [] toks) :: acc) rest    
    | t :: rest -> apply' ctx (t :: acc) rest
    | [] -> List.rev acc


  let replRule kw fn =
    let repl = function
      | id :: toks ->
        fn (), toks
      | _ -> failwith ""
    { keyword = kw
      replFn = ctxFreeRule repl }
    
  let parenRuleExt kw fn =
    let repl ctx = function
      | id :: toks ->
        match eatWs toks with
          | Tok.Group (_, "(", toks) :: rest ->
            let toks = apply' ctx [] toks
            let l1, l2 = fn (toks, rest) in ctx, l1, l2
          | _ -> ctx, [id], toks
      | _ -> failwith ""
    { keyword = kw
      replFn = repl }

  let parenRuleExtCtx kw fn fnCtx =
    let repl ctx = function
      | id :: toks ->
        match eatWs toks with
          | Tok.Group (_, "(", toks) :: rest ->
            let toks = apply' (fnCtx ctx) [] toks
            let l1, l2 = fn (toks, rest) in ctx, l1, l2
          | _ -> ctx, [id], toks
      | _ -> failwith ""
    { keyword = kw
      replFn = repl }
  
  let parenRuleCtx eatSemi kw fn =
    let repl (toks, rest) =
      match eatWs rest with
        | Tok.Op (_, ";") :: rest when eatSemi -> fn toks, rest
        | _ -> fn toks, rest            
    parenRuleExtCtx kw repl
  
  let parenRule eatSemi kw fn = parenRuleCtx eatSemi kw fn (fun x -> x)

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
    let repl ctx = function
      | id :: toks ->
        match eatWs toks with
          | Tok.Group (_, "(", toks) :: rest when (splitAt "," (eatWs toks)).Length = n ->
            let args = splitAt "," (apply' ctx [] toks)
            ctx, fn args, rest
          | _ -> ctx, [id], toks
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
    
  let parenOpt toks =
    match trim toks with
      | [] -> Tok.Whitespace(fakePos, "")
      | [tok] -> tok
      | toks -> paren "(" toks

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
    let canonicalKw = [
                        "atomic"
                        "decreases"
                        "requires"
                        "reads"
                        "writes"
                        "always"
                        "maintains"
                        "returns"
                      ]

    let canonicalFn = [
                        "wrapped"
                        "wrapped0"
                        "thread_local"
                        "span"
                        "extent"
                        "inv"
                        "inv2"
                        "old"
                        "match_long"
                        "match_ulong"
                        "array_range"
                        "array_members"
                        "start_here"
                        "depends"
                        "not_shared"
                        "claims"
                        "unchanged"
                        "claims_claim"
                        "when_claimed"
                        "account_claim"
                        "extent_zero"
                        "full_extent"
                        "mutable"
                        "extent_mutable"
                        "approves"
                        "union_active"
                      ]

    let canonicalSm = [
                        "assert"
                        "assume"
                        "wrap"
                        "unwrap"
                        "deep_unwrap"
                        "axiom"
                        "reads_havoc"
                        "set_closed_owns"
                        "union_reinterpret"
                        "bump_volatile_version"
                        "begin_update"
                        "join_arrays"
                        "split_array"
                        "from_bytes"
                        "to_bytes"
                      ]

    for cw in canonicalKw do addKwRule cw cw
    for cf in canonicalFn do addFnRule cf ("\\" + cf)
    for cs in canonicalSm do addStmtKwRule cs cs

    addStmtKwRule "bv_lemma" "assert {:bv}" // this is stretching it
    addStmtKwRule "spec" "ghost"
    
    addKwRepl "mathint" "\\integer"
    addKwRepl "state_t" "\\state"
    addKwRepl "obj_t" "\\object"
    addKwRepl "ptrset" "\\objset"
    addKwRepl "claim_t" "\\claim"
    addKwRepl "thread_id" "\\thread"
    addKwRepl "this" "\\this"
    addKwRepl "ispure" "_(pure)"
    addKwRepl "backing_member" "_(backing_member)"
    addKwRepl "true" "\\true"
    addKwRepl "false" "\\false"
    addKwRepl "spec_malloc" "\\alloc"             // cannot use fnRule because of template paramters
    addKwRepl "spec_alloc_array" "\\alloc_array"  // cannot use fnRule because of template paramters
    
    addRule (parenRule false "speconly" (fun toks -> spec "ghost" (makeBlock toks)))
    addRule (parenRule false "sk_hack" (fun toks -> [Tok.Id (fakePos, "hint: "); paren "" toks]))
    addRule (quantRule "forall" "==>")
    addRule (quantRule "exists" "&&")
    addRule (quantRule "lambda" ";")
    
    addKwRule "expose" "unwrapping"
    addKwRule "out_param" "writes"
    addKwRule "weak_out_param" "writes"

    addFnRule "valid_claim" "\\active_claim"
    addFnRule "is_claimable" "\\claimable"
    addFnRule "is_fresh" "\\fresh"
    addFnRule "is_thread_local_array" "\\thread_local_array"
    addFnRule "is_mutable_array" "\\mutable_array"
    addFnRule "unchecked" "_(unchecked)"
    addFnRule "keeps" "\\mine"
    addFnRule "set_universe" "\\universe"
    addFnRule "claims_obj" "\\claims_object"
    addFnRule "extent_is_fresh" "\\extent_fresh"
    addFnRule "is_malloc_root" "\\malloc_root"
    addFnRule "is_object_root" "\\object_root"
    addFnRule "in_state" "\\at"
    addFnRule "current_state" "\\now"

    addRule (parenRule false "SET" (fun toks -> [paren "{" toks]))
    addRule (parenRule false "set_singleton" (fun toks -> [paren "{" toks]))
    addRule (parenRule false "set_empty" (fun toks -> [paren "{" []]))
    addRule (parenRule false "claimp" (fun toks -> spec "ghost" (Tok.Id (fakePos, "\claim ") :: toks)))
    
    addRule (parenRuleN "me" 0 (fun _ -> [Tok.Id (fakePos, "\\me")]))
  
    let typed_phys_or_spec isSpec toks = 
      let optNot = if isSpec then [] else [Tok.Op(fakePos, "!")]
      [paren "(" ([parenOpt toks; Tok.Op(fakePos, "->"); Tok.Id(fakePos, "\\valid"); space; Tok.Op(fakePos, "&&"); space] @ optNot @ (fnApp "\\ghost" toks))]

    addRule (parenRule false "typed_phys" (typed_phys_or_spec false))
    addRule (parenRule false "typed_spec" (typed_phys_or_spec true))

    addRule (parenRule false "vcc" (fnApp "_"))
    addInfixRule "set_union" "\\union"
    addInfixRule "set_difference" "\\diff"
    addInfixRule "set_intersection" "\\inter"
    addInfixRule "set_in" "\\in"
    addInfixRule "is" "\\is"

    let addGhostFieldRule fn fld = 
      let ghostFieldRule fieldName = function
        | [e] -> e @ [Tok.Op(fakePos, "->"); Tok.Id(fakePos, fieldName)]
        | _ -> failwith ""
      addRule (parenRuleN fn 1 (ghostFieldRule fld))

    addGhostFieldRule "owns"    "\\owns"
    addGhostFieldRule "owner"   "\\owner"
    addGhostFieldRule "typed"   "\\valid"
    addGhostFieldRule "closed"  "\\consistent"
    addGhostFieldRule "ref_cnt" "\\claim_count"

    let result_rule ctx = function
      | (id:Tok) :: rest when ctx.in_ensures -> ctx, [Tok.Id(id.Pos, "\\result")], rest
      | id :: rest -> ctx, [id], rest
      | _ -> failwith ""

    addRule { keyword = "result"; replFn = result_rule }
    addRule (parenRuleCtx true "ensures" (fun toks -> spec "ensures" toks) (fun (c:ctx) -> { c with in_ensures = true}) )

    
    let as_array = function
      | [arr; sz] ->
        let arr =
          match arr with
            | [Tok.Id _] -> arr
            | _ -> [paren "(" arr]
        [paren "(" [Tok.Id (fakePos, "void"); paren "[" sz]] @ arr
      | _ -> failwith ""
    addRule (parenRuleN "as_array" 2 as_array)
        
    let set_owns = function
      | [e; s] ->
        spec "ghost" (e @ [Tok.Op(fakePos, "->"); Tok.Id(fakePos, "\owns"); Tok.Op(fakePos, " = ")] @ s)
      | _ -> failwith ""
    addRule (parenRuleN "set_owns" 2 set_owns)
    
    let closed_owner_rule op = function
      | [ob; owner] ->
        let owns = fnApp "\\owns" owner
        spec "ghost" (owner @ [Tok.Op(fakePos, "->"); Tok.Id(fakePos, "\\owns"); Tok.Op(fakePos, " " + op + " ") ] @ (eatWs ob))
      | _ -> failwith ""

    addRule (parenRuleN "set_closed_owner" 2 (closed_owner_rule "+="))
    addRule (parenRuleN "giveup_closed_owner" 2 (closed_owner_rule "-="))

    let in_domain = function
     | [e1; e2] ->
       e1 @ [ Tok.Op(fakePos, " \\in ") ] @ fnApp "\\domain" (eatWs e2)
     | _ -> failwith ""
    addRule (parenRuleN "in_domain" 2 in_domain)
    
    let reify fn toks = 
      match List.rev (splitAt "," toks) with 
        | prop :: objs ->
          fnApp fn (paren "{" (joinWith "," (List.rev objs)) :: Tok.Op (fakePos, ",") :: prop)
        | _ -> failwith ""

    addRule (parenRule false "claim" (reify "\\make_claim"))
    addRule (parenRule false "upgrade_claim" (reify "\\upgrade_claim"))
            
    let unclaim toks = 
      match splitAt "," toks with 
        | cl :: objs ->
          spec "ghost" (fnApp "\\destroy_claim" (cl @ [Tok.Op (fakePos, ", "); paren "{" (joinWith "," objs)]))
        | _ -> failwith ""
    addRule (parenRule false "unclaim" unclaim)
            
    let def_group toks = 
      match splitAt "," toks with
        | [groupName] -> spec "group" groupName
        | groupName :: groupSpecs -> spec "group" ((joinWith " " groupSpecs) @ (space ::  groupName))
        | _ -> failwith ""
    addRule (parenRule false "def_group" def_group)

    let in_group = function
      | [toks] -> fnApp "_" (Tok.Op(fakePos, ":") :: toks)
      | _ -> failwith ""    
    addRule (parenRuleN "in_group" 1 in_group)

    let inv_group = function
      | [groupName; invariant] -> spec "invariant" (Tok.Op(fakePos, ":") :: groupName @ (space :: eatWs invariant))
      | _ -> failwith ""
    addRule (parenRuleN "inv_group" 2 inv_group)

    let invariant toks = 
      match eatWs toks with
        | (Tok.Id(_, lbl) as id) :: Tok.Op(_, ":") :: invariant ->  spec "invariant" (Tok.Op(fakePos, ":") :: id :: (space :: eatWs invariant))
        | invariant -> spec "invariant" (eatWs invariant)
    addRule (parenRule true "invariant" invariant)

    let struct_rule = function
      | hd :: rest ->
        match eatWs rest with
          | Tok.Id (_, "vcc") :: args ->
            match eatWs args with
              | Tok.Group (_, "(", toks) :: rest ->
                fnApp "_" toks @ [space], hd :: rest
              | _ -> [hd], rest
          | _ -> [hd], rest
      | _ -> failwith ""
    
    addRule { keyword = "struct"; replFn = ctxFreeRule struct_rule }
    addRule { keyword = "union"; replFn = ctxFreeRule struct_rule }

  let apply = apply' { in_ensures = false }  []
