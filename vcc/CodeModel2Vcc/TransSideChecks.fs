//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light


namespace Microsoft.Research.Vcc
 open Microsoft.Research.Vcc
 open Microsoft.Research.Vcc.Util
 open Microsoft.Research.Vcc.TransUtil
 open Microsoft.Research.Vcc.CAST
 
 module TransSideChecks =
  
 
  // ============================================================================================================
  
  let prestate = Macro ({ bogusEC with Type = Type.MathState }, "prestate", [])
  
  let admChecks helper p =
    AddChecks.invariantCheck helper (fun _ -> true) 8012 "is not admissible" prestate (mkRef p)
  
  let unwrapChecks helper p =
    AddChecks.invariantCheck helper (fun e -> not (AddChecks.isOnUnwrap e)) 8018 "forbids unwrapping" prestate (mkRef p)

  let stuttChecks helper p =    
    let stuttetringCheck (inv:Expr) =
      let seenOld = ref false
      let replaceThisOld self = function 
        | Expr.Macro(c, "this", []) -> Some (mkRef p)
        | Expr.Old (_, Macro (_, "prestate", []), e) -> 
          seenOld := true
          Some (self e)
        | _ -> None
      let inv = inv.SelfMap replaceThisOld
      if !seenOld then
        [Expr.Macro(afmte 8013 "invariant({0}) is not admissible (stuttering)" [inv], "token_holder", [inv])]
            else []
    let td = match p.Type with Ptr (Type.Ref td) -> td | _ -> die()
    List.map stuttetringCheck (AddChecks.invariantsOf td) |> List.concat

  let addDefaultAdmissibilityChecks (explicitAdm:Dict<_,_>) helper =
    let handleDecl = function
      | Top.TypeDecl td as decl when not (_list_mem NoAdmissibility td.CustomAttr) ->
        let rec isTrivialInvariant = function
          | CallMacro(_, "_vcc_typed2", _, [_;  This]) -> true
          | CallMacro(_, "_vcc_set_eq", _, [CallMacro(_, "_vcc_owns", _, [_; This]); CallMacro(_, "_vcc_set_empty", _, [])]) -> true
          | CallMacro(_, "_vcc_inv_is_owner_approved", _, _) -> true
          | CallMacro(_, "labeled_invariant", _, [_; inv]) -> isTrivialInvariant inv
          | _ -> false
        
        if List.forall isTrivialInvariant (td.Invariants) then [decl]
        else              
          let parm =
            { Type = Ptr (Type.Ref td)
              Name = "_this_"
              Kind = Parameter } : Variable
          
          let (explicitAdm, explicitUnwrap, explicitStutt) =
            match explicitAdm.TryGetValue td with
              | true, r -> r
              | _ -> (false, false, false)
          let ec = { bogusEC with Token = td.Token; Type = Type.Void }
          let isStutt = Macro (boolBogusEC(), "_vcc_is_stuttering_check", [])
          let isUnwrap = Macro (boolBogusEC(), "_vcc_is_unwrap_check", [])
          let isAdm = Macro (boolBogusEC(), "_vcc_is_admissibility_check", [])
          let assume name = Expr.MkAssume (Macro (boolBogusEC(), name, [mkRef parm]))
          let unwrapBody = Expr.MkBlock [Expr.MkAssume (mkNot isStutt);
                                         assume "_vcc_unwrap_check_pre";
                                         Macro (ec, "_vcc_unwrap_check", [mkRef parm])]
          let admBody    = Expr.MkBlock ( if explicitAdm then 
                                            [ assume "_vcc_stuttering_pre" ]
                                          else 
                                            [ If (ec, isAdm, assume "_vcc_admissibility_pre", assume "_vcc_stuttering_pre");
                                            Macro (ec, "_vcc_havoc_others", [mkRef parm; typeExpr (Type.Ref td)]);
                                            Expr.MkAssume (mkImpl isStutt (Macro (boolBogusEC(), "_vcc_inv2", [mkRef parm]))) ])
          let assumeNot cond name body =
            if cond then Expr.MkBlock [Expr.MkAssume (mkNot name); body]
            else body
            
          let body = 
            if explicitUnwrap then admBody
            else If (ec, isUnwrap, unwrapBody, admBody)
          let body = 
              body |>
                assumeNot explicitAdm isAdm |>
                assumeNot explicitStutt isStutt |>
                assumeNot explicitUnwrap isUnwrap
          let post = 
            List.map (mkImpl isAdm) (admChecks helper parm) @
            List.map (mkImpl isStutt) (stuttChecks helper parm) @
            List.map (mkImpl isUnwrap) (unwrapChecks helper parm)
                    
              //Expr.Assert( { afmte 8514 "invariant({0}) holds (in admissibility check)" [expr] with Type = Type.Void }, expr )
          let check = 
            { Token = td.Token
              IsSpec = true  
              RetType = Void
              OrigRetType = Void
              Name = td.Name + "#adm"
              Parameters = [parm]
              TypeParameters = []
              Requires = []
              Ensures = post
              Writes = []
              Reads = []
              CustomAttr = [ IsAdmissibilityCheck ]
              Body = Some body
              IsProcessed = true } : Function
          [decl; Top.FunctionDecl check]
      | decl -> [decl]
    List.map handleDecl >> List.concat
 
  let handleCustomAdmissibilityChecks (explicitAdm:Dict<_,_>) (helper:Helper.Env) decls =
    let errCheck (f:Function) cb =
      f.CustomAttr <- []
      match f.Parameters with
        | [{ Type = Ptr (Type.Ref td) } as p] ->
          if f.Requires <> [] || f.Ensures <> [] || f.Writes <> [] then
            helper.Error (f.Token, 9624, "custom admissibility checks are not allowed to have explicit requires/ensures/writes", None)
          if f.Body.IsNone then
            helper.Error (f.Token, 9644, "the admissibility check is required to have a body", None)
          
          cb td p
              
        | _ -> helper.Error (f.Token, 9622, "the admissibility check function expects a single pointer parameter", None)
    
    for d in decls do    
      match d with
        | Top.FunctionDecl f when _list_mem IsAdmissibilityCheck f.CustomAttr ->
          errCheck f (fun td p ->
            match explicitAdm.TryGetValue td with
              | true, (_, u, s) -> explicitAdm.[td] <- (true, u, s)
              | _ -> explicitAdm.[td] <- (true, false, false)
            let pre  = Expr.Macro (afmtt f.Token 8001 "custom admissibility was called" [], 
                                 "_vcc_admissibility_pre", [mkRef p])
            let good = Expr.Macro (afmtt f.Token 8002 "state was altered after havoc_others()" [], 
                                 "_vcc_good_for_post_admissibility", [])
            f.Requires <- pre :: f.Requires
            f.Ensures <- [good] @ admChecks helper p @ stuttChecks helper p @ f.Ensures)
            
        | Top.FunctionDecl f when _list_mem (VccAttr ("is_unwrap_check", "true")) f.CustomAttr ->
          errCheck f (fun td p ->
            match explicitAdm.TryGetValue td with
              | true, (a, _, s) -> explicitAdm.[td] <- (a, true, s)
              | _ -> explicitAdm.[td] <- (false, true, false)
            let pre  = Expr.Macro (afmtt f.Token 8001 "custom unwrap check was called" [], 
                                 "_vcc_unwrap_check_pre", [mkRef p])
            let good = Expr.Macro (afmtt f.Token 8002 "state was altered after unwrap_check()" [], 
                                 "_vcc_good_for_post_can_unwrap", [])
            f.Requires <- pre :: f.Requires
            f.Ensures <- good :: unwrapChecks helper p @ f.Ensures)
            
        | _ -> ()
    decls
  
  let handleAdmissibilityChecks helper decls =
    let expl = gdict()
    decls |>
      handleCustomAdmissibilityChecks expl helper |>
      addDefaultAdmissibilityChecks expl helper
    
  // ============================================================================================================

  let addReadsChecks (helper:Helper.Env) decls =
    let calls = new Dict<_,_>()
    
    let readChecks = new Dict<_,_>()
    let revReadChecks = new Dict<_,_>()
    
    let doDecl = function
      | Top.FunctionDecl f as decl ->
        for a in f.CustomAttr do
          match a with
            | ReadsCheck (name) ->
              let add_rev () =
                if revReadChecks.ContainsKey f then
                  helper.Error (f.Token, 9639, "function '" + f.Name + "' pretends to be several read checks at once", None)
                else
                  revReadChecks.[f] <- name
              match readChecks.TryGetValue name with
                | true, f' when f = f' -> ()
                | true, _ -> 
                  helper.Warning (f.Token, 9105, "reads check for '" + name.Name + "' specified multiple times")
                  helper.Warning ((readChecks.[name]:Function).Token, 9105, "(this is the first one)")
                  add_rev ()
                | _ ->
                  readChecks.[name] <- f
                  add_rev ()
            | _ -> ()
        let fns = new Dict<_,_>()
        let mycalls = ref []
        let check self = function
          | Call (_, fn, _, _) ->
            if not (fns.ContainsKey fn) then 
              mycalls := fn :: !mycalls
              fns.Add (fn, true)
            true
          | _ -> true
        [decl] |> deepVisitExpressions check
        calls.[f] <- !mycalls
      | _ -> ()        
    
    let transCalls = new Dict<_,_>()
    let synteticReadChecks = ref []
    
    // Given a pointer to a struct/union expression, construct
    // expressions that are certainly in the owns set (this is a heuristics of course)
    let owns (expr:Expr) =
      match expr.Type with
        | Ptr (Type.Ref { Kind = Struct|Union; Invariants = invs }) ->
          let possibly = function
            | Macro (_, "keeps", [Macro (_, "this", []); p])
            | Macro (_, ("_vcc_set_in"|"_vcc_set_in0"), [p; Macro (_, "_vcc_owns", [Macro (_, "this", [])])]) ->
              let repl self = function
                | Macro (_, "this", []) -> Some expr
                | _ -> None
              Some (p.SelfMap repl)
            | _ -> None
          invs |> List.map splitConjunction |> List.concat |> revMapSome possibly
        | _ -> []
    
    let addReadCheck (f:Function) =
      if readChecks.ContainsKey f then ()
      // if it's stateless then we have a separate check
      else if f.IsStateless then ()
      else if f.RetType = Type.Void then ()
      else if _list_mem (VccAttr ("no_reads_check", "")) f.CustomAttr then ()
      // no point in checking
      else if List.exists (function Macro (_, "_vcc_set_universe", []) -> true | _ -> false) f.Reads then ()
      else
        let body =
          let tok = { bogusEC with Token = f.Token }
          Expr.MkBlock [Stmt (tok, Macro (tok, "_vcc_reads_havoc", []));
                        Return (tok, None)]
        let rc =
          { Token = f.Token
            IsSpec = true  
            OrigRetType = Void
            RetType = Void
            Name = f.Name + "#reads"
            Parameters = []
            TypeParameters = []
            Requires = []
            Ensures = []
            Writes = []
            Reads = []
            CustomAttr = [ ReadsCheck f ]
            Body = Some body
            IsProcessed = true } : Function
        let decl = Top.FunctionDecl rc
        revReadChecks.[rc] <- f
        synteticReadChecks := decl :: !synteticReadChecks
                
    let checkDecl = function
      | Top.FunctionDecl f when f.IsPure ->
        addReadCheck f
        let trans = new Dict<_,_>()
        let allCalled = ref []
        let error = ref []
        let rec add path g =
          if f = g then error := g :: path
          else if trans.ContainsKey g then ()
          else
            trans.Add (g, true)
            allCalled := g :: !allCalled
            List.iter (add (g::path)) calls.[g]
        for g in calls.[f] do
          if g.IsPure then add [f] g
          else
            helper.Error (f.Token, 9637, "the pure function '" + f.Name + "' calls '" + g.Name + "' which is not pure", Some(g.Token))
        if !error <> [] then
          helper.GraveWarning (f.Token, 9303, "cycle in pure function calls: " + (String.concat " -> " (_list_rev_map (fun (f:Function) -> f.Name) !error)))            
        transCalls.Add (f, !allCalled)
      | _ -> ()
    
    let handleReadsCheck = function
      | Top.FunctionDecl rd_f when revReadChecks.ContainsKey rd_f ->
        let f = revReadChecks.[rd_f]
        if rd_f.Requires <> [] || rd_f.Writes <> [] || rd_f.Ensures <> [] then
          helper.Error (rd_f.Token, 9640, "the reads check cannot specify additional contract", None)
        if rd_f.Parameters.Length <> f.Parameters.Length then
          if rd_f.Parameters <> [] then
            helper.Error (rd_f.Token, 9641, "the reads check and the function checked have different number of parameters", Some(f.Token))
          rd_f.Parameters <- f.Parameters |> List.map (fun p -> { p with Name = p.Name })
        let subst = new Dict<_,_>()
        let addSubst (orig:Variable) (rdchk:Variable) =
          if orig.Type <> rdchk.Type then
            helper.Error (rd_f.Token, 9642, "the reads check and function checked differ on type of parameter '" + rdchk.Name + "'", Some(f.Token))
          subst.Add (orig, rdchk)
        List.iter2 addSubst f.Parameters rd_f.Parameters
        rd_f.Requires <- [Macro (afmtt rd_f.Token 8005 "intercepted call to reads check" [], "_vcc_reads_check_pre", [])]
        let good = Expr.Macro (afmtt rd_f.Token 8006 "state was altered after reads_havoc()" [], 
                               "_vcc_reads_check_post", [])
        rd_f.Ensures <- [good]
        let mkTok name = afmtt rd_f.Token 8007 "the value of '{0}' changed (in reads check of '{1}')" [name; f.Name]
        let theOrigRetType = f.OrigRetType
        let ftok = { bogusEC with Type = f.RetType }
        let fcall = Call (ftok, f, [], List.map mkRef rd_f.Parameters)
        let rec cmp name tp (expr:Expr) =
          match tp with
            | Type.Ref ({ Kind = Struct | Union } as td) ->
              for fld in td.Fields do
                let deref =
                  if fld.Type.IsComposite then
                    vsDot ftok expr fld
                  else
                    Macro ({ ftok with Type = fld.Type }, "vs_fetch", [vsDot ftok expr fld])
                cmp (name + "." + fld.Name) fld.Type deref
            | _ -> 
              rd_f.Ensures <- Prim (mkTok name, Op ("==",Processed), [mkOld ftok "prestate" expr; expr]) :: rd_f.Ensures
        cmp "result" f.OrigRetType fcall
        
        let subst self = function
          | Expr.Ref (c, v) when subst.ContainsKey v ->
            Some (Expr.Ref (c, subst.[v]))
          | _ -> None
        let subst (e:Expr) = e.SelfMap subst
        
        let preconds = List.map (fun e -> Expr.MkAssume (subst e)) f.Requires
        let fixupHavocOthers self = function
          | Stmt (_, CallMacro (ec, "_vcc_reads_havoc", _, [])) as call ->
            let isSame (expr:Expr) =
              match expr.Type with
                | Ptr Void
                | ObjectT ->
                  helper.Error (expr.Token, 9647, "void* and obj_t are not supported in reads clauses", None)
                  Expr.MkBlock []
                | Ptr _ ->
                  Expr.MkAssume (Macro (boolBogusEC(), "reads_same", [subst expr]))
                | _ ->
                  match expr with
                    | Macro (_, "_vcc_set_empty", _) -> ()
                    | _ -> helper.Error (expr.Token, 9648, "non-pointers are not supported in reads clauses", None)
                  Expr.MkBlock []
            Some (Expr.MkBlock ([Macro (ec, "_vcc_reads_havoc", [])] @ List.map isSame f.Reads @ preconds))
          | _ -> None
        let can_frame f =
          Expr.MkAssume (Expr.Macro (boolBogusEC(), "can_use_frame_axiom_of", [Expr.Call (bogusEC, f, [], [])]))
        let inDomain (expr:Expr) =
          match expr.Type with
            | Ptr (Type.Ref { Kind = (Struct|Union) }) ->
              let inDom e = 
                Expr.MkAssume (Macro (boolBogusEC(), "_vcc_in_domain", [e; subst expr]))
              List.map inDom (subst expr :: owns expr)                
            | _ -> []
        match rd_f.Body with
          | Some b -> 
            let can_frame = List.map can_frame (List.filter (fun fn -> fn.RetType <> Type.Void) transCalls.[f]) @ (List.map inDomain f.Reads |> List.concat)
            rd_f.Body <- Some (Expr.MkBlock (preconds @ can_frame @ b.SelfMap fixupHavocOthers :: []))
          | None -> helper.Error (rd_f.Token, 9646, "the reads check is required to have a body", None)
          
      | _ -> ()
        
    List.iter doDecl decls
    List.iter checkDecl decls
    let decls = decls @ !synteticReadChecks
    List.iter handleReadsCheck decls
    decls

  // ============================================================================================================
