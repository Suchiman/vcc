//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
  open System.Text
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.Util
  
  module C = Microsoft.Research.Vcc.CAST
  module B = Microsoft.Research.Vcc.BoogieAST
  module TU = Microsoft.Research.Vcc.TranslatorUtils

  type Variable =
    | Local of string
    | Pointer of string

  module InformationFlow =
    let freshPG = ref (bigint.Zero)
    let freshPtrGrp () = freshPG := bigint.op_Subtraction(!freshPG,bigint.One); !freshPG

    type SecLabel =
      | Bottom
      | Top
      | ProgramContext
      | VarLabel of C.Expr  // Label of a variable
      | VarMeta of C.Expr
      | MemLabel of C.Expr  // Label of a memory location
      | MemMeta of C.Expr
      | PtrCompare of C.Expr*C.Expr // Level of a pointer comparison
      | Join of SecLabel*SecLabel
      | Meet of SecLabel*SecLabel

    let getLocal name = B.Expr.Ref name
    let getPLabel bExpr = B.Expr.FunctionCall ("$get.seclabel", [B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"]); bExpr])
    let getPMeta bExpr = B.Expr.FunctionCall ("$get.metalabel", [B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"]); bExpr])
    let getPC = B.Expr.FunctionCall("$get.secpc",[B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"])])
    let getPG bExpr = B.Expr.FunctionCall ("$get.ptrgrp", [B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"]); bExpr])

    let setLocal name value = B.Stmt.Assign (getLocal name, value)
    let setPLabel tok loc value = B.Stmt.Call (tok, [], "$set_label", [loc; value])
    let setPMeta tok loc value = B.Stmt.Call (tok, [], "$set_meta", [loc; value])
    let setPC tok value = B.Stmt.Call(tok, [], "$set_pc", [value]);
    let setPG tok loc value = B.Stmt.Call(tok, [], "$set_ptr_grp", [loc; (B.Expr.IntLiteral value)])

    let rec normaliseSecLabel = function
      | Bottom
      | Top
      | ProgramContext
      | VarLabel _
      | VarMeta _
      | MemLabel _
      | MemMeta _
      | PtrCompare _ as l -> l
      | Join(Bottom,l)
      | Join(l,Bottom)
      | Meet(Top,l)
      | Meet(l,Top) -> normaliseSecLabel l
      | Join(l1,l2) ->
        let l1' = normaliseSecLabel l1
        let l2' = normaliseSecLabel l2
        match l1',l2' with
          | Bottom,l
          | l,Bottom -> l
          | Top,l
          | l,Top -> Top
          | _,_ -> Join(l1',l2')
      | Meet(l1,l2) ->
        let l1' = normaliseSecLabel l1
        let l2' = normaliseSecLabel l2
        match l1',l2' with
          | Bottom,l
          | l,Bottom -> Bottom
          | Top,l
          | l,Top -> l
          | _,_ -> Meet(l1',l2')

    let rec exprLevel (e:C.Expr) = 
      match e with
        | C.Expr.Prim(_,_,es) ->
          let ugly = List.foldBack (fun e lbl -> Join (lbl,(exprLevel e))) es Bottom    // The arguments' labels are joined
          normaliseSecLabel ugly
        | C.Expr.IntLiteral _
        | C.Expr.BoolLiteral _
        | C.Expr.SizeOf _ -> Bottom
        | C.Expr.Cast (_,_,e') -> exprLevel e'
        | C.Expr.Ref _ as e' -> VarLabel e'
        | C.Expr.Macro (_, "_vcc_as_array", _) as e'
        | C.Expr.Deref(_,e') -> MemLabel e'
        | C.Expr.Macro (_, "_vcc_current_context", []) ->  ProgramContext
        | C.Expr.Macro (_, ("_vcc_ptr_eq" | "_vcc_ptr_neq"), [p1;p2]) -> PtrCompare(p1,p2)
        | C.Expr.Macro (_, "_vcc_label_of", [expr]) ->
          let lblExpr = exprLevel expr
          let rec getMeta lbl =
            match lbl with
              | Bottom | Top | ProgramContext -> Bottom
              | VarMeta _ | MemMeta _ -> lbl
              | VarLabel e' -> VarMeta e'
              | MemLabel e' -> MemMeta e'
              | PtrCompare _ -> die() // We probably want to figure something out here...
              | Meet (l1, l2) -> Meet (getMeta l1, getMeta l2)
              | Join (l1, l2) -> Join (getMeta l1, getMeta l2)
          getMeta lblExpr
        | C.Expr.Index _
        | C.Expr.Dot _ -> Top // This is a memory address, and as such is always high
        | C.Expr.Call _    // First step: no function calls
        | C.Expr.Old _     // Contracts are not supposed to cause
        | C.Expr.Quant _  -> die() //  information-flow problems
        | _ -> failwith (sprintf "Incomplete implementation: Encountered an error while trying to take the expression level of %s." (e.ToString()))

    let rec exprPtrGroup trExpr (e:C.Expr) =
      let doPtr = function
        | C.Expr.Ref _ as e -> Some (getPG (trExpr e))
        | C.Expr.Prim (_, _, es) ->
          let grps = List.map (exprPtrGroup trExpr) es
          let mkCond grp (eqTest,init) =
            match init,grp with
              | _,None -> eqTest,init
              | None,_ -> eqTest,grp
              | Some i,Some g ->
                match eqTest with
                  | None -> Some(B.Expr.Primitive("==",[g;i])),init
                  | Some t -> Some(B.Expr.Primitive("&&", [B.Expr.Primitive("==",[g;i]);t])),init
          let cond,init = List.foldBack mkCond grps (None,None)
          match cond,init with
            | None,_
            | _,None -> die()
            | Some cond',Some init' ->
              match cond' with
                | B.Expr.Primitive ("&&", _) -> Some (B.Expr.Ite(cond',init',B.Expr.IntLiteral (freshPtrGrp ())))
                | _ -> cond
        | C.Expr.Index _ as e -> Some(getPG (trExpr e))
        | C.Expr.Macro (_,"null", []) -> Some (B.Expr.IntLiteral (bigint.Zero))
        | C.Expr.Cast (_, _, e) -> exprPtrGroup trExpr e
        | C.Expr.Dot _ as e -> Some (getPG (trExpr e))
        | _ -> None
      match e.Type with
        | C.Type.PhysPtr _
        | C.Type.SpecPtr _ -> doPtr e
        | _ -> die()
    
    let contextify = function
      | Bottom
      | ProgramContext -> ProgramContext
      | Top -> Top
      | l -> normaliseSecLabel(Join(l,ProgramContext))

    let rec secLabelToBoogie trExpr trVar = function
      | ProgramContext -> getPC
      | PtrCompare (p1,p2) ->
        match exprPtrGroup trExpr p1, exprPtrGroup trExpr p2 with
          | None,_
          | _,None -> die()
          | Some pG1,Some pG2 -> B.Expr.Ite (B.Expr.Primitive ("==", [pG1; pG2]),
                                             B.Expr.Ref "$sec.bot",
                                             B.Expr.Ref "$sec.top")
      | Meet (Bottom, _)
      | Meet (_, Bottom)
      | Bottom -> B.Expr.Ref "$sec.bot"
      | Join (Top, _)
      | Join (_, Top)
      | Top -> B.Expr.Ref "$sec.top"
      | VarLabel e -> 
        match e with
          | C.Expr.Ref(_,v) -> getLocal ("SecLabel#"+(trVar v))
          | C.Expr.Result _ -> getLocal ("SecLabel#special#result")
          | _ -> die()
      | VarMeta e ->
        match e with
          | C.Expr.Ref(_,v) -> getLocal ("SecMeta#"+(trVar v))
          | C.Expr.Result _ -> getLocal ("SecMeta#special#result")
          | _ -> die()
      | MemLabel e ->
        match e with
          | C.Expr.Ref(_, v) -> match v.Type with | C.Type.PhysPtr _ -> getPLabel (trExpr e) | _ -> die()
          | C.Expr.Dot _
          | C.Expr.Index _
          | C.Expr.Macro (_, "_vcc_as_array", _) -> getPLabel (trExpr e)
          | _ -> failwith (sprintf "Incomplete implementation: Encountered a MemLabel with argument %s\n." (e.ToString()))
      | MemMeta e ->
        match e with
          | C.Expr.Ref(_,v) -> match v.Type with | C.Type.PhysPtr _ -> getPMeta (trExpr e) | _ -> die()
          | C.Expr.Dot _
          | C.Expr.Index _
          | C.Expr.Macro (_, "_vcc_as_array", _) -> getPMeta (trExpr e)
          | _ -> failwith (sprintf "Incomplete implementation: Encountered a MemMeta with argument %s\n." (e.ToString()))
      | Meet (Top, l)
      | Meet (l, Top)
      | Join (Bottom, l)
      | Join (l, Bottom) -> secLabelToBoogie trExpr trVar l
      | Join (l1, l2) -> B.Expr.FunctionCall ("$sec.join", [secLabelToBoogie trExpr trVar l1;secLabelToBoogie trExpr trVar l2])
      | Meet (l1, l2) -> B.Expr.FunctionCall ("$sec.meet", [secLabelToBoogie trExpr trVar l1;secLabelToBoogie trExpr trVar l2])

    let scanForIFAnnotations (decl:C.Top) =
      let res = ref false
      let bodyHasIFAnnotations _ = function
        | C.Expr.If(_, Some _, _, _, _)
        | C.Expr.Macro(_, "_vcc_sec_leq", _)
        | C.Expr.Macro(_, "_vcc_is_lower", _)
        | C.Expr.Macro(_, "_vcc_downgrade_to", _)
        | C.Expr.Macro(_, "_vcc_current_context", _)
        | C.Expr.Macro(_,"_vcc_label_of",_) -> res := true; false
        | _ -> true
      match decl with
        | C.Top.FunctionDecl fn ->
          match fn.Body with
            | None -> false
            | Some body -> body.SelfVisit(bodyHasIFAnnotations); !res
        | _ -> false

    let mkSecLattice =
      let mkConst n t = { Unique = true 
                          Name = n
                          Type = t } : B.ConstData
      [B.Decl.Const (mkConst "$sec.top" (B.Type.Ref "$seclabel"))
       B.Decl.Const (mkConst "$sec.bot" (B.Type.Ref "$seclabel"))
       B.Decl.Axiom (B.Expr.Forall(Token.NoToken, [("l",B.Type.Ref "$seclabel")], [], [], B.Expr.Primitive ("<:", [B.Expr.Ref "l"; B.Expr.Ref "$sec.top"])))
       B.Decl.Axiom (B.Expr.Forall(Token.NoToken, [("l",B.Type.Ref "$seclabel")], [], [], B.Expr.Primitive ("<:", [B.Expr.Ref "$sec.bot"; B.Expr.Ref "l"])))
       B.Decl.Axiom (B.Expr.Primitive ("!", [B.Expr.Primitive ("<:", [B.Expr.Ref "$sec.top"; B.Expr.Ref "$sec.bot"])]))
       
       B.Decl.Function (B.Type.Ref "$seclabel", [], "$sec.meet", ["l1", B.Type.Ref "$seclabel";"l2", B.Type.Ref "$seclabel"])
       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"],
                                    [], [],
                                    B.Expr.Primitive ("&&",
                                                      [B.Expr.Primitive ("<:", [B.Expr.FunctionCall ("$sec.meet", [B.Expr.Ref "l1";B.Expr.Ref "l2"]); B.Expr.Ref "l1"])
                                                       B.Expr.Primitive ("<:", [B.Expr.FunctionCall ("$sec.meet", [B.Expr.Ref "l1";B.Expr.Ref "l2"]); B.Expr.Ref "l2"])])))
       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"; "l", B.Type.Ref "$seclabel"],
                                    [], [],
                                    B.Expr.Primitive ("==>", 
                                                      [B.Expr.Primitive ("&&",
                                                                         [B.Expr.Primitive ("<:", [B.Expr.Ref "l"; B.Expr.Ref "l1"])
                                                                          B.Expr.Primitive ("<:", [B.Expr.Ref "l"; B.Expr.Ref "l2"])])
                                                       B.Expr.Primitive ("<:",
                                                                         [B.Expr.FunctionCall ("$sec.meet", [B.Expr.Ref "l1"; B.Expr.Ref "l2"])
                                                                          B.Expr.Ref "l"])])))
       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"],
                                    [], [],
                                    B.Expr.Primitive ("==", [B.Expr.FunctionCall ("$sec.meet", [B.Expr.Ref "l1"; B.Expr.Ref "l2"])
                                                             B.Expr.FunctionCall ("$sec.meet", [B.Expr.Ref "l2"; B.Expr.Ref "l1"])])))
       
       B.Decl.Function (B.Type.Ref "$seclabel", [], "$sec.join", ["l1", B.Type.Ref "$seclabel";"l2", B.Type.Ref "$seclabel"])
       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"],
                                    [], [],
                                    B.Expr.Primitive ("&&",
                                                      [B.Expr.Primitive ("<:", [B.Expr.Ref "l1"; B.Expr.FunctionCall ("$sec.join", [B.Expr.Ref "l1";B.Expr.Ref "l2"])])
                                                       B.Expr.Primitive ("<:", [B.Expr.Ref "l2"; B.Expr.FunctionCall ("$sec.join", [B.Expr.Ref "l1";B.Expr.Ref "l2"])])])))
       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"; "l", B.Type.Ref "$seclabel"],
                                    [], [],
                                    B.Expr.Primitive ("==>", 
                                                      [B.Expr.Primitive ("&&",
                                                                         [B.Expr.Primitive ("<:", [B.Expr.Ref "l1"; B.Expr.Ref "l"])
                                                                          B.Expr.Primitive ("<:", [B.Expr.Ref "l2"; B.Expr.Ref "l"])])
                                                       B.Expr.Primitive ("<:",
                                                                         [B.Expr.Ref "l"
                                                                          B.Expr.FunctionCall ("$sec.join", [B.Expr.Ref "l1"; B.Expr.Ref "l2"])])])))
       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"],
                                    [], [],
                                    B.Expr.Primitive ("==", [B.Expr.FunctionCall ("$sec.join", [B.Expr.Ref "l1"; B.Expr.Ref "l2"])
                                                             B.Expr.FunctionCall ("$sec.join", [B.Expr.Ref "l2"; B.Expr.Ref "l1"])])))
     ]

// Transformations for if statements
    let makePermissiveUpgrade trVar trType cExpr testClassif =
      let rec getFreeVars acc = function
        | C.Expr.Macro (_,_,es)
        | C.Expr.Prim(_,_,es) -> List.foldBack (fun e (vars,types) -> let (vars',types') = getFreeVars [] e in Set.union vars' vars,types'@types) es (Set.empty,acc)
        | C.Expr.IntLiteral _
        | C.Expr.BoolLiteral _
        | C.Expr.SizeOf _ -> Set.empty,acc
        | C.Expr.Cast (_,_,e) -> getFreeVars acc e
        | C.Expr.Ref (_,v) -> Set.ofList [Local (trVar v)],acc
        | C.Expr.Deref (_,e) ->
          match e with
            | C.Expr.Ref (_,p) -> Set.ofList [Pointer (trVar p)], (Pointer (trVar p),e.Type)::acc
            | _ -> failwith (sprintf "Incomplete implementation: Encountered a deref with non-reference argument %s\n." (e.ToString()))
        | C.Expr.Call _
        | C.Expr.Dot _
        | C.Expr.Index _
        | C.Expr.Old _
        | C.Expr.Quant _
        | _ -> die()
      let (freeVars,typeAssocs) = getFreeVars [] cExpr
      let construct v expr =
        match v with
          | Local n ->  B.Expr.Primitive ("||",
                                          [B.Expr.Primitive ("&&",
                                                             [B.Expr.Primitive ("!", [B.Expr.Primitive ("<:", [getLocal ("SecLabel#"+n); B.Expr.Ref "$sec.bot"])])
                                                              B.Expr.Primitive ("<:", [getLocal ("SecMeta#"+n); B.Expr.Ref "$sec.bot"])])
                                           expr])
          | Pointer p ->
            let bType = trType (snd(List.find (fun (var,typ) -> var = v) typeAssocs))
            let getLabel = getPLabel (B.Expr.FunctionCall ("$ptr", [bType; B.Expr.Ref p]))
            let getMeta = getPMeta (B.Expr.FunctionCall ("$ptr", [bType; B.Expr.Ref p]))
            B.Expr.Primitive ("||",
                              [B.Expr.Primitive ("&&",
                                                 [B.Expr.Primitive ("!", [B.Expr.Primitive ("<:", [getLabel; B.Expr.Ref "$sec.bot"])])
                                                  B.Expr.Primitive ("<:", [getMeta; B.Expr.Ref "$sec.bot"])])
                               expr])
      let newDisjuncts = Set.foldBack construct freeVars (B.Expr.Primitive ("!", [B.Expr.Primitive ("<:", [getPC; B.Expr.Ref "$sec.bot"])]))
      match testClassif with
        | B.Expr.BoolLiteral false -> newDisjuncts
        | B.Expr.BoolLiteral true -> testClassif
        | _ -> B.Expr.Primitive ("||", [testClassif;newDisjuncts])

    let makeHazardousCheck bExpr =
      let rec getVarLevels acc = function
        | B.Expr.Ref (vname) when vname.StartsWith "SecLabel#" -> Set.ofList [false,"SecMeta#"+vname.Substring 9],acc
        | B.Expr.Ref (vname) when vname.StartsWith "SecMeta#" -> Set.ofList [false,vname],acc
        | B.Expr.Ref (vname) when vname.StartsWith "L#" -> Set.ofList[false,"SecLabel#"+vname],acc
        | B.Expr.Ref (vname) -> Set.ofList [false,"SecLabel#"+vname],acc
        | B.Expr.FunctionCall ("$get.secpc", _) -> Set.empty,acc
        | B.Expr.FunctionCall ("$get.seclabel", [_;B.Expr.FunctionCall ("$ptr", [t;B.Expr.Ref p])]) -> Set.ofList [true,p],(p,t)::acc
        | B.Expr.FunctionCall ("$get.metalabel", [_;B.Expr.FunctionCall ("$ptr", [t;B.Expr.Ref p])]) -> Set.ofList [true,p],(p,t)::acc
        | B.Expr.BoolLiteral _
        | B.Expr.IntLiteral _
        | B.Expr.BvLiteral _ -> Set.empty,acc
        | B.Expr.BvConcat (e1,e2) ->
          let (v1,t1) = getVarLevels [] e1
          let (v2,t2) = getVarLevels [] e2
          Set.union v1 v2,t1@t2@acc
        | B.Expr.BvExtract (e,_,_) -> getVarLevels acc e
        | B.Expr.Primitive (_,es) -> List.fold (fun (vs,ts) e -> let v,t = getVarLevels [] e in Set.union vs v,t@ts) (Set.empty,acc) es
        | _ as e -> failwith (sprintf "Incomplete implementation: Failed on making hazardous check for expression %s\n." (e.ToString()))
      let (varLevels,types) = getVarLevels [] bExpr
      let construct (b,v) expr =
        if b then let t = snd(List.find (fun (var,_) -> var = v) types)
                  B.Expr.FunctionCall ("&&", [expr; B.Expr.Primitive ("<:", [getPMeta (B.Expr.FunctionCall ("$ptr", [t;B.Expr.Ref v])); B.Expr.Ref "$sec.bot"])])
             else B.Expr.Primitive ("&&", [expr; B.Expr.Primitive ("<:", [getLocal v; B.Expr.Ref "$sec.bot"])])
      Set.foldBack construct varLevels (B.Expr.BoolLiteral true)