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
      | StructLabel of C.Expr*C.Expr // Label of a by-value structure
      | StructMeta of C.Expr*C.Expr
      | PtrCompare of C.Expr*C.Expr // Level of a pointer comparison
      | Join of SecLabel*SecLabel
      | Meet of SecLabel*SecLabel

    let getLocal name = B.Expr.Ref name
    let getPLabel bExpr = B.Expr.FunctionCall ("$select.sec.label", [B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"]); bExpr])
    let getPMeta bExpr = B.Expr.FunctionCall ("$select.sec.meta", [B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"]); bExpr])
    let getPC = B.Expr.Ref "$sec.pc"

    let setLocal name value = B.Stmt.Assign (getLocal name, value)
    let setPLabel tok loc value = B.Stmt.Call (tok, [], "$store_sec_label", [loc; value])
    let setPMeta tok loc value = B.Stmt.Call (tok, [], "$store_sec_meta", [loc; value])
    let setPC value = B.Stmt.Assign (B.Expr.Ref "$sec.pc", value)

    let rec normaliseSecLabel = function
      | Bottom
      | Top
      | ProgramContext
      | VarLabel _ | VarMeta _
      | MemLabel _ | MemMeta _
      | StructLabel _ | StructMeta _
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
        | C.Expr.Macro(_, "null", _)
        | C.Expr.Macro(_, "_vcc_set_empty", _)
        | C.Expr.IntLiteral _
        | C.Expr.BoolLiteral _
        | C.Expr.SizeOf _ -> Bottom
        | C.Expr.Cast (_,_,e') -> exprLevel e'
        | C.Expr.Ref _ as e' -> VarLabel e'
        | C.Expr.Macro (_, "_vcc_as_array", _) as e'
        | C.Expr.Deref(_,e') -> MemLabel e'
        | C.Expr.Macro (_, "_vcc_current_context", []) ->  ProgramContext
        | C.Expr.Macro (_, ("_vcc_ptr_eq" | "_vcc_ptr_neq"), [C.Expr.Cast(_, _, p1);C.Expr.Cast(_, _, p2)]) -> PtrCompare(p1,p2)
        | C.Expr.Macro (_, "_vcc_label_of", [expr]) ->
          let lblExpr = exprLevel expr
          let rec getMeta lbl =
            match lbl with
              | Bottom | Top | ProgramContext -> Bottom
              | VarMeta _ | MemMeta _ | StructMeta _ -> lbl
              | VarLabel e' -> VarMeta e'
              | MemLabel e' -> MemMeta e'
              | StructLabel(s,h) -> StructMeta(s,h)
              | PtrCompare _ -> die() // We probably want to figure something out here...
              | Meet (l1, l2) -> Meet (getMeta l1, getMeta l2)
              | Join (l1, l2) -> Join (getMeta l1, getMeta l2)
          getMeta lblExpr
        | C.Expr.Macro (_, "_vcc_vs_ctor", [s;h]) -> StructLabel(s,h)
        | C.Expr.Macro (_, "_vcc_current_state", _) -> Top
        | C.Expr.Index _
        | C.Expr.Dot _ -> Top
        | _ when e.Type = C.Type.Math "club_t" -> Top   // Those are memory addresses and information flow spec objects. We probably want them to always be high in a first approach.
        | C.Expr.Call _    // First step: no function calls
        | C.Expr.Old _     // Contracts are not supposed to cause
        | C.Expr.Quant _  -> die() //  information-flow problems
        | _ -> failwith (sprintf "Incomplete implementation: Encountered an error while trying to take the expression level of %s." (e.ToString()))
    
    let contextify = function
      | Bottom
      | ProgramContext -> ProgramContext
      | Top -> Top
      | l -> normaliseSecLabel(Join(l,ProgramContext))

    let physicalFullExtent trExpr (ptr: C.Expr) =
      let rec removeFromExtent (ptr: option<B.Expr> -> B.Expr) ({Name = n; Fields = fs}: C.TypeDecl) =
        let newPtr fName = function
          | None -> ptr (Some(B.Expr.Ref (n+"."+fName)))
          | Some v -> B.Expr.FunctionCall("$dot", [ptr (Some(B.Expr.Ref (n+"."+fName))); v])
        let recCall = List.map (fun (f: C.Field) -> match f.Type with | C.Type.Ref td -> Some(removeFromExtent (newPtr f.Name) td) | _ -> None) fs
        let flat =
          List.fold (fun l eop ->
                       match l,eop with
                         | None, None -> None
                         | None, Some _ -> eop
                         | Some _, None -> l
                         | Some l, Some l' -> Some (l@l')) None recCall
        let nested =
          match flat with
            | None -> []
            | Some es -> es
        ptr None ::
        B.Expr.FunctionCall ("$dot", [ptr None; B.Expr.Ref (n+".$owns")]) ::
        nested
      match ptr.Type with
        | C.Type.PhysPtr(C.Type.Ref td) ->
          let bPtr = trExpr ptr
          let stripPtr = function
            | None -> bPtr
            | Some f -> B.Expr.FunctionCall ("$dot", [bPtr; f])
          let minusList = removeFromExtent stripPtr td
          let minus = List.fold (fun setExpr bExpr ->
                                   match setExpr with
                                     | B.Expr.FunctionCall("$set_empty", []) -> B.Expr.FunctionCall("$set_singleton", [bExpr])
                                     | s -> B.Expr.FunctionCall("$set_union", [setExpr; B.Expr.FunctionCall("$set_singleton", [bExpr])]))
                                (B.Expr.FunctionCall("$set_empty", []))
                                minusList
          B.Expr.FunctionCall("$set_difference", [B.Expr.FunctionCall("$full_extent", [bPtr]); minus])
        | _ -> System.Console.WriteLine("Type: {0}", ptr.Type); die()

    let rec secLabelToBoogie trExpr trVar = function
      | ProgramContext -> getPC
      | PtrCompare (p1, p2) ->
        B.Expr.FunctionCall("$ptrclub.compare", [trExpr p1; trExpr p2])
      | Meet (Bottom, _)
      | Meet (_, Bottom)
      | Bottom -> B.Expr.Ref "$lblset.bot"
      | Join (Top, _)
      | Join (_, Top)
      | Top -> B.Expr.Ref "$lblset.top"
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
          | C.Expr.Deref(_, e') -> getPLabel (trExpr e')
          | _ -> failwith (sprintf "Incomplete implementation: Encountered a MemLabel with argument %s\n." (e.ToString()))
      | MemMeta e ->
        match e with
          | C.Expr.Ref(_,v) -> match v.Type with | C.Type.PhysPtr _ -> getPMeta (trExpr e) | _ -> die()
          | C.Expr.Dot _
          | C.Expr.Index _
          | C.Expr.Macro (_, "_vcc_as_array", _) -> getPMeta (trExpr e)
          | C.Expr.Deref(_, e') -> getPMeta (trExpr e')
          | _ -> failwith (sprintf "Incomplete implementation: Encountered a MemMeta with argument %s\n." (e.ToString()))
      | StructLabel(s,h) -> B.Expr.FunctionCall ("$get.seclabel.lub", [B.Expr.FunctionCall("$memory", [trExpr s]); physicalFullExtent trExpr h])
      | StructMeta(s,h) -> B.Expr.FunctionCall ("$get.metalabel.lub", [B.Expr.FunctionCall("$memory", [trExpr s]); physicalFullExtent trExpr h])
      | Meet (Top, l)
      | Meet (l, Top)
      | Join (Bottom, l)
      | Join (l, Bottom) -> secLabelToBoogie trExpr trVar l
      | Join (l1, l2) -> B.Expr.FunctionCall ("$lblset.join", [secLabelToBoogie trExpr trVar l1;secLabelToBoogie trExpr trVar l2])
      | Meet (l1, l2) -> B.Expr.FunctionCall ("$lblset.meet", [secLabelToBoogie trExpr trVar l1;secLabelToBoogie trExpr trVar l2])

    let scanForIFAnnotations (decl:C.Top) =
      let res = ref false
      let bodyHasIFAnnotations _ = function
        | C.Expr.If(_, Some _, _, _, _)
        | C.Expr.Macro(_, "_vcc_lblset_leq", _)
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

    let mkSecLattice = []
//      let mkConst n t = { Unique = true 
//                          Name = n
//                          Type = t } : B.ConstData
//      [B.Decl.Const (mkConst "$lblset.top" (B.Type.Ref "$seclabel"))
//       B.Decl.Const (mkConst "$lblset.bot" (B.Type.Ref "$seclabel"))
//       B.Decl.Axiom (B.Expr.Forall(Token.NoToken, [("l",B.Type.Ref "$seclabel")], [], [], B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "l"; B.Expr.Ref "$lblset.top"])))
//       B.Decl.Axiom (B.Expr.Forall(Token.NoToken, [("l",B.Type.Ref "$seclabel")], [], [], B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "$lblset.bot"; B.Expr.Ref "l"])))
//       B.Decl.Axiom (B.Expr.Primitive ("!", [B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "$lblset.top"; B.Expr.Ref "$lblset.bot"])]))
//       
//       B.Decl.Function (B.Type.Ref "$seclabel", [], "$lblset.meet", ["l1", B.Type.Ref "$seclabel";"l2", B.Type.Ref "$seclabel"])
//       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
//                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"],
//                                    [], [],
//                                    B.Expr.Primitive ("&&",
//                                                      [B.Expr.FunctionCall("$lblset.leq",  [B.Expr.FunctionCall ("$lblset.meet", [B.Expr.Ref "l1";B.Expr.Ref "l2"]); B.Expr.Ref "l1"])
//                                                       B.Expr.FunctionCall("$lblset.leq",  [B.Expr.FunctionCall ("$lblset.meet", [B.Expr.Ref "l1";B.Expr.Ref "l2"]); B.Expr.Ref "l2"])])))
//       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
//                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"; "l", B.Type.Ref "$seclabel"],
//                                    [], [],
//                                    B.Expr.Primitive ("==>", 
//                                                      [B.Expr.Primitive ("&&",
//                                                                         [B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "l"; B.Expr.Ref "l1"])
//                                                                          B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "l"; B.Expr.Ref "l2"])])
//                                                       B.Expr.FunctionCall("$lblset.leq", 
//                                                                         [B.Expr.Ref "l"
//                                                                          B.Expr.FunctionCall ("$lblset.meet", [B.Expr.Ref "l1"; B.Expr.Ref "l2"])])])))
//       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
//                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"],
//                                    [], [],
//                                    B.Expr.Primitive ("==", [B.Expr.FunctionCall ("$lblset.meet", [B.Expr.Ref "l1"; B.Expr.Ref "l2"])
//                                                             B.Expr.FunctionCall ("$lblset.meet", [B.Expr.Ref "l2"; B.Expr.Ref "l1"])])))
//       
//       B.Decl.Function (B.Type.Ref "$seclabel", [], "$lblset.join", ["l1", B.Type.Ref "$seclabel";"l2", B.Type.Ref "$seclabel"])
//       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
//                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"],
//                                    [], [],
//                                    B.Expr.Primitive ("&&",
//                                                      [B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "l1"; B.Expr.FunctionCall ("$lblset.join", [B.Expr.Ref "l1";B.Expr.Ref "l2"])])
//                                                       B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "l2"; B.Expr.FunctionCall ("$lblset.join", [B.Expr.Ref "l1";B.Expr.Ref "l2"])])])))
//       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
//                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"; "l", B.Type.Ref "$seclabel"],
//                                    [], [],
//                                    B.Expr.Primitive ("==>", 
//                                                      [B.Expr.Primitive ("&&",
//                                                                         [B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "l1"; B.Expr.Ref "l"])
//                                                                          B.Expr.FunctionCall("$lblset.leq",  [B.Expr.Ref "l2"; B.Expr.Ref "l"])])
//                                                       B.Expr.FunctionCall("$lblset.leq", 
//                                                                         [B.Expr.FunctionCall ("$lblset.join", [B.Expr.Ref "l1"; B.Expr.Ref "l2"])
//                                                                          B.Expr.Ref "l"])])))
//       B.Decl.Axiom (B.Expr.Forall (Token.NoToken,
//                                    ["l1", B.Type.Ref "$seclabel"; "l2", B.Type.Ref "$seclabel"],
//                                    [], [],
//                                    B.Expr.Primitive ("==", [B.Expr.FunctionCall ("$lblset.join", [B.Expr.Ref "l1"; B.Expr.Ref "l2"])
//                                                             B.Expr.FunctionCall ("$lblset.join", [B.Expr.Ref "l2"; B.Expr.Ref "l1"])])))
//     ]

// Transformations for if statements
    let rec insert acc e = function
      | [] -> e :: acc
      | e'::_ as es when e' = e -> es @ acc
      | e'::es -> insert (e'::acc) e es
    let union es es' = List.foldBack (insert []) es es'

    let makePermissiveUpgrade trExpr trVar trType cExpr testClassif =
      let rec getFreeVars acc = function
        | C.Expr.Ref _
        | C.Expr.Deref (_,(C.Expr.Ref _|C.Expr.Dot _)) as e -> [e] // Base cases
        | C.Expr.Macro (_,_,es)
        | C.Expr.Prim(_,_,es) -> List.foldBack (fun e vars -> let vars' = getFreeVars [] e in union vars' vars) es ([])
        | C.Expr.IntLiteral _
        | C.Expr.BoolLiteral _
        | C.Expr.SizeOf _ -> []
        | C.Expr.Cast (_,_,e) -> getFreeVars acc e
        | C.Expr.Call _
        | C.Expr.Dot _
        | C.Expr.Index _
        | C.Expr.Old _
        | C.Expr.Quant _
        | _ -> die()
      let freeVars = getFreeVars [] cExpr
      let ptr = B.Expr.Ref "PU#CLS#ptr"
      let construct v expr =
        match v with
          | C.Expr.Ref (_,v) ->  B.Expr.Primitive ("||",
                                                   [B.Expr.Primitive ("&&",
                                                                      [B.Expr.Primitive ("!", [B.Expr.FunctionCall("$seclbl.leq",  [B.Expr.ArrayIndex (getLocal ("SecLabel#"+trVar v), [ptr]); B.Expr.Ref "$seclbl.bot"])])
                                                                       B.Expr.FunctionCall("$seclbl.leq",  [B.Expr.ArrayIndex (getLocal ("SecMeta#"+trVar v), [ptr]); B.Expr.Ref "$seclbl.bot"])])
                                                    expr])
          | C.Expr.Deref (_,e) ->
            let bType = trType (v.Type)
            let getLabel = getPLabel (trExpr e)
            let getMeta = getPMeta (trExpr e)
            B.Expr.Primitive ("||",
                              [B.Expr.Primitive ("&&",
                                                 [B.Expr.Primitive ("!", [B.Expr.FunctionCall("$seclbl.leq",  [B.Expr.ArrayIndex (getLabel, [ptr]); B.Expr.Ref "$seclbl.bot"])])
                                                  B.Expr.FunctionCall("$seclbl.leq",  [B.Expr.ArrayIndex (getMeta, [ptr]); B.Expr.Ref "$seclbl.bot"])])
                               expr])
          | _ -> die()
      B.Expr.Lambda (Token.NoToken,
                     ["PU#CLS#ptr",B.Type.Ref "$ptr"],
                     [],
                     B.Expr.Primitive("||",
                                      [B.Expr.FunctionCall("$select.$map_t..$ptr_to..^^void.^^bool", [testClassif; ptr])
                                       List.foldBack construct freeVars (B.Expr.Primitive ("!", [B.Expr.FunctionCall("$seclbl.leq",  [B.Expr.ArrayIndex (getPC, [ptr]); B.Expr.Ref "$seclbl.bot"])]))]))

    let bExprLevel bExpr =
      let rec getVarLevels acc = function
        | B.Expr.Ref (vname) when vname.StartsWith "SecLabel#" -> Set.ofList [false,"SecMeta#"+vname.Substring 9],acc
        | B.Expr.Ref (vname) when vname.StartsWith "SecMeta#" -> Set.ofList [false,vname],acc
        | B.Expr.Ref (vname) when vname.StartsWith "L#" -> Set.ofList[false,"SecLabel#"+vname],acc
        | B.Expr.Ref (vname) -> Set.ofList [false,"SecLabel#"+vname],acc
        | B.Expr.Ref "$sec.pc" -> Set.empty,acc
        | B.Expr.FunctionCall ("$select.sec.label", [_;B.Expr.FunctionCall ("$ptr", [t;B.Expr.Ref p])]) -> Set.ofList [true,p],(p,t)::acc
        | B.Expr.FunctionCall ("$select.sec.meta", [_;B.Expr.FunctionCall ("$ptr", [t;B.Expr.Ref p])]) -> Set.ofList [true,p],(p,t)::acc
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
                  B.Expr.FunctionCall ("&&", [expr; B.Expr.FunctionCall("$lblset.leq",  [getPMeta (B.Expr.FunctionCall ("$ptr", [t;B.Expr.Ref v])); B.Expr.Ref "$lblset.bot"])])
             else B.Expr.Primitive ("&&", [expr; B.Expr.FunctionCall("$lblset.leq",  [getLocal v; B.Expr.Ref "$lblset.bot"])])
      Set.foldBack construct varLevels (B.Expr.BoolLiteral true)
