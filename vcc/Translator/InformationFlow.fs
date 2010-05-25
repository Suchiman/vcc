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
    type SecLabel =
      | Bottom
      | Top
      | ProgramContext
      | VarLabel of C.Expr  // Label of a variable
      | MemLabel of C.Expr  // Label of a memory location
      // The label of a dereferenced value is the join of the labels of the variable and the memory location
      | Join of SecLabel*SecLabel
      | Meet of SecLabel*SecLabel

    let rec exprLevel (e:C.Expr) = 
      match e with
        | C.Expr.Prim(_,C.Op(">",_),[e1;e2])
        | C.Expr.Prim(_,C.Op("<",_),[e1;e2])
        | C.Expr.Prim(_,C.Op("<=",_),[e1;e2])
        | C.Expr.Prim(_,C.Op(">=",_),[e1;e2])
        | C.Expr.Prim(_,C.Op("==",_),[e1;e2]) ->
          let b1,b2 = exprLevel e1, exprLevel e2
          match b1,b2 with
            | ProgramContext,ProgramContext -> Bottom
            | ProgramContext,b
            | b,ProgramContext -> b
            | _,_ -> Join(b1,b2)
        | C.Expr.Prim(_,_,es) ->
          List.foldBack (fun e lbl -> Join (lbl,(exprLevel e))) es ProgramContext    // The arguments' labels are joined
        | C.Expr.IntLiteral _
        | C.Expr.BoolLiteral _
        | C.Expr.SizeOf _ -> ProgramContext
        | C.Expr.Cast (_,_,e) -> exprLevel e
        | C.Expr.Ref _ as e -> VarLabel e
        | C.Expr.Deref(_,e') -> MemLabel e'// Join (exprLevel e', MemLabel e')
        | C.Expr.Call (_, fn, [], []) when  fn.Name = "$current_context" -> ProgramContext
        | C.Expr.Call _    // First step: no function calls, ...
        | C.Expr.Dot _     // ... or structures, ...
        | C.Expr.Index _   // ... or arrays.
        | C.Expr.Old _     // Contracts are not supposed to cause
        | C.Expr.Quant _   //  information-flow problems
        | _ -> die()       // We limit ourselves to real expressions


    let rec secLabelToBoogie trExpr trVar = function
      | ProgramContext -> B.Expr.FunctionCall("$get.secpc",[B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"])])
      | Meet (Bottom, _)
      | Meet (_, Bottom)
      | Bottom -> B.Expr.BoolLiteral true
      | Join (Top, _)
      | Join (_, Top)
      | Top -> B.Expr.BoolLiteral false
      | VarLabel e -> 
        match e with
          | C.Expr.Ref(_,v) -> B.Expr.Ref ("SecLabel#"+(trVar v))
          | _ -> die()
      | MemLabel e ->
        match e with
          | C.Expr.Ref(_,v) -> match v.Type with | C.Type.PhysPtr _ -> B.Expr.FunctionCall ("$get.seclabel", [B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"]); trExpr e]) | _ -> die()
          | _ -> failwith (sprintf "Incomplete implementation: Encountered a MemLabel with argument %s\n." (e.ToString()))
      | Meet (Top, l)
      | Meet (l, Top)
      | Join (Bottom, l)
      | Join (l, Bottom) -> secLabelToBoogie trExpr trVar l
      | Join (l1, l2) -> B.Expr.Primitive ("&&", [secLabelToBoogie trExpr trVar l1;secLabelToBoogie trExpr trVar l2])
      | Meet (l1, l2) -> B.Expr.Primitive ("||", [secLabelToBoogie trExpr trVar l1;secLabelToBoogie trExpr trVar l2])

    let makePermissiveUpgrade trVar cExpr testClassif =
      let rec getFreeVars = function
        | C.Expr.Prim(_,_,es) -> List.foldBack (fun e vars -> Set.union (getFreeVars e) vars) es Set.empty
        | C.Expr.IntLiteral _
        | C.Expr.BoolLiteral _
        | C.Expr.SizeOf _ -> Set.empty
        | C.Expr.Cast (_,_,e) -> getFreeVars e
        | C.Expr.Ref (_,v) -> Set.ofList [Local (trVar v)]
        | C.Expr.Deref (_,e) ->
          match e with
            | C.Expr.Ref (_,p) -> Set.ofList [Pointer (trVar p)]
            | _ -> failwith (sprintf "Incomplete implementation: Encountered a deref with non-reference argument %s\n." (e.ToString()))
        | C.Expr.Call _
        | C.Expr.Dot _
        | C.Expr.Index _
        | C.Expr.Old _
        | C.Expr.Quant _
        | _ -> die()
      let freeVars = getFreeVars cExpr
      let construct v expr =
        match v with
          | Local n ->  B.Expr.Primitive ("||", [expr;B.Expr.Primitive ("==>", [B.Expr.Primitive ("!", [B.Expr.Ref ("SecLabel#"+n)]);B.Expr.Ref ("SecMeta#"+n)])])
          | Pointer p -> B.Expr.Primitive ("||", [expr;B.Expr.Primitive ("==>", [B.Expr.Primitive ("!", [B.Expr.FunctionCall ("$get.seclabel", [B.Expr.Ref p])]);B.Expr.FunctionCall ("$memory_location_metalabel", [B.Expr.Ref p])])])
      let newDisjuncts = Set.foldBack construct freeVars (B.Expr.Primitive ("!", [B.Expr.FunctionCall("$get.secpc",[B.Expr.FunctionCall("$memory",[B.Expr.Ref "$s"])])]))
      match testClassif with
        | B.Expr.BoolLiteral false -> newDisjuncts
        | B.Expr.BoolLiteral true -> testClassif
        | _ -> B.Expr.Primitive ("||", [testClassif;newDisjuncts])

    let makeHazardousCheck bExpr =
      let rec getVarLevels = function
        | B.Expr.Ref (vname) when vname.StartsWith "SecLabel#" -> Set.ofList [false,"SecMeta#"+vname.Substring 9]
        | B.Expr.Ref (vname) when vname.StartsWith "SecMeta#" -> Set.ofList [false,vname]
        | B.Expr.Ref (vname) when vname.StartsWith "L#" -> Set.ofList[false,"SecLabel#"+vname]
        | B.Expr.Ref (vname) -> Set.ofList [false,"SecLabel#"+vname]
        | B.Expr.FunctionCall ("$get.secpc", _) -> Set.empty
        | B.Expr.FunctionCall ("$get.seclabel", [B.Expr.Ref p]) -> Set.ofList [true,p]
        | B.Expr.FunctionCall ("$get.metalabel", [B.Expr.Ref p]) -> Set.ofList [true,p]
        | B.Expr.BoolLiteral _
        | B.Expr.IntLiteral _
        | B.Expr.BvLiteral _ -> Set.empty
        | B.Expr.BvConcat (e1,e2) -> Set.union (getVarLevels e1) (getVarLevels e2)
        | B.Expr.BvExtract (e,_,_) -> getVarLevels e
        | B.Expr.Primitive (_,es) -> List.fold (fun s e -> Set.union s (getVarLevels e)) Set.empty es
        | _ as e -> failwith (sprintf "Incomplete implementation: Failed on making hazardous check for expression %s\n." (e.ToString()))
      let varLevels = getVarLevels bExpr
      let construct (b,v) expr =
        if b then B.Expr.Primitive ("&&", [expr;B.Expr.FunctionCall ("$get.seclabel", [B.Expr.Ref v])])
             else B.Expr.Primitive ("&&", [expr;B.Expr.Ref v])
      Set.foldBack construct varLevels (B.Expr.BoolLiteral true)

    let scanForIFAnnotations (decl:C.Top) =
      let res = ref false
      let bodyHasIFAnnotations _ = function
        | C.Expr.If(_, Some _, _, _, _)
        | C.Expr.Macro(_, "_vcc_is_low", _)
        | C.Expr.Macro(_, "_vcc_downgrade_to", _)
        | C.Expr.Macro(_, "_vcc_current_context", _) -> res := true; false
        | _ -> true
      match decl with
        | C.Top.FunctionDecl fn ->
          match fn.Body with
            | None -> false
            | Some body -> body.SelfVisit(bodyHasIFAnnotations); !res
        | _ -> false