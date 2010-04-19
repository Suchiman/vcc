﻿//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
  open Microsoft.FSharp.Math
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.Util
  open System
  
  module C = Microsoft.Research.Vcc.CAST
  module B = Microsoft.Research.Vcc.BoogieAST
  
  module TranslatorUtils =
    let die() = failwith "confused, will now die"

    let rec _try_assoc elem = function
      | [] -> None
      | (a,b) :: _ when elem = a -> Some b
      | _ :: tail -> _try_assoc elem tail
    
    let _list_mem elem = List.exists (fun e -> e = elem)

    let xassert cond =
      if cond then ()
      else die()
    
    let notok = B.noToken
    let er = B.Expr.Ref
    let bState = er "$s"
    let cState = C.Expr.Macro({C.ExprCommon.Bogus with Type = C.Type.MathState}, "state", [])
    let bOld x = B.Expr.Old x
    let bTrue = B.Expr.BoolLiteral true
    let bFalse = B.Expr.BoolLiteral false
    let bEq a b = B.Expr.Primitive ("==", [a; b])
    let bNeq a b = B.Expr.Primitive ("!=", [a; b])    
    let bNot a = B.Expr.Primitive ("!", [a])
    let bCall a b = B.Expr.FunctionCall (a, b)
    let bInt (n : int32) = B.IntLiteral (new bigint(n))
    let bBool v = B.BoolLiteral v
    let dont_inst p = [[bCall "$dont_instantiate" [p]]]
    
    let bContains name (expr:B.Expr) = 
      let seen = ref false
      let check = function
        | B.Expr.Ref name' when name = name' -> seen := true; None
        | _ -> None
      expr.Map check |> ignore
      !seen
    
    let afmte id msg exprs =
      (TransUtil.afmte id msg exprs).Token
    
    let afmtet tok id msg (objs:list<C.Expr>) =
      (TransUtil.forwardingToken tok None (fun () -> TransUtil.afmt id msg [ for o in objs -> o.Token.Value ])).Token

    let tpPtr = B.Type.Ref "$ptr"
    let tpPrimitive = B.Type.Ref "$primitive"
    let tpStruct = B.Type.Ref "$struct"
    let tpRecord = B.Type.Ref "$record"
    let tpPtrset = B.Type.Ref "$ptrset"
    let tpCtype = B.Type.Ref "$ctype"
    let tpField = B.Type.Ref "$field"
    let tpState = B.Type.Ref "$state"
    let tpVersion = B.Type.Ref "$version"
    let tpToken = B.Type.Ref "$token"
    let tpLabel = B.Type.Ref "$label"
    
    let bImpl a b = 
      match a, b with
        | _, B.Expr.BoolLiteral true -> bTrue
        | B.Expr.BoolLiteral true, _ -> b
        | _ -> B.Expr.Primitive ("==>", [a; b])
    
    let bOr x y =
      match (x, y) with
        | (B.Expr.BoolLiteral false, e)
        | (e, B.Expr.BoolLiteral false) -> e
        | (a, b) -> B.Expr.Primitive ("||", [a; b])
      
    let bInvImpl a b =
      if true then bImpl a b
      else
        match a, b with
          | _, B.Expr.BoolLiteral true -> bTrue
          | B.Expr.BoolLiteral true, _ -> b
          | _ -> bOr b (B.Expr.Primitive ("!", [a]))
    
    let bAnd x y =
      match (x, y) with
        | (B.Expr.BoolLiteral true, e)
        | (e, B.Expr.BoolLiteral true) -> e
        | (a, b) -> B.Expr.Primitive ("&&", [a; b])
    
    let bMultiAnd = List.fold bAnd bTrue    
    let bMultiOr = List.fold bOr bFalse
    
    let bSubst args (e:B.Expr) =
      e.Map (function 
                | B.Expr.Ref n -> _try_assoc n args
                | _ -> None)

    let max = function
      | x :: xs -> List.fold (fun acc e -> if e > acc then e else acc) x xs
      | [] -> 0
      
    type TranslationState(helper:Helper.Env) =
      let quantVarTokens = new Dict<_,_>()
      let tokenConstantNames = new Dict<_,_>()
      let tokenConstants = ref []
      let soFarAssignedLocals = ref []      
      let fileIndices = new Dict<_,_>()
      let conversionTypes = new Dict<_,_>()
      let mapTypes = new Dict<_,_>()
      let mapTypeList = glist[]
      let distinctTypes = new Dict<_,_>()
      let typeCodes = new Dict<_,_>()
      let invLabels = new Dict<_,_>()
      let invLabelConstants = ref []
      let floatLiterals = new Dict<_,_>()
      
      let addDecls lst = tokenConstants := lst @ !tokenConstants
    
      let defaultWeights = [("writes", 0); ("select", 0); ("def-field-dot", 0); ("", 1)]
    
      let weights = defaultWeights
      let weight (id:string) =
        let w =
          let rec aux = function
            | (pref, w) :: rest ->
              if id.StartsWith pref then w
              else aux rest
            | [] -> failwith "weight"
          aux weights
        if w = 1 then []
        else [B.ExprAttr ("weight", bInt w)]
      
      let castSuffix t = 
        let rec suff = function
          | B.Type.Bool -> "bool"
          | B.Type.Int -> "int"
          | B.Type.Map ([f], t) -> "map." + suff f + "." + suff t
          | B.Type.Ref n -> n.Replace ("$", "")
          | t -> helper.Panic ("wrong type in castSuffix " + t.ToString())
        let suff = suff t
        match suff with
          // predefined in the prelude
          | "record"
          | "version"
          | "ptr"
          | "bool"
          | "int"
          | "ptrset" -> suff
          // possible need to generate conversion function
          | _ ->
            if not (conversionTypes.ContainsKey suff) then
              conversionTypes.Add (suff, true)
              let toIntName = "$" + suff + "_to_int"
              let toInt = B.Decl.Function (B.Type.Int, [], toIntName, [("x", t)])
              let fromIntName = "$int_to_" + suff
              let fromInt = B.Decl.Function (t, [], fromIntName, [("x", B.Type.Int)])
              let both = bCall fromIntName [bCall toIntName [er "#x"]]
              let ax1 = B.Decl.Axiom (B.Expr.Forall (Token.NoToken, [("#x", t)], [], weight "conversion", bEq (er "#x") both))
              addDecls [toInt; fromInt; ax1]
            suff
            
      let rec toTypeId' translateArrayAsPtr t =
      
        let internalizeType t bt =      
          let rec isDerivedFromTypeVar = function
            | C.Type.TypeVar _ -> true
            | C.Ptr(t)
            | C.Array(t, _) -> isDerivedFromTypeVar t
            | C.Map(from, _to) -> List.exists isDerivedFromTypeVar [from; _to]
            | _ -> false
          if not (distinctTypes.ContainsKey t) && not (isDerivedFromTypeVar t) then
            distinctTypes.Add (t, distinctTypes.Count + 1)
            let cd = { Unique = true 
                       Name = "#distTp" + distinctTypes.Count.ToString() 
                       Type = tpCtype } : B.ConstData
            let eq = bEq (er cd.Name) bt
            addDecls [B.Decl.Const cd; B.Decl.Axiom eq]
          bt
        match t with
          | C.Type.Bool -> er "^^bool"
          | C.Type.Integer kind -> er ("^^" + C.Type.IntSuffix kind)
          | C.Type.MathInteger -> er "^^mathint"
          | C.Type.Primitive kind -> er ("^^" + C.Type.PrimSuffix kind) 
          | C.Type.Void -> er "^^void"
          | C.Type.PhysPtr tp ->
            internalizeType t (bCall "$ptr_to" [toTypeId' false tp])
          | C.Type.SpecPtr tp ->
            internalizeType t (bCall "$spec_ptr_to" [toTypeId' false tp])
          | C.Type.ObjectT -> toTypeId' false (C.Type.PhysPtr C.Void)
          | C.Type.Array (tp, _) when translateArrayAsPtr ->
            internalizeType (C.Type.PhysPtr tp) (bCall "$ptr_to" [toTypeId' translateArrayAsPtr tp])
          | C.Type.Array (tp, sz) ->
            internalizeType t (bCall "$array" [toTypeId' translateArrayAsPtr tp; B.Expr.IntLiteral(new bigint(sz))])
            //bCall "$array" [toTypeId tp; bInt sz]
          | C.Type.Map (range, C.Type.Ref({ Kind = C.Union|C.Struct})) -> toTypeId' translateArrayAsPtr (C.Type.Map(range, C.Type.MathStruct))
          | C.Type.Map (range, dom) -> 
            internalizeType t (bCall "$map_t" [toTypeId' false range; toTypeId' false dom])
          | C.Type.Ref { Name = n; Kind = (C.MathType|C.Record|C.FunctDecl _) } -> er ("^$#" + n)
          | C.Type.Ref td -> er ("^" + td.Name)
          | C.Type.TypeIdT -> er "^$#typeid_t"
          | C.Type.Claim -> er "^^claim"
          | C.Type.TypeVar({Name = id}) -> er ("^^TV#" + id)
          | C.Type.Volatile(t) -> 
            helper.Panic("volatile type modifier survived")
            toTypeId'  false t
            
      let toTypeId = toTypeId' false
      
      let getTypeCode t =
        match typeCodes.TryGetValue t with
          | true, n -> n
          | _ ->
            // needed here, as it might add stuff to distinctTypes
            let typeId = toTypeId t
            match distinctTypes.TryGetValue t with
              | true, n ->
                typeCodes.[t] <- n
                n
              | _ ->
                let n = -(typeCodes.Count + 1)
                typeCodes.[t] <- n
                addDecls [B.Decl.Axiom (bCall "$type_code_is" [bInt (- n); typeId])]
                n
      
      let rec typeIdToName = function
        | B.Expr.Ref s -> s
        | B.Expr.FunctionCall (f, a) -> f + ".." + String.concat "." (List.map typeIdToName a)
        | t -> helper.Panic ("cannot compute name for type expression " + t.ToString()); ""              

      let rec trType (t:C.Type) : B.Type = 
        match t with
          | C.Type.MathInteger
          | C.Type.Integer _ 
          | C.Type.SpecPtr _
          | C.Type.PhysPtr _ -> B.Type.Int
          | C.Type.Primitive _ -> tpPrimitive
          | C.Type.Bool -> B.Type.Bool
          | C.Type.ObjectT -> tpPtr
          | C.Type.TypeIdT -> tpCtype
          | C.Type.Map (t1, C.Type.Ref({Kind = C.Union|C.Struct})) -> 
            trType (C.Type.Map (t1, C.Type.MathStruct))
          | C.Type.Map (t1, t2) ->
            let bt1 = trType t1
            let bt2 = trType t2
            let mapName = typeIdToName (toTypeId t)
            let mapType = B.Type.Ref mapName
            if not (mapTypes.ContainsKey mapName) then
              mapTypeList.Add t
              mapTypes.Add (mapName, true)
            mapType
          | C.Type.Ref ({ Kind = C.Record }) -> B.Type.Ref "$record"
          | C.Type.Ref ({ Name = n; Kind = (C.MathType|C.FunctDecl _) }) ->
            match n with
              | "ptrset" -> tpPtrset
              | "struct" -> tpStruct
              | "state_t" -> tpState
              | _ -> B.Type.Ref ("$#" + n)
          | C.Type.Volatile _
          | C.Type.Claim
          | C.Type.Array _ 
          | C.Type.Void
          | C.Type.TypeVar _
          | C.Type.Ref _ ->
            helper.Panic ("wrong type survived: " + t.ToString())


      member this.Helper = helper
      
      member this.RegisterToken name =
        if not (tokenConstantNames.ContainsKey name) then 
          tokenConstantNames.Add (name, true)
          let constdata = { Name = name; Type = tpToken; Unique = true } : B.ConstData
          addDecls [B.Decl.Const constdata]
      
      member this.GetTokenConst tok =
        let name = "#tok" + this.TokSuffix tok
        this.RegisterToken name
        name
      
      member this.GetFloatConst (f : float) =
        match floatLiterals.TryGetValue f with
          | true, e -> e
          | false, _ ->
            let floatName = "floatLiteral#" + helper.UniqueId().ToString()
            let t = B.Type.Ref "$primitive"
            let decl = B.Const( {Unique = true; Name = floatName; Type = t } )
            let result = B.Expr.Ref floatName
            floatLiterals.Add(f, result)
            addDecls [decl]
            result
            
      member this.CastToInt t e =
        match t with
          | B.Type.Int -> e
          | _ -> bCall ("$" + castSuffix t + "_to_int") [e]
      
      member this.CastFromInt t e =
        match t with
          | B.Type.Int -> e
          | _ -> bCall ("$int_to_" + castSuffix t) [e]
     
      member this.TokSuffix (t:Token) = 
        let fidx =
          if t.Filename = null || t.Filename = "no_file" then 0
          else
            match fileIndices.TryGetValue t.Filename with
              | true, idx -> idx
              | _ ->
                let idx = fileIndices.Count + 1
                fileIndices.Add (t.Filename, idx)
                let sb = new System.Text.StringBuilder()
                for c in t.Filename do
                  if System.Char.IsLetterOrDigit c || c = '.' || c = '_' then
                    sb.Append c |> ignore
                  else
                    (sb.Append '?').Append(System.String.Format("{0:X2}", (int)c)) |> ignore
                let name = "#file^" + sb.ToString()
                let constdata = { Name = name; Type = tpToken; Unique = true } : B.ConstData
                let axiom = B.Decl.Axiom (bCall "$file_name_is" [bInt idx; er name])
                addDecls [B.Decl.Const constdata; axiom]
                idx
        System.String.Format ("${0}^{1}.{2}", fidx, t.Line, t.Column)
    
      member this.GetTypeSuffix t = 
        let tc = getTypeCode t
        if tc < 0 then "#tc" + (-tc).ToString()
        else "#dt" + tc.ToString()      
      
      member this.ToTypeId = toTypeId' false     
      member this.ToTypeIdArraysAsPtrs = toTypeId' true
      member this.AddDecls = addDecls
      member this.TrType = trType
      member this.TypeIdToName = typeIdToName
      member this.QuantVarTokens = quantVarTokens
      member this.SoFarAssignedLocals = !soFarAssignedLocals
      
      member this.VarName (v:C.Variable) =
        if v.Name.IndexOf '#' >= 0 || v.Name.IndexOf '.' >= 0 then
          if v.Kind = C.VarKind.ConstGlobal then
            "G#" + v.Name + this.GetTypeSuffix v.Type
          else
            v.Name
        else
          match v.Kind with
            | C.VarKind.QuantBound -> "Q#" + v.Name + (this.TokSuffix quantVarTokens.[v]) + this.GetTypeSuffix v.Type
            | C.VarKind.ConstGlobal -> "G#" + v.Name + this.GetTypeSuffix v.Type
            | C.VarKind.SpecParameter -> "SP#" + v.Name
            | C.VarKind.OutParameter -> "OP#" + v.Name
            | C.VarKind.Parameter -> "P#" + v.Name
            | C.VarKind.SpecLocal -> "SL#" + v.Name
            | C.VarKind.Local -> "L#" + v.Name
            | C.VarKind.SpecGlobal
            | C.VarKind.Global -> die()
      
      member this.VarRef n = er (this.VarName n)
      
      member this.AddType t e = 
        match t with
          | C.Type.SpecPtr t
          | C.Type.PhysPtr t ->
            bCall "$ptr" [toTypeId t; e]
          | _ -> e
      
      // TODO: this shouldn't be here
      member this.AssumeLocalIs tok (l:C.Variable) =
        let pos = this.GetTokenConst tok
        let name = "#loc." + l.Name
        if not (tokenConstantNames.ContainsKey name) then
          soFarAssignedLocals := l :: !soFarAssignedLocals
        this.RegisterToken name
        let valIs suff v = bCall ("$local_value_is" + suff) [bState; er pos; er name; v; toTypeId l.Type]
        let cond =
          match l.Type with
            | C.Ptr _ -> 
              let v' = this.AddType l.Type (this.VarRef l)
              bAnd (valIs "" (bCall "$ptr_to_int" [v'])) (valIs "_ptr" v')
            | _ -> valIs "" (this.CastToInt (trType l.Type) (this.VarRef l))
        B.Stmt.Assume cond
        
      member this.TrInvLabel (lbl:string) =
        let result = "l#" + lbl;
        if not (invLabels.ContainsKey(result)) then 
          invLabels.Add(result, true)
          let constdata = { Name = result; Type = tpLabel; Unique = true } : B.ConstData
          invLabelConstants := B.Decl.Const constdata :: !invLabelConstants
        result
        
      member this.TypeDepth = function
        | C.Type.Ref td ->
          1 + max (List.map (fun (f:C.Field) -> this.TypeDepth f.Type) td.Fields)
        | C.Type.Array (t, _) -> this.TypeDepth t
        | C.Type.Claim -> 1
        | t ->
          if t.IsComposite then
            helper.Oops (C.bogusToken, "strange type " + t.ToString())
          1
      
      member this.FlushDecls mapEq =
        while mapTypeList.Count > 0 do
          let types = mapTypeList |> Seq.toList
          mapTypeList.Clear()
          addDecls (List.map mapEq types |> List.concat)
        !invLabelConstants @ !tokenConstants
      
      member this.Weight = weight
      
      member this.NewFunction() = soFarAssignedLocals := []        
