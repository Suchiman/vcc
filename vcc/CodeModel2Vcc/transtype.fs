//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
 open Microsoft.FSharp.Math
 open Microsoft.Research.Vcc
 open Microsoft.Research.Vcc.Util
 open Microsoft.Research.Vcc.TransUtil
 open Microsoft.Research.Vcc.CAST
 
 module TransType =
 
  let setEqualityKind (td : TypeDecl) eqKind =
    let rec markTypeDecl eqKind (td : TypeDecl) =
      match td.GenerateEquality, eqKind with
        | (NoEq | ShallowEq), DeepEq  -> 
          td.GenerateEquality <- eqKind
          List.iter (markField eqKind) td.Fields
        | (NoEq | ShallowEq), ShallowEq when td.Kind = TypeKind.Union  -> 
          td.GenerateEquality <- eqKind
          List.iter (markField eqKind) td.Fields
        | DeepEq , ShallowEq -> ()
        | _ -> 
          assert (eqKind <> NoEq)
          td.GenerateEquality <- eqKind
    and markField eqKind (f : Field) =
      match f.Type with
      | Type.Ref td -> markTypeDecl eqKind td
      | _ -> ()
    markTypeDecl eqKind td

  let isRecord (td : TypeDecl) = hasBoolAttr "record" td.CustomAttr
  
  // ============================================================================================================
  
  
  let init (helper:Helper.Env) =
    
    // ============================================================================================================
    
    let handleFunctionPointers decls =
      let addDecls = ref []
      let checks = new Dict<_,_>()
      let fnids = new Dict<_,_>()
      let addThis (fnptr:Function) =
        match fnptr.Parameters with
          | { Name = "$this" } :: _ -> ()
          | parms ->
            let th = { Type = Ptr Void; Name = "$this"; Kind = VarKind.Parameter } : Variable
            fnptr.Parameters <- th :: parms
                    
      let repl ctx self = function
        | Expr.Macro (comm, "&", [Expr.Macro (_, "get_fnptr", [Expr.Call (_, called, _, [])])]) ->
          if not (fnids.ContainsKey called.Name) then
            fnids.[called.Name] <- fnids.Count
          Some (Expr.Macro ({ comm with Type = Ptr Void }, "_vcc_get_fnptr", [mkInt fnids.[called.Name]; typeExpr Void]))
          
        | Expr.Cast ({ Token = tok; Type = (Ptr (Type.Ref { Kind = FunctDecl fnptr } as fnptrref)) } as comm, _, 
                     Expr.Macro (_, "&", [Expr.Macro (_, "get_fnptr", [Expr.Call (_, called, [], [])])])) ->
          if not (fnids.ContainsKey called.Name) then
            fnids.[called.Name] <- fnids.Count
          let neqPar (a:Variable) (b:Variable) = a.Type <> b.Type
          addThis fnptr
          let fnptrparms = fnptr.Parameters.Tail
          if ctx.IsPure then
            helper.Error (tok, 9602, "function pointer casts are only allowed in non-pure context", None)
            None
          elif List.length fnptrparms <> List.length called.Parameters then
            helper.Error (tok, 9603, "function pointers: different number of parameters", Some(called.Token))
            None
          elif List.exists2 neqPar fnptrparms called.Parameters then
            helper.Error (tok, 9604, "function pointers: different types of parameters", Some(called.Token))
            None
          elif fnptr.RetType <> called.RetType then
            helper.Error (tok, 9605, "function pointers: different return types", Some(called.Token))
            None
          else
            let header = { fnptr with Name = "$fnptr_" + called.Name + "_to_" + fnptr.Name; Token = tok }
            if not (checks.ContainsKey header.Name) then
              checks.Add (header.Name, checks.Count)
              let call = Expr.Call ({ comm with Type = called.RetType },
                                    called, 
                                    [], 
                                    List.map mkRef fnptrparms)
              let body = 
                if called.RetType = Void then call 
                else Expr.Return ({ call.Common with Type = Void }, Some call)
              let def = FunctionDecl { header with Body = Some body }
              addDecls := def :: !addDecls
            Some (Expr.Macro (comm, "_vcc_get_fnptr", [mkInt fnids.[called.Name]; typeExpr fnptrref]))
        | Expr.Macro (c, "fnptr_call", fn :: args) ->
          match fn.Type with
            | FunctionPtr decl ->
              addThis decl
              if ctx.IsPure then
                Some (Expr.Call (c, decl, [], fn :: args))
              else
                let (init, tmp) = cache helper "ptrcall" fn VarKind.Local
                let assertions = init @ [
                                   propAssert 8504 "{0} is valid function pointer" "_vcc_typed2" tmp;
                                   Expr.Call (c, decl, [], tmp :: args)]
                Some (self (Expr.MkBlock assertions))
            | _ -> die()
        | _ -> None
      
      let decls = deepMapExpressionsCtx repl decls
      decls @ !addDecls
      
    // ============================================================================================================
    
    let liftGroups decls =   
    
      let groupTypes = new Dict<TypeDecl * string, TypeDecl>()
      let fieldTransform = new Dict<Field, TypeDecl * Field * Field>()
      let groupParent = new Dict<TypeDecl, TypeDecl * Field>()
      let groupAxioms = ref []
           
      let rec getGroupNameFromAttrs = function
        | [] -> None
        | InGroupDeclAttr(name) :: _ -> 
          Some name
        | GroupDeclAttr(name) :: _ -> 
          Some name
        | _ :: attrs -> getGroupNameFromAttrs attrs
        
      let removeGroupDeclAttrs =
        let isNotGroupDeclAttr = function
          | InGroupDeclAttr _ -> false
          | GroupDeclAttr _ -> false
          | _ -> true
        List.filter isNotGroupDeclAttr

      let makeTypeNameForGroup parentName groupName = parentName + "##" + groupName

      let findGroupTypes decls =
        let addToDictIfGroupType = function
          | Top.TypeDecl td ->
            match getGroupNameFromAttrs td.CustomAttr with
            | Some groupName ->
              match td.Parent with
              | Some parent -> 
                td.Name <- makeTypeNameForGroup parent.Name groupName
                td.SizeOf <- parent.SizeOf
                match groupTypes.TryGetValue ((parent, groupName)) with
                  | true, td' ->
                    let msg = "'" + groupName + "' : group name redefinition"
                    helper.Error(td.Token, 9678, msg, Some(td'.Token))
                  | _ -> groupTypes.[(parent, groupName)] <- td
              | None -> 
                helper.Oops(td.Token, "Encountered group type without parent.")
                helper.Die()
            | None -> ()
          | _ -> ()
          
        List.iter addToDictIfGroupType decls
           
      let genGroupAxiom (group:TypeDecl) (parent:TypeDecl) (groupField : Field) =
        let t = Type.Ptr(Type.Ref(group))
        let pt = Type.Ptr(Type.Ref(parent))
        let v = { Name = "ptr"; Type = t; Kind = VarKind.QuantBound } : Variable
        let ptr = Expr.Ref({bogusEC with Type = t}, v)
        let rhs = Expr.Dot({bogusEC with Type = t}, Expr.Cast({bogusEC with Type = pt}, CheckedStatus.Unchecked, ptr), groupField)
        let eq = Expr.Prim(boolBogusEC(), Op.Op("==", CheckedStatus.Unchecked), [ptr; rhs])
        let forall = Expr.Quant(boolBogusEC(), { Kind = QuantKind.Forall
                                                 Variables = [v]
                                                 Triggers = [[ptr]]
                                                 Condition = None
                                                 Body = eq })
        Top.GeneratedAxiom(forall, Top.TypeDecl(parent))                                                             
        
           
      let makeTypeDeclsForGroups (parent:TypeDecl) =
        let typeDecls = new Dict<string, TypeDecl * Field>()
        let findOrCreateTypeForGroup groupName =
          match typeDecls.TryGetValue groupName with
          | (true, (td, field)) -> (false, td, field) // type exists, no new field
          | (false, _) ->
            // create type for the previously unencountered group and create a field for it in the parent
            match groupTypes.TryGetValue((parent, groupName)) with
            | (true, td) ->
              let newField = { Name = groupName
                               Token = parent.Token
                               Type = Type.Ref td
                               Parent = parent
                               IsSpec = false
                               IsVolatile = false 
                               Offset = Normal 0
                               CustomAttr = [] }
              typeDecls.[groupName] <- (td, newField)
              groupParent.[td] <- (parent, newField)
              groupAxioms := genGroupAxiom td parent newField :: !groupAxioms
              (true, td, newField)
            | _ -> 
              helper.Oops(parent.Token, "Unknown group \"" + groupName + "\" in field declaration.")
              helper.Die()

        let rec processFields = function
          | [] -> []
          | (field : Field) :: fields' ->
            match getGroupNameFromAttrs field.CustomAttr with
            | None -> field :: processFields fields'
            | Some groupName ->
              let (isNewField, groupTd, groupField) = findOrCreateTypeForGroup groupName 
              let nestedField = { field with Parent = groupTd; CustomAttr = removeGroupDeclAttrs field.CustomAttr }
              groupTd.Fields <- groupTd.Fields @ [ nestedField ]
              fieldTransform.[field] <- (groupTd, groupField, nestedField)
              if (isNewField) then
                groupField :: processFields fields'
              else
                processFields fields'
        
        let rec processInvariants invs =
          let retypeThis td self = function
            // the 'this' will have type of the group in the invariant defintion
            // however field accesses are done with the type of parent in mind
            // therefore for field accesses we cast back
            | Expr.Dot (c1, (Expr.Macro (c2, "this", []) as th), f) ->
              Some (Expr.Dot (c1, Expr.Cast (c2, Processed, self th), f))
            | Expr.Macro(c, "this", []) ->
              Some (Expr.Macro( { c with Type = Type.Ptr(Type.Ref(td)) }, "this", []))
            | _ -> None
           
     
          match invs with 
          | [] -> []
          | Expr.Macro(_, "labeled_invariant", [_; Expr.Macro(_, "group_invariant", [Expr.Macro(_, groupName,_); groupInvariant])]) :: invs'
          | Expr.Macro(_, "group_invariant", [Expr.Macro(_, groupName,_); groupInvariant]) :: invs' ->
            match typeDecls.TryGetValue(groupName) with
            | (true, (td, _)) -> 
              td.Invariants <-  groupInvariant.SelfMap (retypeThis td) :: td.Invariants
              processInvariants invs'
            | (false, _) ->
              helper.Oops(groupInvariant.Common.Token, "Unknown group \"" + groupName + "\" in invariant declaration.")
              helper.Die()
              
          | inv :: invs' -> inv :: processInvariants invs'
        
        parent.Fields <- processFields parent.Fields
        parent.Invariants <- processInvariants parent.Invariants
              
      let transformFields self = function
        | Dot (c, e, f) ->
          match fieldTransform.TryGetValue f with
          | (false, _) -> None
          | (true, (groupTd, groupField, fieldField)) ->
            if e.Type.IsPtrTo(Type.Ref groupTd) then
              Some (Dot (c, self e, fieldField))
            else
              match self e with
                // Simplify group field access in invariants, makes matching in @approves(...) work.
                | Cast (_, _, (Macro (_, "this", []) as th)) when th.Type.IsPtrTo (Type.Ref groupTd) ->
                  Some (Dot (c, th, fieldField))
                | se ->
                  Some (Dot (c, Expr.MkDot(c, se, groupField), fieldField))
        // rewrite (user supplied) casts to group types into field accesses
        | Cast (c, _, e) ->
          match c.Type, e.Type with
            | Ptr (Type.Ref gr), Ptr (Type.Ref par) ->
              match groupParent.TryGetValue gr with
                | true, (tp, fld) when tp = par ->
                  Some (self (Dot (c, e, fld)))
                | _ -> None
            | _ -> None
        | _ -> None 
                 
      let processDecls decls =
        let processDecl = function
          | Top.TypeDecl td -> makeTypeDeclsForGroups td
          | _ -> ()
        List.iter processDecl decls

      findGroupTypes decls
      processDecls decls
      deepMapExpressions transformFields (decls @ !groupAxioms)
    
    
    
    // ============================================================================================================
    
         
    (*
        mark those types that have been introduced as as nested anonymous types, e.g., as in
        
        struct S {
          int a;
          int b;
          struct {  <- this is the type that is marked as such
            int c;
            int d;
          };
        };
        
        and introduce extra Dot expressions that normalize the access to the type's members
        
    *)     
    let markNestedAnonymousTypes decls =
      for d in decls do
        match d with
          | Top.TypeDecl td ->
            for f in td.Fields do
              if f.Name = "" then
                match f.Type with
                  | Type.Ref td' when td'.Name.StartsWith (td.Name + ".") ->
                    td'.IsNestedAnon <- true
                  | _ -> ()
          | _ -> ()

      let findFieldForType (td:TypeDecl) =
        match td.Parent with
          | Some parent -> List.find (fun (fld:Field) -> fld.Type = Type.Ref(td)) parent.Fields
          | None -> die()

      let rec addDotForAnonField  = function
        | Dot (c, e, f) when f.Parent.IsNestedAnon -> 
          let fld = findFieldForType f.Parent
          Dot(c, addDotForAnonField(Expr.MkDot(c, e, fld)), f)
        | expr -> expr
     
      let normalizeDots self = function
        | Dot (c, Macro (_, "&", [Deref (_, e)]), f) -> Some (self (Dot (c, e, f)))
        | Dot (c, e, f) when f.Parent.IsNestedAnon -> Some (addDotForAnonField (Dot(c, self e, f)))
        | _ -> None
          
      deepMapExpressions normalizeDots decls
       
    // ============================================================================================================
    
    let rec bv_extrAndPad (c:ExprCommon) (e:Expr) fromBit toBit =

      let sz =
        match e.Type.Deref with
          | Integer IntKind.UInt64
          | Integer IntKind.Int64 -> 64
          | Integer IntKind.UInt32
          | Integer IntKind.Int32 -> 32
          | Integer IntKind.Int16
          | Integer IntKind.UInt16 -> 16
          | Integer IntKind.Int8 
          | Integer IntKind.UInt8 -> 8
          | _ -> die() // we explicitly don't want anything else here

      Expr.Macro (c, "pullout_bv_extract_" + (if c.Type.Deref.IsSignedInteger then "signed" else "unsigned"), 
                  [e; mkInt sz; mkInt fromBit; mkInt toBit])

    let flattenUnions decls =

      let fieldsToReplace = new Dict<Field, Field>()
      let processedTypes = new Dict<TypeDecl, bool>()
  
      let tryFindBackingMember (td:TypeDecl) = 
        let tdIsRecord = isRecord td
        let isValidField (fld : Field) =
          let isValidType = 
            let rec isValidType' allowArray = function
            | Integer _ | Ptr _ -> true
            | Array(t, _) when allowArray -> isValidType' false t
            | Type.Ref(td) -> td.Fields.Length = 1 && isValidType' false  td.Fields.Head.Type
            | _ -> false
            isValidType' true
          (tdIsRecord || not fld.IsSpec) && fld.Type.SizeOf = td.SizeOf && isValidType fld.Type
        match List.tryFind (fun (fld:Field) -> _list_mem (VccAttr("backing_member", "")) fld.CustomAttr) (td.Fields) with
        | Some fld ->
           if isValidField fld then 
             Some fld 
           else 
             helper.Error (fld.Token, 9633, "'" + fld.Name + "' cannot be used as a backing member for type '" + td.Name + "'", Some(td.Token))
             None
        | _ -> None
    
      let backingField (fld : Field) =
        let rec getPrimitiveType = function
          | Integer _ | Ptr _ as t -> t
          | Type.Ref(td) -> getPrimitiveType td.Fields.Head.Type
          | _ -> die()
        match fld.Type with
          | Integer _ | Ptr _ | Array(Integer _, _) | Array(Ptr _, _) -> fld
          | Type.Ref(_) -> { fld with Name = fld.Name + "#bm"; Type = getPrimitiveType fld.Type }
          | Array(Type.Ref(_) as t, n) -> { fld with Name = fld.Name + "#bm"; Type = Array(getPrimitiveType(t), n) }
          | _ -> die()
                     
      let rec hasVolatileInExtent (fld : Field) =
        if fld.IsVolatile then true else hasVolatileExtentForType fld.Type
      and hasVolatileExtentForType = function
          | Type.Ref(td) -> List.exists hasVolatileInExtent td.Fields
          | Array(t,_) -> hasVolatileExtentForType t
          | _ -> false
    
      let rec processTypeDecl = function
        | TypeDecl(td) when processedTypes.ContainsKey(td) -> ()
        | TypeDecl(td) when td.Kind = TypeKind.Union ->
          let rec processType = function
            | Type.Ref(td) -> processTypeDecl (TypeDecl(td))
            | Type.Array(t, _) -> processType t
            | _ -> ()
          processedTypes.Add(td, true)
          List.iter (fun (fld : Field) -> processType (fld.Type)) td.Fields
          match tryFindBackingMember td with
            | Some fld -> 
              let bf = { backingField fld with IsVolatile = List.exists hasVolatileInExtent td.Fields }        
              let tdIsRecord = isRecord td
              let addOtherFlds (f : Field) =
                if f = bf || (f.IsSpec && not tdIsRecord) then () else fieldsToReplace.Add(f, bf)
              List.iter addOtherFlds (td.Fields)
              td.Fields <- bf :: if tdIsRecord then [] else List.filter (fun (f : Field) -> f.IsSpec) td.Fields
            | None ->()
        | _ -> ()
    
      let rec genDotPrime t1 t2 expr =
        match t1, t2 with
          | (Integer _ | Type.Ref _ | Ptr _), (Integer _ | Ptr _) -> Macro({bogusEC with Type = Ptr(t1)}, "Dot'", [expr; mkInt 0])
          | t1', Array(t2', _) -> genDotPrime t1' t2' expr
          | Array(t1',_), t2' -> genDotPrime t1' t2' expr
          | _, _ -> die()
    
      let replExpr self = function
        | Dot (c, Macro (_, "&", [Deref (_, e)]), f) -> Some (self (Dot (c, e, f)))
        | Index(c, Macro (_, "&", [Deref (_, e)]), i) -> Some(self (Index(c, e, i)))
        | Dot (c, e, f) as expr ->
          match fieldsToReplace.TryGetValue(f) with
            | true, newFld -> 
              Some(self (genDotPrime (f.Type) (newFld.Type) (Dot({c with Type = Ptr(newFld.Type)}, (self e), newFld)))) 
              // do NOT call MkDot here because we need to preserve the information that the new field is of array type
              // todo: make this go away
            | false, _ -> None          
        | _ -> None

      let rippleOutDotPrime self = function
        | Dot(c, e, f) ->
          match self e with
            | Macro(_, "Dot'", [expr; IntLiteral(_, offset)]) ->
              Some(Macro({bogusEC with Type = Ptr(f.Type)}, "Dot'", [expr; mkInt ((int)offset + f.ByteOffset)]))
            | se -> Some(Dot(c, se, f))
        | Index(c, e, idx) ->
          match self e with
            | Macro(_, "Dot'", [expr; IntLiteral(_, offset)]) ->
              let reportError() = helper.Error(c.Token, 9654, "Expression is invalid due to union flattening. Flattening of arrays requires the backing member to be of array type with same element size and alignment.", None)
              if not expr.Type.Deref._IsArray then 
                reportError()
                Some(bogusExpr)
              else
                let elemSize = c.Type.Deref.SizeOf
                let bmElemType = match expr.Type.Deref with Array(t, _) -> t | _ -> die()
                if ((int)offset % elemSize <> 0) || (elemSize  <> bmElemType.SizeOf) then
                  reportError()
                  Some(bogusExpr)
                else
                  let idx' = if offset = bigint.Zero then idx else Prim(idx.Common, Op("+", Checked), [mkInt((int)offset); idx])
                  Some(self(Macro({bogusEC with Type = c.Type}, "Dot'", [Index({c with Type = Ptr(bmElemType)}, expr, idx'); mkInt 0])))
            | e' -> Some(Index(c, e', self idx))
        | _ -> None

      let removeDotPrime self = function
        | Macro(c, "Dot'", [expr; IntLiteral(_, offset)]) ->
          match c.Type.Deref, expr.Type.Deref with
            | ct, _ when ct.IsComposite ->
              Some(expr)
            | _, Array(elemType, _) ->  
              Some(self(Macro(c, "Dot'", [Index({expr.Common with Type = Ptr(elemType)}, expr, mkInt ((int)offset / elemType.SizeOf)); mkInt ((int)offset % elemType.SizeOf)])))
            | (Ptr(_) as ct), Ptr(_) -> Some(Macro({c with Type = ct}, "pullout_cast", [self expr]))
            | Integer(_) as ct, Ptr(_) ->
              let targetTypeCode, conversionFunction = if ct.IsSignedInteger then IntKind.Int64, "pullout__vcc_ptr_to_i8" else IntKind.UInt64, "pullout__vcc_ptr_to_u8"
              Some(self(Macro(c, "Dot'", [Macro({bogusEC with Type = Ptr(Integer(targetTypeCode))}, conversionFunction, [expr]); mkInt((int)offset)])))
            | Ptr(_) as ct, (Integer(_) as et) ->
              let conversionFunction = if et.IsSignedInteger then "pullout__vcc_i8_to_ptr" else "pullout__vcc_u8_to_ptr"
              Some(Macro({c with Type = ct}, "pullout_cast", [Macro({bogusEC with Type = Ptr(Void)}, conversionFunction, [self expr])]))
            | Integer(_) as ct, Integer(_) as et ->
              Some(bv_extrAndPad {expr.Common with Type = ct} (self expr) ((int)offset * 8) (((int)offset + ct.SizeOf) * 8))
            | _ -> die()
        | Deref(c,e) ->
          match (self e) with
            | Macro(bvec, name, e'::args) when name.StartsWith("pullout_") ->
              Some(self (Macro(bvec, name, Deref({c with Type = e'.Type.Deref}, e')::args)))
            | e' -> Some(Deref(c, e'))
        | _ -> None

      let removePullOut self = function
        | Macro(bvec, "pullout_cast", [e']) -> Some(Cast(bvec, Unchecked, self e'))
        | Macro(c, name, args) when name.StartsWith("pullout_") -> Some(Macro(c, name.Substring(8), List.map self args))
        | _ -> None
              
      let coalesceBvExtractsAndUncheckedCoversions self = 
        let getSuffix = function 
          | Integer k -> Type.IntSuffix(k)
          | _ -> die()
        function
          | Expr.Macro (c, ("bv_extract_signed" | "bv_extract_unsigned"), 
                        [e; IntLiteral(_, sz); IntLiteral(_, z); IntLiteral(_, sz') ]) when z = zero && sz = sz' -> 
              let unchecked (expr :Expr) =
                match expr.Type.Deref with 
                  | Integer k -> Macro({expr.Common with Type = Ptr(Integer(Type.SwitchSignedness k))}, "unchecked_" + Type.IntSuffix(Type.SwitchSignedness k), [expr])
                  | _ -> die()                      
              let e' = if c.Type.Deref.IsSignedInteger = e.Type.Deref.IsSignedInteger then e else unchecked e             
              Some(self e')
          | Macro(c, outer, [Macro(_, inner, [expr])]) when outer.StartsWith("unchecked_") && inner.StartsWith("unchecked_") ->
            Some(self(Macro(c, outer, [expr])))
          | Macro(c, outer, [expr]) when outer.StartsWith("unchecked_") && outer.Substring(10) = getSuffix (expr.Type) ->
            Some(self expr)
          | Macro(c, outer, [Macro(_, inner, [expr; innerSize; innerStart; _]); _; outerStart; outerEnd]) when outer.StartsWith("bv_extract_") && inner.StartsWith("bv_extract_") ->
            let toInt = function
              | IntLiteral(_, i) -> (int)i
              | _ -> die()
            Some(self(Macro(c, outer, [self expr; innerSize; mkInt ((toInt innerStart) + (toInt outerStart)); mkInt ((toInt innerStart) + (toInt outerEnd))])))
          | Expr.Macro (c, (("bv_extract_signed" | "bv_extract_unsigned") as extractName), e :: args)  -> 
            match self e with
              | Expr.Macro(c, name, [e']) when name.StartsWith("unchecked_") ->
                Some(self (Expr.Macro(c, extractName, e' :: args)))
              | e' -> Some(Expr.Macro(c, extractName, e' :: args))
                
          
          | _ -> None
      
      decls |> List.iter processTypeDecl
      decls |> deepMapExpressions replExpr 
            |> deepMapExpressions rippleOutDotPrime 
            |> deepMapExpressions removeDotPrime 
            |> deepMapExpressions removePullOut
            //|> deepMapExpressions removeTrivialBvOps
            |> deepMapExpressions coalesceBvExtractsAndUncheckedCoversions
    
    // ============================================================================================================
    
    let removeBitfields decls =
    
      let fieldsToReplace = new Dict<Field, Field>()
    
      let hasBitFields (td : TypeDecl) =
        let isBitField = function
          | BitField(_,_,_) -> true
          | _ -> false
        List.exists ((fun f -> f.Offset) >> isBitField) (td.Fields)

      let fieldsForBitfields td = 
        let addFieldSubst oldFld = function
          | None -> die()
          | Some newFld -> fieldsToReplace.Add(oldFld, newFld)
        let rec fieldsForBitfields' currentOffset lastNewField = function
          | [] -> []
          | ({ Offset = Normal(n) } as fld) :: flds -> fld :: (fieldsForBitfields' (n+fld.Type.SizeOf) None flds)
          | ({ Offset = BitField(byteOffset, bitOffset, bitSize) } as fld) :: flds ->
              if (currentOffset * 8 >= (byteOffset * 8 + bitOffset + bitSize)) then
                addFieldSubst fld lastNewField
                fieldsForBitfields' currentOffset lastNewField flds
              else
                let newFld = { fld with Name = "bitfield#" + byteOffset.ToString(); Offset = Normal(byteOffset) }
                addFieldSubst fld (Some newFld)
                newFld :: (fieldsForBitfields' (currentOffset + newFld.Type.SizeOf) (Some newFld) flds)
        fieldsForBitfields' 0 None td.Fields

      let fieldsForBitfieldsInUnion td =
        let f = function
          | ({ Offset = BitField(byteOffset, bitOffset, _) } as fld) ->
            if byteOffset <> 0 || bitOffset <> 0 then helper.Oops(fld.Token, "bitfield with non-zero byte- or bit-offset in union")
            let newFld = { fld with Offset = Normal(0) }
            fieldsToReplace.Add(fld, newFld)
            newFld
          | fld -> fld
        List.map f (td.Fields)
          

      let processTypes = function
         | TypeDecl td when td.Kind = TypeKind.Struct && hasBitFields td -> td.Fields <- fieldsForBitfields td
         | TypeDecl td when td.Kind = TypeKind.Union && hasBitFields td -> td.Fields <- fieldsForBitfieldsInUnion td
         | _ -> ()

      let toBitFieldOffset size = function
        | Normal n -> BitField(n, 0, size*8)
        | bf -> bf

      let replExpr self = function
        | Dot (c, Macro (_, "&", [Deref (_, e)]), f) -> Some (self (Dot (c, e, f)))
        | Index(c, Macro (_, "&", [Deref (_, e)]), i) -> Some(self (Index(c, e, i)))
        | Macro(c, "vs_updated", [Dot(_, e, f); expr]) ->
          match fieldsToReplace.TryGetValue(f) with
            | true, newFld -> 
              let ec = {c with Type = Ptr(f.Type)}
              match subtractOffsets (f.Offset) (newFld.Offset) |> (toBitFieldOffset newFld.Type.SizeOf) with
                | BitField(0, start, size) -> 
                  match bv_extrAndPad ec (Expr.MkDot(c, (self e), newFld)) start (start+size) with
                    | Macro(_, name, [e; bvSize; bvStart; bvEnd]) when name.StartsWith("pullout_bv_extract") ->
                      Some(Macro(c, "vs_updated_bv", [e; bvSize; bvStart; bvEnd; self expr]))
                    | _ -> die()
                | _ -> die()
            | false, _ -> None
        | Dot (c, e, f) as expr ->
          match fieldsToReplace.TryGetValue(f) with
            | true, newFld -> 
              let ec = {c with Type = Ptr(f.Type)}
              match subtractOffsets (f.Offset) (newFld.Offset) |> (toBitFieldOffset newFld.Type.SizeOf) with
                // this will create 'pullout' expressions, which will be normalized in the subsequent union flattening step
                | BitField(0, start, size) -> Some(bv_extrAndPad ec (Expr.MkDot(c, (self e), newFld)) start (start+size))
                | _ -> die()
            | false, _ -> None
        | _ -> None

      decls |> List.iter processTypes
      decls |> deepMapExpressions replExpr
    
    // ============================================================================================================
    
    (*
        struct X {
          int a;
          struct {
            int b;
            int c;
          };
          int d;
        }
        
        The inner struct is unfolded into X.
        
        TODO: make the dummy struct declaration created for inner struct go away.
     *)

    let removedNestedAnonymousTypes decls = 

      let fields = new Dict<Field,Field>()
      let fieldsToRemove = new Dict<Field,bool>()
      let anonTypeDeclsToKeep = new Dict<TypeDecl,bool>()
      let processedTypeDecls = new Dict<TypeDecl, bool>()
      let id = ref 0
      let unnamedFieldId = ref 0
    
      let rec processTypeDecl = function
        | td when processedTypeDecls.ContainsKey(td) -> ()
        | td -> 
            // first process all fields
            processedTypeDecls.Add(td, true)
            let rec processType = function
              | Type.Ref(td) -> processTypeDecl td
              | Array (t, _) -> processType t
              | _ -> ()
            let trField (f:Field) =
              processType f.Type
              match f.Type with 
                | Type.Ref td' when td'.IsNestedAnon && ((td.Kind = Struct && td'.Kind = Struct) || td'.Fields.Length <= 1) ->
                  fieldsToRemove.Add (f, true)
                  [for f' in td'.Fields -> 
                     let newf' =
                       { f' with Offset = addOffsets (f'.Offset) (f.Offset);
                                 Parent = td; 
                                 Name = f'.Name + "#nest" + (!id).ToString () }
                     incr id
                     fields.Add (f', newf')
                     newf'
                  ]
                | Type.Ref td' when td'.IsNestedAnon ->
                  anonTypeDeclsToKeep.Add(td', true)
                  [f]
                | _ -> [f]
            let nameNonameFields (fld:Field) =
              if fld.Name = "" then                    
                fld.Name <- "unnamed#" + (!unnamedFieldId).ToString() 
                incr unnamedFieldId
              fld

            let newFields = td.Fields |> List.map trField |> List.concat |> List.map nameNonameFields
            td.Fields <- newFields
      
      let nameFieldsByMemberName td =
        let nameFieldByMemberName (f:Field) =
          match f.Name, f.Type with
            | "", Type.Ref(td') ->
              let rec findMemberNames acc = function
                | [] -> List.rev acc
                | CustomAttr.VccAttr("member_name", name) :: attrs -> findMemberNames (name :: acc) attrs
                | _ :: attrs -> findMemberNames acc attrs
              match findMemberNames [] td'.CustomAttr with
                | [] -> ()
                | [name] -> f.Name <- name; td'.IsNestedAnon <- false
                | _ -> helper.Error(f.Token, 9695, "More then one member_name for field")
            | _ -> ()
        List.iter nameFieldByMemberName td.Fields

      for d in decls do
        match d with
          | Top.TypeDecl td -> nameFieldsByMemberName td
          | _ -> ()
       
      for d in decls do
        match d with
          | Top.TypeDecl td -> processTypeDecl td
          | _ -> ()
      
      let replFields self = function
        | Dot (c, Macro (_, "&", [Deref (_, e)]), f) -> Some (self (Dot (c, e, f)))
        | Dot (c, Dot (_, e, f'), f) when fields.ContainsKey f && fieldsToRemove.ContainsKey f' ->
          Some (self (Dot (c, e, f)))
        | Dot (c, e, f) ->
          if fieldsToRemove.ContainsKey f then die()
          match fields.TryGetValue f with
            | (true, sField) -> Some (self (Dot (c, e, sField)))
            | (false, _) -> None
        | _ -> None
 
      let keepTypeDecl d = 
        match d with
          | Top.TypeDecl td when not td.IsNestedAnon || anonTypeDeclsToKeep.ContainsKey td -> true
          | Top.TypeDecl _ -> false
          | _ -> true

      decls |> List.filter keepTypeDecl |> deepMapExpressions replFields 

    // ============================================================================================================

    let inlineFieldsByRequest decls =
      let processedTypeDecls = new Dict<TypeDecl, bool>()
      let inlinedFields = new Dict<Field, (Field * Field) list>()
      let inlinedArrays = new Dict<Field, Field>()
      let (dotAxioms : Top list ref) = ref []

      let mkBogusEC t = { bogusEC with Type = t }
      
      let genDotAxioms (td : TypeDecl) (oldField: Field) (newFields : Field list) =
        let genDotAxiom (oldSubField : Field) (newField : Field) =
          let varType = Ptr(Type.Ref(td))
          let var = { Name = "p"; Type = varType; Kind = QuantBound } : Variable
          let varref = Ref(mkBogusEC varType, var)
            
          let left = Expr.MkDot(Cast(mkBogusEC (Ptr(oldField.Type)), Unchecked, Expr.MkDot(varref, newFields.Head)), oldSubField)
          let right = Expr.MkDot(varref, newField)
          GeneratedAxiom(Quant(mkBogusEC Bool, { Kind = Forall 
                                                 Variables = [var] 
                                                 Triggers = [[left]] 
                                                 Condition = None 
                                                 Body = Prim(mkBogusEC Bool, Op("==", Unchecked), [left; right]) }), Top.TypeDecl(td))
        let oldFieldTypeDecl = 
          match oldField.Type with
            | Type.Ref(td) -> td
            | _ -> die()
        List.map2 genDotAxiom (oldFieldTypeDecl.Fields) newFields
        
      let genInlinedArrayAxiom td elemType (newFields : Field list) =
        let var = { Name = "p"; Type = ObjectT; Kind = QuantBound } : Variable
        let varref = Ref(mkBogusEC ObjectT, var)
        let mkInstantiatePtr (f : Field) = Macro(mkBogusEC Bool, "instantiate_ptr", [Expr.MkDot(varref, f)])
        let mkAnd e1 e2 = Expr.Prim(mkBogusEC Bool, Op("&&", Unchecked), [e1; e2])
        let instExpr = List.fold mkAnd (mkInstantiatePtr newFields.Head) (List.map mkInstantiatePtr (newFields.Tail))
        let ec = mkBogusEC elemType
        let trigger = Macro(ec, "_vcc_inlined_array", [Dot(ec, varref, newFields.Head)])
        GeneratedAxiom(Quant(mkBogusEC Bool, { Kind = Forall 
                                               Variables = [var] 
                                               Triggers = [[trigger]] 
                                               Condition = None 
                                               Body = instExpr }), Top.TypeDecl(td))
          
      let rec processTypeDecl = function
        | td when processedTypeDecls.ContainsKey(td) -> ()
        | td ->
          processedTypeDecls.Add(td, true)
          let rec processType = function
              | Type.Ref(td) -> processTypeDecl td
              | Array (t, _) -> processType t
              | _ -> ()
          let trField (f:Field) =
            processType f.Type
            let inlineRequested = 
              let inlineAttr = function
                | CustomAttr.VccAttr("inline", "true") -> true
                | _ -> false
              List.exists inlineAttr
            if (not (inlineRequested f.CustomAttr)) then [f] else
              match f.Type with 
                | Type.Ref td' when ((td.Kind = Struct && td'.Kind = Struct) || td'.Fields.Length <= 1) ->
                  let newFields = 
                    [for f' in td'.Fields -> 
                       let newf' =
                         { f' with Offset = 
                                      match f'.Offset with
                                        | Normal n -> Normal (n + f.ByteOffset)
                                        | BitField (n, b, s) -> BitField (n + f.ByteOffset, b, s)
                                      ;
                                    Parent = td; 
                                    Name = "inline#" + f.Name + "#" + f'.Name }
                       newf'
                    ]
                  inlinedFields.Add(f, List.zip td'.Fields newFields)
                  dotAxioms := !dotAxioms @ (genDotAxioms td f newFields)
                  newFields
                | Type.Array(Type.Ref td', n) when td.Kind = Struct ->
                  let mkFieldForIndex i =
                    [ for f' in td'.Fields ->
                      { f' with Offset = 
                                   match f'.Offset with
                                     | Normal n -> Normal (n + f.ByteOffset + i * td'.SizeOf)
                                     | BitField (n,b,s) -> BitField (n + f.ByteOffset + i * td'.SizeOf, b, s)
                                   ;
                                 Parent = td;
                                 Name = f.Name + "#" + f'.Name + "#" + i.ToString() }
                    ]
                  let newFields = [ for i in seq { 0 .. n-1 } -> mkFieldForIndex i ] |> List.concat
                  inlinedArrays.Add(f, newFields.Head)
                  dotAxioms := (genInlinedArrayAxiom td (Ptr(Type.Ref(td'))) newFields) :: !dotAxioms
                  newFields
                | _ -> 
                  helper.Warning(f.Token, 9109, "field '" + f.Name + "' could not be inlined")
                  [f] 
          td.Fields <- td.Fields |> List.map trField |> List.concat 

      for d in decls do
        match d with 
          | Top.TypeDecl td -> processTypeDecl td
          | _ -> ()
      
      let replFields self = function
        | Dot (c, Macro (_, "&", [Deref (_, e)]), f) -> Some (self (Dot (c, e, f)))
        | Dot (c, (Dot(c', e, f') as dot'), f) ->
          let fAfterInlining = 
            match self (Dot(c, Macro(c', "bogus_for_inlining", []), f)) with
              | Dot(_,_, fAfterInlining) -> fAfterInlining
              | _ -> f
          match inlinedFields.TryGetValue(f') with
            | false, _ -> None
            | true, fields -> 
              match _try_assoc fAfterInlining fields with
                | Some field -> Some(self (Dot(c, e, field)))
                | None -> die()
        | Dot (c, e, f) as expr ->
          match inlinedFields.TryGetValue(f) with
            //| true, [(_, f')] -> Some (self (Dot({c with Type = Ptr(f'.Type) }, e, f')))
            | true, _ -> helper.Error(expr.Token, 9679, "Field '" + f.Name + "' has been inlined an cannot be referred to as such; refer to its nested fields instead"); None
            | false, _ -> 
              match inlinedArrays.TryGetValue(f) with
                | true, f' -> Some (self (Macro(c, "_vcc_inlined_array", [Dot(c, e, f')])))
                | _ -> None
         | _ -> None

      (decls |> deepMapExpressions replFields) @ !dotAxioms
  
    // ============================================================================================================
         
    let turnSingleMemberUnionsIntoStructs decls =
    
      let atMostOneNonspecField =
        let rec atMostOneNonspecField' found = function
          | [] -> true
          | (fld : Field) :: flds when fld.IsSpec -> atMostOneNonspecField' found flds
          | _ :: flds when not found -> atMostOneNonspecField' true flds // first found
          | _ -> false // second found
        atMostOneNonspecField' false
    
      for d in decls do
        match d with 
          | TypeDecl td when td.Kind = TypeKind.Union && atMostOneNonspecField td.Fields -> td.Kind <- Struct
          | _ -> ()
      decls
      
    // ============================================================================================================
    
    // TODO
    // remark: not quite sure we need that                
    /// Replace structs/unions smaller than 64-bit with integers (or longs).
    (* E.g.    
         union U {
           struct {
             INT32 i1;
             INT32 i2;
           }
           UINT64 AsUINT64;
         }
       
       Then U should be replaced with UINT64 and field accesses on U should be
       replaced with bit operators.
       
       union U *u;
       
       u->i2 ====> Macro("bv_extract", [*u; 32; 64])
       
       
       Later:       
       
         Macro("bv_extract", [p->f; x; y]) = e'  ====>
           tmp = p
           tmp->f = Macro("bv_concat", [Macro("bv_extract", [tmp->f; 0; x]);
                                        Macro("bv_extract", [e'; 0; y-x]);
                                        Macro("bv_extract", [tmp->f; y; 8*sizeof(p->f)])])
    
      The other (simpler) option, just translate:
      
        union U {
          T f1;
          T f2;
          ...
          T fN;
        }
        
      into T (this includes the case when N==1).
    *)
    let removeSmallStructs (decls:list<Top>) = decls
    
    // ============================================================================================================
    
    let handleStructAndRecordEquality self expr =
      let rec markTypeDecl eqKind (td : TypeDecl) =
        match td.GenerateEquality, eqKind with
          | (NoEq | ShallowEq), DeepEq  -> 
            td.GenerateEquality <- eqKind
            List.iter (markField eqKind) td.Fields
          | (NoEq | ShallowEq), ShallowEq when td.Kind = TypeKind.Union  -> 
            td.GenerateEquality <- eqKind
            List.iter (markField eqKind) td.Fields
          | DeepEq , ShallowEq -> ()
          | _ -> 
            assert (eqKind <> NoEq)
            td.GenerateEquality <- eqKind
      and markField eqKind (f : Field) =
        match f.Type with
        | Type.Ref td -> markTypeDecl eqKind td
        | _ -> ()
      
      match expr with
        | Expr.Macro(c, (("_vcc_deep_struct_eq" | "_vcc_shallow_struct_eq") as name), ([e1;e2] as args)) ->
          let eqKind = if name = "_vcc_deep_struct_eq" then DeepEq else ShallowEq
          match e1.Type with
          | Type.Ref(td) as t ->
            setEqualityKind td eqKind
            Some(Expr.Macro(c, name + "." + td.Name, [self e1; self e2]))
          | _ -> 
            helper.Error(c.Token, 9655, "structured equality applied to non-structured type " + e1.Type.ToString(), None)
            None
        | Expr.Prim(ec, Op("==",_), [e1; e2]) ->
          match e1.Type with
            | Type.Ref(td) when isRecord td -> Some(self (Expr.Macro(ec, "_vcc_rec_eq", [e1; e2])))
            | _ -> None
        | _ -> None

    // ============================================================================================================

    
    let handleVolatileModifiers decls =
    
      let fldToVolatileFld = new Dict<_,_>()
      let initialFldToVolatileFld = new Dict<_,_>()
      let volatileVars = new Dict<Variable,Variable>()
      let volatileTds = ref []
      let typeToVolatileType = new Dict<_,_>()
    
      let mkVolFld td (f : Field) = 
        let f' = {f with IsVolatile = true; Parent = td}
        fldToVolatileFld.Add(f,f')
        f'

      let rec mkVolTd (td : TypeDecl) =
      
        let fixFields oldFields newFields =
          let fieldSubst = new Dict<Field,Field>()
          List.iter2 (fun oldF newF -> fieldSubst.Add(oldF, newF)) oldFields newFields
          
          let subsFields self = function
            | Dot(ec, expr, f) ->
              match fieldSubst.TryGetValue(f) with
                | (true, f') -> Some(Expr.MkDot({ec with Type = Type.Ptr(f'.Type)}, self expr, f'))
                | _ -> None
            | _ -> None
          subsFields      
          
        let retypeThis oldTd newTd self = function
          | Expr.Macro({Type = Ptr(Type.Ref(oldType))} as c, "this", []) ->
              Some (Expr.Macro( { c with Type = Type.Ptr(Type.Ref(newTd)) }, "this", []))
          | _ -> None
      
        match typeToVolatileType.TryGetValue(td) with
          | true, td' -> td'
          | false, _ ->
            let td' = { td with Name = "volatile#" + td.Name; 
                                IsVolatile = true; 
                                CustomAttr = CustomAttr.VccAttr("volatile_owns", "true") :: td.CustomAttr }
            typeToVolatileType.Add(td, td')
            typeToVolatileType.Add(td', td')
            let vFields = List.map (mkVolFld td') td'.Fields
            td'.Invariants <- List.map (fun (expr:Expr) -> expr.SelfMap(retypeThis td td').SelfMap (fixFields td'.Fields vFields)) td'.Invariants
            td'.Fields <- vFields
            volatileTds := td' :: !volatileTds
            pushDownOne false td'  
            td'
        
      and pushDownOne initial (td : TypeDecl) =
        let pdo (f:Field) =
          match f.Type with
            | Type.Ref({Kind = Struct|Union} as td) when f.IsVolatile && not (isRecord td)->
              let td' = mkVolTd td
              let f' = {f with IsVolatile = false; Type = Type.Ref(td')}
              if initial then initialFldToVolatileFld.Add(f, f')
              f'
            | Type.Ptr(Type.Volatile((Type.Ref({Kind = Struct|Union} as td)))) ->
              let td' = mkVolTd td
              let f' = {f with Type = Type.Ptr(Type.Ref(td'))}
              initialFldToVolatileFld.Add(f, f')
              f'
            | Type.Array(Type.Volatile((Type.Ref({Kind = Struct|Union} as td))), size) ->
              let td' = mkVolTd td
              let f' = {f with Type = Type.Array(Type.Ref(td'), size)}
              initialFldToVolatileFld.Add(f, f')
              f'
            | Type.Array(Type.Volatile(t), size) ->
              let f' = {f with Type = Type.Array(t, size); IsVolatile=true}
              initialFldToVolatileFld.Add(f, f')
              f'
            | Type.Ptr(Type.Volatile(t)) ->
              if initial then
                helper.Warning(f.Token, 9114, "volatile specifier for pointers to primitive types are currently ignored")
              let f' = {f with Type = Type.Ptr(t) }
              initialFldToVolatileFld.Add(f, f')
              f'
            | _ -> f
        // if typeToVolatileType.ContainsKey(td)  then () 
        td.Fields <- List.map pdo td.Fields
        
      let rec typeShouldBePropagated = function
        | Type.Ref(td) when td.IsVolatile -> true
        | Type.Ptr(t) -> typeShouldBePropagated t
        | Type.Array(t, _) -> typeShouldBePropagated t
        | _ -> false
        
      let subsFields self = function
        | Expr.Ref(ec, v) ->
          match volatileVars.TryGetValue(v) with
            | true, v' -> Some(Expr.Ref({ec with Type = v'.Type}, v'))
            | _ -> None
        | Dot(ec, expr, f) ->
          match initialFldToVolatileFld.TryGetValue(f) with
            | true, f' -> 
              let t = match f'.Type with | Array(t, _) | t -> Type.Ptr(t)
              Some(Dot({ec with Type = t}, self expr, f'))
            | false, _ ->
              let (expr' : Expr) = self expr
              match expr'.Type with
                | Type.Ptr(Type.Ref({IsVolatile = true})) ->
                  match fldToVolatileFld.TryGetValue(f) with
                    | true, f' -> Some(Expr.MkDot(ec, expr', f'))
                    | false, _ -> Some(Dot(ec, expr', f))
                | _ -> None

        // for the above to work as expected, we need to propagate volatile type information outward
  
        | Index(ec, expr, idx) -> 
          let se = self expr
          let si = self idx
          if typeShouldBePropagated se.Type then
            let t = match se.Type with | Ptr(Array(t, _)) -> Ptr(t) | t -> t
            Some(Index({ec with Type = t}, se, si))
          else
            Some(Index(ec, se, si))
        | Deref(ec, expr) ->
          let se = self expr
          if typeShouldBePropagated se.Type then
            match se.Type with
              | Ptr(t) -> Some(Deref({ec with Type = t}, se))
              | _      -> Some(Deref(ec, se))
          else 
            Some(Deref(ec, se))
        | Macro(ec, "&", [expr]) ->
          let se = self expr
          if typeShouldBePropagated se.Type then
            Some(Macro({ec with Type = Ptr(se.Type)}, "&", [se]))
          else
            Some(Macro(ec, "&", [se]))
        | Pure(ec, expr) ->
          let se = self expr
          if typeShouldBePropagated se.Type then
            Some(Pure({ec with Type = se.Type}, se))
          else
            Some(Pure(ec, se))
        | _ -> None
      
      for d in decls do
        match d with
          | Top.TypeDecl(td) -> pushDownOne true td
          | Top.FunctionDecl({Body = Some(body)}) ->
            let pdLocalDecls self = function
              | VarDecl(_, ({Type = Ptr(Volatile(Type.Ref(td)))} as v)) -> 
                let td' = mkVolTd td
                volatileVars.Add(v, {v with Type = Ptr(Type.Ref(td'))})
                false
              | _ -> true
            body.SelfVisit pdLocalDecls
          | Top.Global({Type = Type.Volatile(Type.Ref(td))} as v, _) ->
            let td' = mkVolTd td
            volatileVars.Add(v, {v with Type = Type.Ref(td')})
          | _ -> ()
          
      decls @ (List.map (fun td -> Top.TypeDecl(td)) !volatileTds) |> deepMapExpressions subsFields
    
    // ============================================================================================================

    let assignSingleFieldStructsByField self = function
      | Macro(ec, "=", [dst; src])  ->
        match dst.Type with
          | Type.Ref({Fields = [fld]} as td) when not (isRecord td)->
            let addDot (e:Expr) = 
              Deref({e.Common with Type = fld.Type}, Expr.MkDot(Macro({e.Common with Type = Ptr(e.Type)}, "&", [e]), fld))
            Some(Macro(ec, "=", [addDot dst; addDot src]))
          | _ -> None
      | _ -> None
            
    // ============================================================================================================

    let addAxiomsForReadsFromConstArrays decls = 
      let addAxiomsForReadsFromConstArrays' = function
        | Global({Type = Array(t, n); Kind = ConstGlobal} as v, Some(init)) -> 
          let ec_b = {bogusEC with Type = Bool }
          let ec_t = {bogusEC with Type = t }
          let ec_ptr = {bogusEC with Type = Ptr(t)}
          let readAxioms =
            let rec readAxiom n = function
              | [] -> []
              | expr :: exprs ->
                let ax = Top.GeneratedAxiom(Expr.Prim(ec_b, 
                                             Op("==", Unchecked), 
                                             [
                                               Expr.Deref(ec_t, Expr.Index(ec_ptr, Expr.Macro(ec_ptr, "&", [Expr.Ref(ec_t, v)]), mkInt n));
                                               expr
                                             ]), Top.Global(v, None)) 
                ax :: (readAxiom (n+1) exprs)
            function 
              | Expr.Macro(_, "init", initializers) -> readAxiom 0 initializers
              | _ -> die()
          let threadLocalAxiom = Top.GeneratedAxiom(Expr.Macro(ec_b, 
                                                               "_vcc_is_thread_local_array" , 
                                                               [Expr.Macro(ec_ptr, "&", [Expr.Ref(ec_t, v)]); mkInt n]), Top.Global(v,None));

          Global (v, None) :: threadLocalAxiom :: (readAxioms init) 
        | Global ({Kind = VarKind.ConstGlobal} as v, Some init) when not init.Type.IsComposite -> [Global (v, None)] // the initial value is handled by FELT in the AST
        | Global (v, Some(init)) -> 
          helper.Warning(init.Token, 9112, "unhandled initializer")
          [Global(v, None)]
        | other -> [other]
        
        
      decls |> List.map addAxiomsForReadsFromConstArrays' |> List.concat

    // ============================================================================================================

    let checkRecordValidity decls =
      
      let checkDecl (td : TypeDecl) =
      
        if (td.Invariants.Length > 0) then
          helper.Error(td.Token, 9698, "type '" + td.Name + "' is marked as a record type and thus must not declare invariants", Some(td.Invariants.Head.Token))
        
        if (td.Kind = TypeKind.Union) then
          helper.Error(td.Token, 9682, "union '" + td.Name + "' cannot be flattened into a struct and thus cannot be marked as record type")
        else 
          let checkField (f:Field) =
            let rec checkType = function
              | Type.Ref(td') when not (isRecord td') ->
                helper.Error(f.Token, 9680, "field '" + f.Name + "' of record type '" + td.Name + "' cannot be of non-record structured type '" + td'.Name + "'", Some(td'.Token))
              | Type.Volatile _ -> helper.Error(f.Token, 9683, "volatile modified on field '" + f.Name + "' in record type '" + td.Name + "' is currently not supported")
              | Type.Array _ -> helper.Error(f.Token, 9688, "inline array field '" + f.Name + "' in record type '" + td.Name + "' is currently not supported")
              | _ -> ()
            // we now automatically mark them all as spec
            //if (f.IsSpec) then helper.Warning(f.Token, 9118, "field '" + f.Name + "' in record type does not need to marked as spec field")
            checkType f.Type
          List.iter checkField td.Fields
          
          
      
      for d in decls do
        match d with
          | Top.TypeDecl(td) when isRecord td-> checkDecl td
          | _ -> ()

      decls

    // ============================================================================================================

    helper.AddTransformer ("type-begin", Helper.DoNothing)
    helper.AddTransformer ("type-function-pointer", Helper.Decl handleFunctionPointers)
    helper.AddTransformer ("type-groups", Helper.Decl liftGroups)
    helper.AddTransformer ("type-mark-nested", Helper.Decl markNestedAnonymousTypes)
    helper.AddTransformer ("type-remove-bitfields", Helper.Decl removeBitfields)
    helper.AddTransformer ("type-flatten-unions", Helper.Decl flattenUnions)
    helper.AddTransformer ("type-remove-nested", Helper.Decl removedNestedAnonymousTypes)
    helper.AddTransformer ("type-fixup-single-member-unions", Helper.Decl turnSingleMemberUnionsIntoStructs)
    helper.AddTransformer ("type-assign-by-single-field", Helper.Expr assignSingleFieldStructsByField)
    helper.AddTransformer ("type-inline-fields", Helper.Decl inlineFieldsByRequest)
    helper.AddTransformer ("type-fixup-single-member-unions", Helper.Decl turnSingleMemberUnionsIntoStructs)
    helper.AddTransformer ("type-check-records", Helper.Decl checkRecordValidity)
    helper.AddTransformer ("type-handle-volatile-modifiers", Helper.Decl handleVolatileModifiers)
    helper.AddTransformer ("type-struct-equality", Helper.Expr handleStructAndRecordEquality)
    helper.AddTransformer ("type-constant-arrays", Helper.Decl addAxiomsForReadsFromConstArrays)
    helper.AddTransformer ("type-end", Helper.DoNothing)
