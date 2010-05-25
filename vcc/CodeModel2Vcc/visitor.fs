//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#light

namespace Microsoft.Research.Vcc
  open Microsoft.FSharp.Math
  open Microsoft.Cci
  open Microsoft.Cci.Contracts
  open Microsoft.Cci.Ast
  open Microsoft.Research.Vcc
  open Microsoft.Research.Vcc.Util
  module C = Microsoft.Research.Vcc.CAST

  module StringLiterals =
    [<Literal>]
    let SystemDiagnosticsContractsCodeContract = "System.Diagnostics.Contracts.CodeContract"

    [<Literal>]
    let SystemDiagnosticsContractsCodeContractTypedPtr = "System.Diagnostics.Contracts.CodeContract.TypedPtr"

    [<Literal>]
    let SystemDiagnosticsContractsCodeContractMap = "System.Diagnostics.Contracts.CodeContract.Map"

    [<Literal>]
    let SystemDiagnosticsContractsCodeContractBigInt = "System.Diagnostics.Contracts.CodeContract.BigInt"

    [<Literal>]
    let SystemDiagnosticsContractsCodeContractObjset = "System.Diagnostics.Contracts.CodeContract.Objset"


  open StringLiterals

  type Dict<'a, 'b> = System.Collections.Generic.Dictionary<'a, 'b>
  
  and Visitor(contractProvider:Microsoft.Cci.Ast.SourceContractProvider, helper:Helper.Env) =

    do C.PointerSizeInBytes := helper.PointerSizeInBytes

    let mutable topDecls : list<C.Top> = []
    let mutable stmtRes : C.Expr = C.Expr.Bogus
    let mutable exprRes : C.Expr = C.Expr.Bogus
    let mutable typeRes : C.Type = C.Type.Bogus
    let mutable globalsType = null
    let mutable fnPtrCount = 0
    let finalActions = System.Collections.Generic.Queue<(unit -> unit)>()
    
    let mutable localsMap = new Dict<obj, C.Variable>(new ObjEqualityComparer());
    let mutable localVars = []
    let globalsMap = new Dict<IGlobalFieldDefinition, C.Variable>()
    let specGlobalsMap = new Dict<string, C.Variable>()
    let methodsMap = new Dict<ISignature, C.Function>()
    let functionPointerMap = new Dict<ISignature, C.TypeDecl>()
    let methodNameMap = new Dict<string, C.Function>()
    let typesMap = new Dict<ITypeDefinition, C.TypeDecl>()
    let typeNameMap = new Dict<string, C.TypeDecl>()
    let fieldsMap = new Dict<IFieldDefinition, C.Field>()
    let mutable doingEarlyPruning = false
    let mutable requestedFunctions = []
    let mutable currentFunctionName = ""
    
    let cTrue = C.Expr.BoolLiteral ( { Type = C.Type.Bool; Token = C.bogusToken }, true )
    let cFalse = C.Expr.BoolLiteral ( { Type = C.Type.Bool; Token = C.bogusToken }, false )

    let token (o : Microsoft.Cci.IObjectWithLocations) = VisitorHelper.GetTokenFor (o.Locations)
    let oops msg =
      helper.Oops (C.bogusToken, msg)
    let oopsLoc (o : Microsoft.Cci.IObjectWithLocations) msg =
      helper.Oops (token o, msg)
    let emacro name args = C.Expr.Macro (C.ExprCommon.Bogus, name, args)
    let die () = helper.Die ()
    let xassert cond =
      if cond then ()
      else oops "assertion failed"; die ()

    let findFunctionOrDie name objWithLoc =
      match methodNameMap.TryGetValue(name) with
        | true, f -> f
        | _ -> oopsLoc objWithLoc ("cannot find internal function " + name + ". Forgotten #include <vcc.h>?"); die()


    let checkedStatus ch = if ch then C.CheckedStatus.Checked else C.CheckedStatus.Unchecked
  
    let stmtToken (msg:string) (e:C.Expr) =
      let forwardingToken tok (getmsg : unit -> string) =
        { Token = (new ForwardingToken (tok, ForwardingToken.GetValue getmsg) :> Token);
          Type = C.Type.Void } : C.ExprCommon
      forwardingToken e.Token (fun () -> msg.Replace ("@@", e.Token.Value))

    let removeDuplicates l =
      let elements = new Dict<_,bool>()
      let rec loop = function
        | [] -> []
        | x::xs ->
          match elements.TryGetValue x with
            | true, _ -> loop xs
            | false, _ -> elements.Add(x, true); x :: loop xs
      loop l

    let hasCustomAttr n = List.exists (function C.VccAttr (n', _) -> n = n' | _ -> false)

    let convCustomAttributes tok (attrs : ICustomAttribute seq) = 
      let getAttrTypeName (attr:ICustomAttribute) = TypeHelper.GetTypeName(attr.Type.ResolvedType)
      let getAttrArgs (attr:ICustomAttribute) =
        let args = new System.Collections.Generic.List<IMetadataExpression>(attr.Arguments)
        let name = ((args.Item(0) :?> IMetadataConstant).Value :?> string)
        let value = if args.Count > 1 then (args.Item(1) :?> IMetadataConstant).Value else null
        (name, value)      
      [ for attr in attrs do
        let attrName = getAttrTypeName attr
        match attrName with
        | "Microsoft.Cci.DummyType" -> yield! []   
        | "System.Diagnostics.Contracts.CodeContract.BoolBoogieAttr" -> 
          let (name, value) = getAttrArgs attr
          yield C.BoolBoogieAttr (name, (value :?> int) <> 0)
        | "System.Diagnostics.Contracts.CodeContract.StringVccAttr" ->
          let (name, value) = getAttrArgs attr
          yield C.VccAttr (name, (value :?> string))
        | "System.Diagnostics.Contracts.CodeContract.IntBoogieAttr" ->
          let (name, value) = getAttrArgs attr
          yield C.IntBoogieAttr (name, (value :?> int))
        | other when other.StartsWith("Microsoft.Contracts") ->
            do helper.Oops (tok, "unsupported custom attribute: " + other); die (); 
            yield! []
        | _ -> yield! []
      ]
    
    static member private CheckHasError(e : IExpression) =
     match e with
       | :? IErrorCheckable as ec -> ec.HasErrors
       | _ -> false
    
    static member private CheckHasError(c : ITypeContract) =
     match c with
       | :? IErrorCheckable as ec -> ec.HasErrors
       | _ -> false
    
    static member private CheckHasError(s : IStatement) =
     match s with
       | :? IErrorCheckable as ec -> ec.HasErrors
       | _ -> false
    
    static member private CheckHasError(c : IMethodContract) =
     match c with
       | :? IErrorCheckable as ec -> ec.HasErrors
       | _ -> false
    
    static member private CheckHasError(c : ILoopContract) =
     match c with
       | :? IErrorCheckable as ec -> ec.HasErrors
       | _ -> false
    
    member private this.DoPrecond (p:IPrecondition) =
      if Visitor.CheckHasError(p.Condition) then oopsLoc p "precondition has errors"; cTrue
      else this.DoExpression (p.Condition)
    member private this.DoPostcond (p:IPostcondition) =
      if Visitor.CheckHasError(p.Condition) then oopsLoc p "postcondition has errors"; cFalse
      else this.DoExpression (p.Condition)
    member private this.DoMethodVariant (v:IMethodVariant) =
      if Visitor.CheckHasError(v.Condition) then oopsLoc v "variant has errors"; cFalse
      else this.DoExpression (v.Condition)
        
    member this.GetResult () =
      while finalActions.Count > 0 do      
        finalActions.Dequeue () ()
      topDecls

    member this.EnsureMethodIsVisited (m : IMethodDefinition) =
      if doingEarlyPruning then 
        match m with 
          | :? IGlobalMethodDefinition -> this.DoMethod(m, true) 
          | :? IGenericMethodInstance as gmi -> this.EnsureMethodIsVisited(gmi.GenericMethod.ResolvedMethod)
          | _ -> ()
    
    member this.DoType (typ:ITypeReference) =
      typeRes <- C.Type.Bogus
      typ.Dispatch (this)
      let res = typeRes
      typeRes <- C.Type.Bogus
      if typ.TypeCode <> PrimitiveTypeCode.Invalid && res = C.Type.Bogus then
        die ()
      match res with
        | C.Type.Ref ({ Name = "typeid_t"; Kind = C.MathType }) -> C.Type.TypeIdT
        | _ -> res
    
    member this.ExprCommon (expr:IExpression) = { Token = token expr; Type = this.DoType (expr.Type) } : C.ExprCommon
      
    member this.StmtCommon (expr:IStatement) = { Token = token expr; Type = C.Void } : C.ExprCommon
      
    member this.LookupMethod (meth:ISignature) : C.Function =
      if methodsMap.ContainsKey meth then
        methodsMap.[meth]
      else
        let (name, tok) = 
          match meth with
            | :? IMethodDefinition as def -> (def.Name.Value, token def)
            | _ -> 
              fnPtrCount <- fnPtrCount + 1
              ("fnptr#" + fnPtrCount.ToString(), C.bogusToken)
        // there is an additional malloc in Vcc.Runtime, we want only one
        // same goes for other ones
        // TODO check if the thing is from Vcc.Runtime and if so, ignore it -- don't list all the names
        match name with
          | "malloc" 
          | "free"
          | "get___FUNCTION__"
          | "memcmp"
          | "__int2c"
          | "__debugbreak"
          | "memcpy" 
              when methodNameMap.ContainsKey name ->
            let decl = methodNameMap.[name]
            methodsMap.Add (meth, decl)
            decl
          | _ when methodsMap.ContainsKey meth -> methodsMap.[meth]
          | _ ->
            let sanitizedTypeName (t : C.Type) = t.ToString().Replace(' ', '_').Replace('*', '.')
            let parKind (p:IParameterTypeInformation) =
              match p with
                | :? Microsoft.Research.Vcc.VccParameterDefinition as vcp -> 
                  if vcp.IsOut then "out_" elif vcp.IsSpec then "spec_" else ""
                | _ -> ""
            let parKind' (p:C.Variable) = 
              match p.Kind with
                | C.VarKind.Parameter -> ""
                | C.VarKind.SpecParameter -> "spec_"
                | C.VarKind.OutParameter -> "out_"
                | _ -> die()
            let nameWhenOverloadsArePresent methName (meth : ISignature) =
              let typeName t = sanitizedTypeName (this.DoType(t))
              let parTypes = [| for p in meth.Parameters -> parKind p + typeName p.Type |]
              let parTypeString = if parTypes.Length = 0 then "" else "#" + System.String.Join("#", parTypes)
              methName + "#overload#" + (typeName meth.Type)  + parTypeString
            let updatedNameWhenOverloadsArePresent (fn : C.Function) =
              let parTypes = [| for p in fn.Parameters -> (parKind' p) + (sanitizedTypeName p.Type) |]
              let parTypeString = if parTypes.Length = 0 then "" else "#" + System.String.Join("#", parTypes)
              fn.Name + "#overload#" + (sanitizedTypeName fn.RetType) + parTypeString
            let isSpec =
              if name.StartsWith("_vcc") then true 
              else match meth with
                    | :? Microsoft.Research.Vcc.VccGlobalMethodDefinition as def -> def.IsSpec 
                    | _ -> false 
            let decl =
              { Token           = tok
                IsSpec          = isSpec
                RetType         = this.DoType(meth.Type)
                OrigRetType     = this.DoType(meth.Type)
                Name            = name
                Parameters      = []
                TypeParameters  = []
                Requires        = []
                Ensures         = []
                Writes          = []
                Variants        = []
                Reads           = []
                CustomAttr      = []
                Body            = None
                IsProcessed     = false
                UniqueId        = CAST.unique() } : C.Function                      
            if decl.Name = "" then
              printf "null name\n"
            else
              match methodNameMap.TryGetValue(decl.Name) with
                | true, clashingDecl ->
                  decl.Name <- nameWhenOverloadsArePresent decl.Name meth
                  clashingDecl.Name <- updatedNameWhenOverloadsArePresent clashingDecl
                | _ ->
                  methodNameMap.Add(decl.Name, decl)
            methodsMap.Add(meth, decl)
            decl
    
    member this.DoMethod (meth:ISignature, contractsOnly:bool) =
      
      let (name, genericPars) =
        match meth with
          | :? IMethodDefinition as def -> def.Name.Value, def.GenericParameters
          | _ -> "", Seq.empty

      if name = "_vcc_atomic_op_result" then ()
      else       
        let decl = this.LookupMethod (meth)
        let body = 
          match meth with
            | :? IMethodDefinition as def ->
              (def.Body :?> ISourceMethodBody).Block
            | _ -> null
        if decl.IsProcessed then ()
        else
          decl.IsProcessed <- true       
          let parm (p:IParameterTypeInformation) =
            let name =
              match p with
                | :? IParameterDefinition as def -> def.Name.Value
                | _ -> "#p" + p.GetHashCode().ToString()
            let isSpec, isOut =
              match p with
                | :? Microsoft.Research.Vcc.VccParameterDefinition as vcp -> vcp.IsSpec, vcp.IsOut
                | _ -> false, false
            let varKind =
              match isSpec, isOut with
                | true, true -> C.VarKind.OutParameter
                | true, false -> C.VarKind.SpecParameter
                | false, false -> C.VarKind.Parameter
                | _ -> die() // out param must always also be a spec parameter
            let v = C.Variable.CreateUnique name (this.DoType (p.Type)) varKind
            if localsMap.ContainsKey p || localsMap.ContainsKey ((p.ContainingSignature, v.Name)) then
              helper.Error(decl.Token, 9707, "'" + v.Name + "' : parameter redefinition")
            else 
              localsMap.Add (p, v)
              // this is SO WRONG, however it seems that sometimes different instances
              // of IParameterDefinition are referenced from code (does it have to do with
              // contract variable renaming?)
              localsMap.Add ((p.ContainingSignature, v.Name), v)
            v
                                    
          // needs to be done first so they get into localsMap
          decl.TypeParameters <- [ for tp in genericPars -> { Name = tp.Name.Value } : C.TypeVariable ]
          decl.Parameters <- meth.Parameters |> Seq.toList |> List.filter (fun p -> p.Type.TypeCode <> PrimitiveTypeCode.Void) |> List.map parm

          // extract custom attributes
          match meth with
            | :? IMethodDefinition as def ->
              let attrsFromDecls = 
                match meth with
                | :? VccGlobalMethodDefinition as fd ->
                  List.concat [ for d in fd.Declarations -> convCustomAttributes (token d) d.Attributes ]
                | _ -> []
              let attrsFromDef = convCustomAttributes (token def) def.Attributes
              decl.CustomAttr <- removeDuplicates (attrsFromDef @ attrsFromDecls)
            | _ -> ()

          let contractsOnly = contractsOnly && 
                              not (hasCustomAttr "atomic_inline" decl.CustomAttr) && 
                              not (List.exists (fun n -> n = decl.Name) requestedFunctions)
          // make sure that if the current function is explicitly requested or atomic_inline, then process its body
          // coming here again to process the body in a second round does not work.
          
          if body = null || contractsOnly then
            topDecls <- C.Top.FunctionDecl decl :: topDecls
          else
            let savedLocalVars = localVars
            localVars <- []
            currentFunctionName <- decl.Name
            let body = this.DoStatement body
            let body' = if decl.IsSpec then C.Expr.Macro(body.Common, "spec", [body]) else body
            let locals = (List.map (fun v -> C.Expr.VarDecl (C.voidBogusEC(), v)) decl.InParameters) @ List.rev localVars
            decl.Body <- Some (C.Expr.MkBlock (locals @ [body']))
            localVars <- savedLocalVars
            topDecls <- C.Top.FunctionDecl decl :: topDecls

          let contract = contractProvider.GetMethodContractFor(meth)     
          if contract <> null then
            Visitor.CheckHasError(contract) |> ignore
            // reset localsMap to deal with renaming of contracts between definition and declaration          
            let savedLocalsMap = localsMap
            match contract with
            | :? MethodContract as methodContract ->
              let isNotVoidPar (p : ParameterDeclaration) = p.Type.ResolvedType.TypeCode <> PrimitiveTypeCode.Void
              if (methodContract.ContainingSignatureDeclaration.Parameters |> Seq.filter isNotVoidPar |> Seq.length) <> (Seq.length decl.Parameters) then
                helper.Error(decl.Token, 9658, "declared formal parameter list different from definition", Some(VisitorHelper.GetTokenFor [(methodContract.ContainingSignatureDeclaration.SourceLocation :> ILocation)]))
              localsMap <- new Dict<obj,_>(new ObjEqualityComparer())
              let addParmRenaming (fromParm:ParameterDeclaration) (toVar:C.Variable) =
                localsMap.Add(((fromParm.ContainingSignature.SignatureDefinition), fromParm.Name.Value), toVar)
              let contractPars = seq { for p in methodContract.ContainingSignatureDeclaration.Parameters do if p.Type.ResolvedType.TypeCode <> PrimitiveTypeCode.Void then yield p }
              Seq.iter2 addParmRenaming contractPars decl.Parameters
            | _ -> ()

            decl.Requires   <- [ for p in contract.Preconditions -> this.DoPrecond p ]
            decl.Ensures    <- [ for r in contract.Postconditions -> this.DoPostcond r ]
            decl.Writes     <- [ for e in contract.Writes -> this.DoExpression e ]
            decl.Reads      <- [ for e in contract.Reads -> this.DoExpression e ]
            localsMap <- savedLocalsMap
        
        
    member this.DoStatement (stmt:IStatement) : C.Expr =
      if Visitor.CheckHasError(stmt) then
        oopsLoc stmt  "errors in stmt"
        C.Comment (this.StmtCommon stmt, sprintf "statement %s had errors" (stmt.ToString()))
      else
        stmtRes <- C.Expr.Bogus
        match stmt with
          | :? IVccStatement as stmt -> stmt.Dispatch(this)
          | stmt -> stmt.Dispatch(this)
        let res = stmtRes
        stmtRes <- C.Expr.Bogus
        xassert (res <> C.Expr.Bogus)
        res
     
    member this.DoInvariant (inv:ITypeInvariant) : C.Expr =
      let cond = this.DoExpression inv.Condition
      let name =if inv.Name <> null then inv.Name.Value else "public"
      C.Expr.Macro(cond.Common, "labeled_invariant", [C.Expr.Macro(C.bogusEC, name, []); cond])
     
    member this.DoExpression(expr:IExpression) : C.Expr =
      if Visitor.CheckHasError(expr) then
        oopsLoc expr "error in expr"
        C.Expr.Bogus
      else
        exprRes <- C.Expr.Bogus
        expr.Dispatch(this)
        let res = exprRes
        exprRes <- C.Expr.Bogus
        xassert (res <> C.Expr.Bogus)
        res

    member this.DoBlock(block : IBlockStatement) =
     let savedLocalVars = localVars
     localVars <- []
     let stmts = [for s in block.Statements -> this.DoStatement (s)]
     let block' = C.Expr.MkBlock (localVars @ stmts)
     localVars <- savedLocalVars
     let contract = contractProvider.GetMethodContractFor(block)
     let result = 
       if (contract = null) then
         block'
       else 
        let cs = {Requires = [ for req in contract.Preconditions -> this.DoPrecond req ];
                  Ensures  = [ for ens in contract.Postconditions -> this.DoPostcond ens ];
                  Reads    = [ for rd in contract.Reads -> this.DoExpression rd ];
                  Writes   = [ for wr in contract.Writes -> this.DoExpression wr ];
                  Decreases= [ for vr in contract.Variants -> this.DoMethodVariant vr ]} : CAST.BlockContract
        match block' with
          | C.Expr.Block (ec,ss,_) -> C.Expr.Block (ec,ss, Some cs)
          | _ -> C.Expr.Block ({ Type = block'.Type; Token = block'.Token }, [block'], Some cs)
     match block with
       //| :? VccSpecBlock -> C.Macro(result.Common, "spec", [result])
       | _ -> result

    member this.DoUnary (op:string, bin:IUnaryOperation, ch) =
      exprRes <- C.Expr.Prim (this.ExprCommon bin, C.Op(op, checkedStatus ch), [this.DoExpression (bin.Operand)])

    member this.DoBinary (op:string, bin:IBinaryOperation) =
      this.DoBinary(op, bin, false)

    member this.DoBinary (op:string, bin:IBinaryOperation, ch:bool) =
      exprRes <- C.Expr.Prim (this.ExprCommon bin, C.Op(op, checkedStatus ch), [this.DoExpression (bin.LeftOperand); this.DoExpression (bin.RightOperand)])

    member this.DoGlobal (g:IGlobalFieldDefinition) =
      // TODO initializer?
      match globalsMap.TryGetValue g with
        | (true, v) -> v
        | _ -> 
          match g with
            | :? GlobalFieldDefinition as decl ->
              let rec doInitializer t (expr : Expression) = 
                match expr with
                  | :? VccInitializer as initializer ->
                    let ec = { Token = token expr; Type = t } : C.ExprCommon
                    C.Macro(ec, "init", [ for e in initializer.Expressions -> doInitializer (this.DoType (e.Type)) e])
                  | _ when expr.Type.ResolvedType = Microsoft.Cci.Dummy.Type ->
                    this.DoExpression (expr.ContainingBlock.Helper.ImplicitConversionInAssignmentContext(expr, decl.Type.ResolvedType).ProjectAsIExpression())
                  | _ -> this.DoExpression (expr.ProjectAsIExpression())
              let t = this.DoType g.Type
              let t' = if decl.GlobalFieldDeclaration.IsVolatile then C.Type.Volatile(t) else t
              let var = C.Variable.CreateUnique g.Name.Value t' (if decl.IsReadOnly then C.VarKind.ConstGlobal else C.VarKind.Global)
              globalsMap.Add (g, var)
              let initializer = if decl.GlobalFieldDeclaration.Initializer = null then None else Some(doInitializer (var.Type) (decl.GlobalFieldDeclaration.Initializer))
              topDecls <- C.Top.Global(var, initializer) :: topDecls
              var
            | _ -> die()
    
    member this.DoSpecGlobal (g:Microsoft.Cci.Ast.FieldDefinition) =
      // TODO initializer?
      match specGlobalsMap.TryGetValue (g.Name.Value) with
        | (true, v) -> v
        | _ -> 
          let var = C.Variable.CreateUnique g.Name.Value (this.DoType g.Type) C.VarKind.SpecGlobal
          specGlobalsMap.Add (var.Name, var)
          topDecls <- C.Top.Global(var, None) :: topDecls
          var
    
    member this.DoField (expr:IExpression) (instance:IExpression) (definition:obj) =
      let ec = this.ExprCommon expr
      if instance = null then
        exprRes <-
          match definition with
            | :? IExpression as e -> this.DoExpression e
            | _ when localsMap.ContainsKey definition ->
              C.Expr.Ref (ec, localsMap.[definition]) 
            | :? IParameterDefinition as p ->
              let key = (p.ContainingSignature, p.Name.Value)
              let name = p.Name.Value
              if localsMap.ContainsKey name then
                C.Expr.Ref (ec, localsMap.[name])
              else
                if not (localsMap.ContainsKey (key :> obj)) then
                  oopsLoc p ("cannot find parameter " + name); die ()
                C.Expr.Ref (ec, localsMap.[key])
            | :? IGlobalFieldDefinition as g ->
              C.Expr.Ref (ec, this.DoGlobal g)
            | :? Microsoft.Cci.Ast.FieldDefinition as def ->
              if def.FieldDeclaration.Name.Value.StartsWith ("?mappedLiteral") then
                C.Expr.Macro (ec, "string", [C.Expr.ToUserData(def.FieldDeclaration.Initializer.Value)])
              else
                C.Expr.Ref (ec, this.DoSpecGlobal def)
            | :? IMethodDefinition as def ->
              this.EnsureMethodIsVisited(def)
              let fn = this.LookupMethod def
              C.Expr.Macro (ec, "get_fnptr", [C.Expr.Call ({ ec with Type = fn.RetType }, fn, [], [])])
            | _ -> oopsLoc expr ("cannot find " + definition.ToString()); die ()
      else
        match definition with
          | :? IAddressDereference as deref -> (this :> ICodeVisitor).Visit deref
          | :? IFieldDefinition as def ->
            let instance = this.DoExpression instance
            if not (fieldsMap.ContainsKey def) then
              oopsLoc def ("field " + def.Name.Value + " not found")
            
            let field = fieldsMap.[def]
            let instance =
              match instance.Type with
               | C.Ptr _ -> instance
               | t -> C.Expr.Macro ({ instance.Common with Type = C.PhysPtr t }, // TODO: Ptr kind
                                    "&", [instance])
            let dot =  C.Expr.MkDot(ec, instance, field)
            exprRes <- C.Expr.Deref (ec, dot)
          | _ -> assert false
    
    member this.DoTypeDefinition (typeDef:ITypeDefinition) =
      typeRes <-
          match typeDef.TypeCode with
            | PrimitiveTypeCode.Char -> C.Type.Integer (C.IntKind.UInt8)
            | PrimitiveTypeCode.String -> C.Type.PhysPtr (C.Type.Integer C.IntKind.UInt8)
            | PrimitiveTypeCode.UInt8  -> C.Type.Integer (C.IntKind.UInt8)
            | PrimitiveTypeCode.UInt16 -> C.Type.Integer (C.IntKind.UInt16)
            | PrimitiveTypeCode.UInt32 -> C.Type.Integer (C.IntKind.UInt32)
            | PrimitiveTypeCode.UInt64 -> C.Type.Integer (C.IntKind.UInt64)
            | PrimitiveTypeCode.Int8   -> C.Type.Integer (C.IntKind.Int8)
            | PrimitiveTypeCode.Int16  -> C.Type.Integer (C.IntKind.Int16)
            | PrimitiveTypeCode.Int32  -> C.Type.Integer (C.IntKind.Int32)
            | PrimitiveTypeCode.Int64  -> C.Type.Integer (C.IntKind.Int64)
            | PrimitiveTypeCode.Boolean -> C.Type.Bool
            | PrimitiveTypeCode.Void -> C.Type.Void
            | PrimitiveTypeCode.Float32 -> C.Type.Primitive C.PrimKind.Float32
            | PrimitiveTypeCode.Float64 -> C.Type.Primitive C.PrimKind.Float64
            | PrimitiveTypeCode.NotPrimitive ->
              let (hasit, ret) = typesMap.TryGetValue(typeDef)
              if hasit then C.Type.Ref (ret)
              elif typeDef.IsEnum then this.DoType typeDef.UnderlyingType
              else 
                let fields = [ for f in typeDef.Fields -> f ]
                let members = [ for m in typeDef.Members -> m ] // contains fields as well as nested types
                // TODO check embedded structs in unions
                let name =
                  match typeDef with
                    | :? INamespaceTypeDefinition as n -> n.Name.Value
                    | :? INestedTypeDefinition as n -> n.ContainingTypeDefinition.ToString() + "." + n.Name.Value
                    | _ -> die()
                
                let mathPref = "_vcc_math_type_"
                if name.StartsWith mathPref then
                  let typeName = (if helper.Options.Vcc2 then "\\" else "") + (name.Substring (mathPref.Length))
                  let td = C.Type.MathTd typeName
                  typesMap.Add(typeDef, td)
                  typeNameMap.Add(td.Name, td)
                  topDecls <- C.Top.TypeDecl (td) :: topDecls
                  C.Type.Ref td
                else if name = "_vcc_claim_struct" || name = "\\claim_struct" then
                  C.Type.Claim
                else if name.Contains ("._FixedArrayOfSize") then
                  match fields with
                    | [f] ->
                      xassert (typeDef.SizeOf > 0u)
                      let eltype = this.DoType f.Type
                      C.Type.Array (eltype, int typeDef.SizeOf / eltype.SizeOf)
                    | _ -> die()
                else if name = SystemDiagnosticsContractsCodeContractTypedPtr then
                  C.Type.ObjectT
                else if name = SystemDiagnosticsContractsCodeContractBigInt then
                  C.Type.MathInteger
                else if name = SystemDiagnosticsContractsCodeContractObjset then
                  C.Type.Math "\\objset"
                else              
                  let tok = token typeDef
                  let totalOffset f = MemberHelper.GetFieldBitOffset f + f.Offset
                  let notAllEqual = function
                    | x :: xs -> List.exists (fun y -> y <> x) xs
                    | _ -> die()                        
                  let customAttr = convCustomAttributes tok typeDef.Attributes
                  let contract = contractProvider.GetTypeContractFor(typeDef)
                  let specFromContract = 
                    match contract with
                      | :? VccTypeContract as vccTypeContract -> vccTypeContract.IsSpec
                      | _ -> false
                  let td = 
                    { Token = tok
                      Name = name
                      Fields = []
                      Invariants = []
                      CustomAttr = customAttr
                      SizeOf = int typeDef.SizeOf
                      IsNestedAnon = false
                      GenerateEquality = CAST.StructEqualityKind.NoEq
                      GenerateFieldOffsetAxioms = false
                      Kind = 
                        // Cci does not know about unions, so a union for us is a struct with more than one member whose all offsets are equal
                        if List.length fields <= 1 || notAllEqual (List.map totalOffset fields) then
                          C.TypeKind.Struct
                        else
                          C.TypeKind.Union
                      Parent =
                        match typeDef with
                        | :? NestedTypeDefinition as nestedType ->
                          match typesMap.TryGetValue(nestedType.ContainingTypeDefinition) with
                          | (true, parent) -> Some parent
                          | _ -> None
                        | _ -> None
                      IsSpec = specFromContract || hasCustomAttr "record" customAttr
                      IsVolatile = false
                      UniqueId = C.unique()
                    } : C.TypeDecl
                                      
                  // TODO?
                  let td =
                    if td.Name = "Object" && td.SizeOf = 0 then
                      { td with Name = "#Object"; SizeOf = C.Type.ObjectT.SizeOf }
                    else td
                  
                  let minOffset = 
                    match fields with
                      | f :: _ -> int f.Offset
                      | _ -> 0
                  let minOffset = List.fold (fun off (f:IFieldDefinition) -> 
                                                    if int f.Offset < off then int f.Offset else off) minOffset fields
                  typesMap.Add(typeDef, td)
                  typeNameMap.Add(td.Name, td)
                  topDecls <- C.Top.TypeDecl (td) :: topDecls
                  
                  let trField isSpec (f:IFieldDefinition) =
                    let (fldMarkedVolatile, fldDeclaredAsPointer) = 
                      match f with
                        | :? Microsoft.Cci.Ast.FieldDefinition as fd -> (fd.FieldDeclaration.IsVolatile,
                                                                         match fd.FieldDeclaration.Type with
                                                                           | :? VccPointerTypeExpression -> true
                                                                           | :? VccArrayTypeExpression -> true
                                                                           | _ -> false )
                        | _ -> false, false
                    let ptrDeclaredAsVolatile = 
                      match f.Type.ResolvedType with
                        | :? IPointerType as pt -> VccCompilationHelper.IsVolatilePointer(pt)
                        | _ -> false
                    let (fldVolatile, pointsToVolatile) =
                      if fldDeclaredAsPointer then (ptrDeclaredAsVolatile, fldMarkedVolatile)
                      else (fldMarkedVolatile, ptrDeclaredAsVolatile)
                      
                    let t = 
                      match this.DoType (f.Type) with
                        | C.PtrSoP(typ, isSpec) when pointsToVolatile -> C.Type.MkPtr(C.Type.Volatile(typ), isSpec)
                        | C.Type.Array(typ, size) when pointsToVolatile -> C.Type.Array(C.Type.Volatile(typ), size)
                        | typ -> typ
                    let tok = token f
                    let isSpec = isSpec || td.IsSpec
                    let isSpec =
                      match t with
                        | C.Type.Map(_,_) when not isSpec ->
                          helper.Error(tok, 9632, "fields of map type must be declared as specification fields", None)
                          true
                        | _ -> isSpec
                    let res =
                      { Name = f.Name.Value
                        Token = tok
                        Type = t
                        Parent = td
                        IsSpec = isSpec
                        IsVolatile = fldVolatile
                        Offset =
                          if f.IsBitField then                               
                            C.FieldOffset.BitField (int f.Offset - minOffset, int (MemberHelper.GetFieldBitOffset f), int f.BitLength)
                          else
                            C.FieldOffset.Normal (int f.Offset - minOffset)
                        CustomAttr = convCustomAttributes (token f) f.Attributes
                        UniqueId = C.unique()
                      } : C.Field               
                    fieldsMap.Add(f, res)
                    res
                        
                  let trMember isSpec (m:ITypeDefinitionMember) =
                    match m with
                    | :? IFieldDefinition as f -> Some(trField isSpec f)
                    | :? NestedTypeDefinition as t -> 
                      this.DoTypeDefinition t
                      None
                    | _ -> None
                    
                  let rec trMembers isSpec = function
                  | [] -> []
                  | m :: ms ->
                    match trMember isSpec m with
                    | None -> trMembers isSpec ms
                    | Some(f) -> f :: trMembers isSpec ms
                  
                  //if (contract <> null) then contract.HasErrors |> ignore

                  match fields with
                    | [] when not (VccScopedName.IsGroupType(typeDef)) && not td.IsSpec ->
                      if contract <> null && Seq.length contract.ContractFields > 0 then
                        helper.Error (tok, 9620, "need at least one physical field in structure, got only spec fields", None)
                      // forward declaration
                      C.Type.Ref td
                    | _ ->
                      td.Fields <- trMembers false members

                      if td.SizeOf < 1  && not td.IsSpec then
                        helper.Oops(td.Token, "type " + td.Name + " smaller than 1 byte!")
                        die()
                                            
                      if contract <> null then
                        td.Fields <- td.Fields @ [ for f in contract.ContractFields -> trField true f ]
                        if not (Visitor.CheckHasError(contract)) then
                          finalActions.Enqueue (fun () ->
                            td.Invariants <- [ for inv in contract.Invariants -> this.DoInvariant inv])
                       
                      let reportErrorForDuplicateFields fields =
                        let seenFields = new Dict<string,C.Field>()
                        let addFieldOrReportError (f:C.Field) =
                          if f.Name = "" then ()
                          else 
                            match seenFields.TryGetValue f.Name with
                              | true, f' -> 
                                let msg = "'" + f.Name + "' : '" + (if td.Kind = C.TypeKind.Struct then "struct" else "union") + "' member redefinition"
                                helper.Error(f.Token, 9677, msg, Some(f'.Token))
                              | _ -> seenFields.Add(f.Name, f)
                        List.iter addFieldOrReportError fields
                        
                      reportErrorForDuplicateFields td.Fields
                      C.Type.Ref (td)
                
            | v ->
              oopsLoc typeDef ("bad value of typecode: " + v.ToString())
              die()

    // Range checks are added later, in transformers
    member this.GetExpressionAndBindings (arguments:System.Collections.Generic.IEnumerable<IExpression>, kind:C.QuantKind, methodCall:IMethodCall) =
      let getReturnStmt (blockStmt:IBlockStatement) : IExpression =
        let mutable found = false
        let mutable retval = CodeDummy.Expression
        for stmt in blockStmt.Statements do
          if (found = false) then
            let retStmt = stmt :?> IReturnStatement
            if (retStmt <> null) then
              if (retStmt.Expression <> null) then 
                found <- true
                retval <- retStmt.Expression
        retval
        
      let mutable (bindings:list<C.Variable>) = []
      let argEnumerator = arguments.GetEnumerator()
      if (argEnumerator.MoveNext() <> true) then
        (cTrue, bindings, [])
      else 
        let anonymousDelegate = (argEnumerator.Current :?> IAnonymousDelegate)
        if (anonymousDelegate = null) then
          oopsLoc methodCall "found errors in quantifier body, faking it to be 'true'"
          (cTrue, bindings, [])
        else
          let parEnumerator = anonymousDelegate.Parameters.GetEnumerator()
          if (parEnumerator.MoveNext() <> true) then
            oopsLoc anonymousDelegate ("found errors in quantifier body, faking it to be 'true'")
            (cTrue, bindings, [])
          else
            let par = parEnumerator.Current
            let parType = par.Type.ResolvedType
            let var = C.Variable.CreateUnique par.Name.Value (this.DoType (parType)) C.VarKind.QuantBound
            localsMap.Add (par, var)
            
            let name = par.Name.Value
            match localsMap.TryGetValue name with
              | true, prev ->
                helper.Error(token par, 9675, "Quantified variable '" + name + "' clashes with earlier declaration", None)
              | false, _ ->
                localsMap.Add (name, var)
            
            bindings <- var :: bindings
            let body = getReturnStmt(anonymousDelegate.Body)
            if Visitor.CheckHasError(body) then
              oopsLoc body "found errors in quantifier body, faking it to be 'true'"
              (cTrue, bindings, [])
            else
              let resultExpr = this.DoExpression (body)
              let rec collect addVars = function
                | C.Quant (_, { Variables = vars; Body = b }) ->
                  collect (vars @ addVars) b
                | _ -> addVars
              let addVars = collect [] resultExpr
              for v in addVars do
                localsMap.Add (v.Name, v)
              let triggers = this.GetTriggers(methodCall)
              for v in var :: addVars do
                localsMap.Remove v.Name |> ignore
              (resultExpr, bindings, triggers)

    member this.GetTriggers (o:obj) =
      let doTrigger (expr:Expression) = 
        let ec = { Token = token expr; Type = this.DoType (expr.Type) } : C.ExprCommon
        match expr with
        | :? VccLabeledExpression as lblExpr ->
          let lbl = C.Expr.Label(ec, {Name = lblExpr.Label.Name.Value})
          match lblExpr.Expression with
            | :? DummyExpression -> C.Expr.Macro(ec, "labeled_expr", [lbl])
            | e -> 
              let e' = this.DoExpression(e.ProjectAsIExpression())
              C.Expr.Macro(ec, "labeled_expr", [lbl; e'])
        | _ -> this.DoExpression(expr.ProjectAsIExpression())
      let triggers = contractProvider.GetTriggersFor(o)
      match triggers with
        | null -> []
        | _ -> 
          [ for triggerExprs in triggers -> 
            [ for triggerExpr in triggerExprs -> doTrigger triggerExpr] ]
    
    member this.DoQuant (methodCall:IMethodCall) = 
      let methodToCall = methodCall.MethodToCall.ResolvedMethod
      let methodName = methodToCall.Name.Value
      let kind = match methodName with 
                   | "Exists" -> C.Exists
                   | "ForAll" -> C.Forall
                   | "Lambda" -> C.Lambda
                   | _ -> die()
      let (body, bindings, triggers) = this.GetExpressionAndBindings(methodCall.Arguments, kind, methodCall)
      { 
        Variables = bindings
        Triggers = triggers
        Condition = None // TODO: figure out where condition is in code model
        Body = body
        Kind = kind
      } : C.QuantData 
    
    member this.DoLoopContract (loop:IStatement) =
      let contract = contractProvider.GetLoopContractFor loop
      if (contract <> null) then Visitor.CheckHasError(contract) |> ignore
      let conds =
        if contract = null then []
        else [ for i in contract.Invariants -> C.Expr.MkAssert (this.DoExpression i.Condition) ] @
             [ for w in contract.Writes -> 
                  let set = this.DoExpression w                    
                  C.Expr.MkAssert (C.Expr.Macro ({ set.Common with Type = C.Type.Bool }, "loop_writes", [set]))]
      C.Expr.Macro (C.bogusEC, "loop_contract", conds)

    member this.DoneVisitingAssembly() : unit = 

      if globalsType <> null then
        let contract = contractProvider.GetTypeContractFor globalsType
        if contract <> null then
          Visitor.CheckHasError(contract) |> ignore
          for inv in contract.Invariants do
            if inv.IsAxiom then
              topDecls <- C.Top.Axiom (this.DoExpression inv.Condition) :: topDecls
              
      topDecls <- List.rev topDecls        

    member this.VisitOnly (assembly:IAssembly, fnNames) : unit =
      let isRequired sym =
        let syms = [ "malloc" ]
        List.exists (fun elem -> elem = sym) syms
      let ns = assembly.NamespaceRoot
      doingEarlyPruning <- true
      requestedFunctions <- Seq.toList fnNames
      for n in ns.Members do 
        let ncmp s = s = n.Name.Value
        if n.Name.Value.StartsWith("_vcc") || List.exists ncmp requestedFunctions || isRequired n.Name.Value then
          n.Dispatch(this)
      this.DoneVisitingAssembly()


    // The idea is to only do dispatch into children, and not put any complicated
    // logic inside Visit(***) methods. The logic should be up, in the Do*** methods.
    // This is however not yet the case.
    interface IVccCodeVisitor with    

      member this.Visit (arrayTypeReference:IArrayTypeReference) : unit = assert false

      member this.Visit (assembly:IAssembly) : unit =
        let ns = assembly.NamespaceRoot
        ns.Dispatch(this)
        this.DoneVisitingAssembly()        
                          
      member this.Visit (assemblyReference:IAssemblyReference) : unit = assert false

      member this.Visit (customAttribute:ICustomAttribute) : unit = assert false

      member this.Visit (customModifier:ICustomModifier) : unit = assert false

      member this.Visit (eventDefinition:IEventDefinition) : unit = assert false

      member this.Visit (fieldDefinition:IFieldDefinition) : unit = assert false

      member this.Visit (fieldReference:IFieldReference) : unit = assert false

      member this.Visit (fileReference:IFileReference) : unit = assert false
      
      member this.Visit (aliasForType:IAliasForType) : unit = assert false

      member this.Visit (functionPointerTypeReference:IFunctionPointerTypeReference) : unit =
        let meth = functionPointerTypeReference :> ISignature
        let cont =
          match Seq.toList meth.Parameters with
            | x :: _ -> x.ContainingSignature
            | _ -> meth
        let td =
          match functionPointerMap.TryGetValue cont with
            | true, td -> td
            | _ ->
              this.DoMethod(meth, true)
              let fndecl = this.LookupMethod meth
              let td = 
                { 
                  Token = C.bogusToken
                  Kind = C.FunctDecl fndecl
                  Name = fndecl.Name
                  Fields = []
                  Invariants = []
                  CustomAttr = []
                  SizeOf = C.Type.ObjectT.SizeOf
                  IsNestedAnon = false
                  GenerateEquality = CAST.StructEqualityKind.NoEq
                  GenerateFieldOffsetAxioms = false
                  Parent = None
                  IsVolatile = false
                  IsSpec = false
                  UniqueId = C.unique()
                 } : C.TypeDecl
              topDecls <- C.Top.TypeDecl td :: topDecls
              functionPointerMap.Add (cont, td)
              td
        typeRes <- C.Type.PhysPtr (C.Type.Ref td)

      member this.Visit (genericMethodInstanceReference:IGenericMethodInstanceReference) : unit = assert false

      member this.Visit (genericMethodParameter:IGenericMethodParameter) : unit = 
        typeRes <- C.Type.TypeVar({ Name = genericMethodParameter.Name.Value })

      member this.Visit (genericMethodParameterReference:IGenericMethodParameterReference) : unit = assert false

      member this.Visit (globalFieldDefinition:IGlobalFieldDefinition) : unit =
        this.DoGlobal globalFieldDefinition |> ignore
                                  
      member this.Visit (globalMethodDefinition:IGlobalMethodDefinition) : unit =
        globalsType <- globalMethodDefinition.ContainingTypeDefinition
        match globalMethodDefinition.Name.Value with
          | "_vcc_in_state" | "_vcc_approves" | "_vcc_deep_struct_eq" | "_vcc_shallow_struct_eq" | "_vcc_known" | "_vcc_is_low" | "_vcc_test_classifier" | "_vcc_current_context" -> ()
          | _ -> this.DoMethod (globalMethodDefinition, false)

      member this.Visit (genericTypeInstanceReference:IGenericTypeInstanceReference) : unit =
        let rec isAdmissibleMapDomainType = function
          | C.Volatile t -> isAdmissibleMapDomainType t
          | C.Ref _ 
          | C.TypeVar _
          | C.Array _
          | C.Void -> false
          | _ -> true

        if genericTypeInstanceReference.GenericType.ToString () = SystemDiagnosticsContractsCodeContractMap then
          match [ for t in genericTypeInstanceReference.GenericArguments -> this.DoType t ] with
            | [t1; t2] ->
              let domainType = [ for t in genericTypeInstanceReference.GenericArguments -> t ].Head
              let t1 = if isAdmissibleMapDomainType t1 then t1
                       else 
                         helper.Error(token domainType, 9702, "Illegal type '" + t1.ToString() + "' in map domain.")
                         C.Type.Bogus
              typeRes <- C.Type.Map (t1, t2)
            | _ -> assert false
        else
          assert false
        
      member this.Visit (genericTypeParameter:IGenericTypeParameter) : unit = assert false

      member this.Visit (genericTypeParameterReference:IGenericTypeParameterReference) : unit = assert false

      member this.Visit (managedPointerTypeReference:IManagedPointerTypeReference) : unit =
        typeRes <- C.Type.PhysPtr (this.DoType (managedPointerTypeReference.TargetType)) //TODO: Ptr kind

      member this.Visit (marshallingInformation:IMarshallingInformation) : unit = assert false
      
      member this.Visit (constant:IMetadataConstant) : unit = assert false

      member this.Visit (createArray:IMetadataCreateArray) : unit = assert false

      member this.Visit (expression:IMetadataExpression) : unit = assert false

      member this.Visit (namedArgument:IMetadataNamedArgument) : unit = assert false

      member this.Visit (typeOf:IMetadataTypeOf) : unit = assert false

      member this.Visit (methodBody:IMethodBody) : unit = assert false

      member this.Visit (method_:IMethodDefinition) : unit = assert false

      member this.Visit (methodImplementation:IMethodImplementation) : unit = assert false

      member this.Visit (methodReference:IMethodReference) : unit = assert false

      member this.Visit (modifiedTypeReference:IModifiedTypeReference) : unit = 
        modifiedTypeReference.UnmodifiedType.Dispatch(this)
        //TODO: this may loose modifiers that we need

      member this.Visit (module_:IModule) : unit = assert false

      member this.Visit (moduleReference:IModuleReference) : unit = assert false

      member this.Visit (namespaceAliasForType:INamespaceAliasForType) : unit = assert false

      member this.Visit (namespaceTypeDefinition:INamespaceTypeDefinition) : unit =
        this.DoTypeDefinition namespaceTypeDefinition
            
      member this.Visit (namespaceTypeReference:INamespaceTypeReference) : unit =
        this.DoTypeDefinition namespaceTypeReference.ResolvedType

      member this.Visit (nestedAliasForType:INestedAliasForType) : unit = assert false

      member this.Visit (nestedTypeDefinition:INestedTypeDefinition) : unit =
        if not (nestedTypeDefinition.ContainingTypeDefinition.ToString().StartsWith(SystemDiagnosticsContractsCodeContract))
           && (nestedTypeDefinition.ContainingTypeDefinition.ToString()) <> "__Globals__" then
          nestedTypeDefinition.ContainingTypeDefinition.Dispatch(this)
        this.DoTypeDefinition nestedTypeDefinition

      member this.Visit (nestedTypeReference:INestedTypeReference) : unit = assert false

      member this.Visit (nestedUnitNamespace:INestedUnitNamespace) : unit = assert false

      member this.Visit (nestedUnitNamespaceReference:INestedUnitNamespaceReference) : unit = assert false

      member this.Visit (nestedUnitSetNamespace:INestedUnitSetNamespace) : unit = assert false

      member this.Visit (parameterDefinition:IParameterDefinition) : unit = assert false

      member this.Visit (parameterTypeInformation:IParameterTypeInformation) : unit = assert false

      member this.Visit (pointerTypeReference:IPointerTypeReference) : unit =
        let isSpec = match pointerTypeReference with
                      | :? IPointerType as pt -> VccCompilationHelper.IsSpecPointer(pt)
                      | _ -> false // TODO: Ptr kind
        typeRes <- C.Type.MkPtr (this.DoType (pointerTypeReference.TargetType), isSpec)

      member this.Visit (propertyDefinition:IPropertyDefinition) : unit = assert false

      member this.Visit (resourceReference:IResourceReference) : unit = assert false

      member this.Visit (rootUnitNamespace:IRootUnitNamespace) : unit =
        Seq.iter (fun (m : INamespaceMember) -> m.Dispatch(this)) rootUnitNamespace.Members

      member this.Visit (rootUnitNamespaceReference:IRootUnitNamespaceReference) : unit = assert false

      member this.Visit (rootUnitSetNamespace:IRootUnitSetNamespace) : unit = assert false

      member this.Visit (securityAttribute:ISecurityAttribute) : unit = assert false

      member this.Visit (unitSet:IUnitSet) : unit = assert false

      member this.Visit (win32Resource:IWin32Resource) : unit = assert false
    
      member this.Visit (addition:IAddition) : unit =
        if (addition.LeftOperand.Type :? IPointerType) || (addition.RightOperand.Type :? IPointerType) then
          exprRes <- C.Expr.Macro (this.ExprCommon addition, "ptr_addition", 
                                   [this.DoExpression addition.LeftOperand; this.DoExpression addition.RightOperand])
        else
          this.DoBinary ("+", addition, addition.CheckOverflow)

      member this.Visit (addressableExpression:IAddressableExpression) : unit =
        this.DoField addressableExpression addressableExpression.Instance addressableExpression.Definition

      member this.Visit (addressDereference:IAddressDereference) : unit =
        exprRes <- C.Expr.Deref (this.ExprCommon addressDereference, this.DoExpression (addressDereference.Address))

      member this.Visit (addressOf:IAddressOf) : unit =
        exprRes <- C.Expr.Macro (this.ExprCommon addressOf, "&", [this.DoExpression (addressOf.Expression)])

      member this.Visit (anonymousMethod:IAnonymousDelegate) : unit = assert false

      member this.Visit (arrayIndexer:IArrayIndexer) : unit = assert false

      member this.Visit (assertStatement:IAssertStatement) : unit =
        let cond = this.DoExpression (assertStatement.Condition)
        stmtRes <- C.Expr.Assert({ cond.Common with Type = C.Type.Void }, cond, this.GetTriggers assertStatement)

      member this.Visit (assignment:IAssignment) : unit =
        let target = this.DoExpression (assignment.Target)
        let source = this.DoExpression (assignment.Source)
        exprRes <- C.Expr.Macro (this.ExprCommon assignment, "=", [target; source])

      member this.Visit (assumeStatement:IAssumeStatement) : unit =
        stmtRes <- C.Expr.MkAssume (this.DoExpression (assumeStatement.Condition))

      member this.Visit (baseClassReference:IBaseClassReference) : unit = assert false

      member this.Visit (bitwiseAnd:IBitwiseAnd) : unit =
        this.DoBinary ("&", bitwiseAnd)

      member this.Visit (bitwiseOr:IBitwiseOr) : unit =
        this.DoBinary ("|", bitwiseOr)

      member this.Visit (blockExpression:IBlockExpression) : unit =
        let savedLocalVars = localVars
        localVars <- []
        let stmts = [for s in blockExpression.BlockStatement.Statements -> this.DoStatement s]
        let expr = this.DoExpression blockExpression.Expression
        exprRes <- C.Expr.Block (this.ExprCommon blockExpression, localVars @ stmts @ [expr], None)
        localVars <- savedLocalVars
        
      member this.Visit (block:IBlockStatement) : unit =
        stmtRes <- this.DoBlock(block) 
        
      member this.Visit (breakStatement:IBreakStatement) : unit = 
        stmtRes <- C.Expr.Macro(this.StmtCommon breakStatement, "break", [])

      member this.Visit (boundExpression:IBoundExpression) : unit =
        this.DoField boundExpression boundExpression.Instance boundExpression.Definition 
          
      member this.Visit (castIfPossible:ICastIfPossible) : unit = assert false

      member this.Visit (catchClause:ICatchClause) : unit = assert false

      member this.Visit (checkIfInstance:ICheckIfInstance) : unit = assert false

      member this.Visit (constant:ICompileTimeConstant) : unit =
        let ec = this.ExprCommon constant
        exprRes <-
          match ec.Type with
            | C.Type.Integer _ ->
              match constant.Value with
                | :? char as c -> C.Expr.IntLiteral(ec, new bigint((int)c))
                | _ -> C.Expr.IntLiteral (ec, bigint.Parse(constant.Value.ToString ()))
            | C.Type.Bool      -> C.Expr.BoolLiteral (ec, unbox (constant.Value))
            | C.Type.Primitive _ -> C.Expr.Macro(ec, "float_literal", [C.Expr.UserData(ec, constant.Value)])
            | C.Ptr (C.Type.Integer C.IntKind.UInt8) -> C.Expr.Macro (ec, "string", [C.Expr.ToUserData(constant.Value)])
            | _ -> die()

      member this.Visit (conversion:IConversion) : unit =
        match conversion with
          | :? Microsoft.Research.Vcc.VccCast.VccCastArrayConversion as arrConv -> 
            let cmn = this.ExprCommon conversion
            exprRes <- C.Expr.Macro({cmn with Type = C.Type.ObjectT}, "_vcc_as_array",
                                   [this.DoExpression(arrConv.ValueToConvert); this.DoExpression(arrConv.Size)]) 
          | _ -> 
            exprRes <- C.Expr.Cast (this.ExprCommon conversion, 
                                    checkedStatus conversion.CheckNumericRange, 
                                    this.DoExpression (conversion.ValueToConvert))

      member this.Visit (conditional:IConditional) : unit =
        exprRes <- C.Expr.Macro (this.ExprCommon conditional, "ite",
                                 [this.DoExpression conditional.Condition;
                                  this.DoExpression conditional.ResultIfTrue;
                                  this.DoExpression conditional.ResultIfFalse])

      member this.Visit (conditionalStatement:IConditionalStatement) : unit =
        stmtRes <- C.Expr.If (this.StmtCommon conditionalStatement,
                              None,
                              this.DoExpression conditionalStatement.Condition,
                              this.DoStatement conditionalStatement.TrueBranch,
                              this.DoStatement conditionalStatement.FalseBranch)

      member this.Visit (continueStatement:IContinueStatement) : unit = 
        stmtRes <- C.Expr.Macro(this.StmtCommon continueStatement, "continue", [])

      member this.Visit (createArray:ICreateArray) : unit = assert false

      member this.Visit (createDelegateInstance:ICreateDelegateInstance) : unit = assert false

      member this.Visit (createObjectInstance:ICreateObjectInstance) : unit = assert false

      member this.Visit (debuggerBreakStatement:IDebuggerBreakStatement) : unit = assert false

      member this.Visit (defaultValue:IDefaultValue) : unit = assert false

      member this.Visit (division:IDivision) : unit =
        this.DoBinary ("/", division, division.CheckOverflow)
      
      member this.Visit (doUntilStatement:IDoUntilStatement) : unit =
        stmtRes <- C.Expr.Macro (this.StmtCommon doUntilStatement,
                                 "doUntil", 
                                 [this.DoLoopContract doUntilStatement;
                                  this.DoStatement doUntilStatement.Body; 
                                  this.DoExpression doUntilStatement.Condition])

      member this.Visit (emptyStatement:IEmptyStatement) : unit =
        stmtRes <- C.Expr.Comment (this.StmtCommon emptyStatement, "empty")

      member this.Visit (equality:IEquality) : unit =
        this.DoBinary ("==", equality)

      member this.Visit (exclusiveOr:IExclusiveOr) : unit =
        this.DoBinary ("^", exclusiveOr)

      member this.Visit (expression:IExpression) : unit = assert false

      member this.Visit (expressionStatement:IExpressionStatement) : unit =
        let expr = this.DoExpression (expressionStatement.Expression)
        let expr =
          match expr with
            | C.Macro (c, "=", args) -> C.Macro ({ c with Type = C.Void }, "=", args) 
            | expr -> expr
        stmtRes <- expr

      member this.Visit (forEachStatement:IForEachStatement) : unit = assert false

      member this.Visit (forStatement:IForStatement) : unit =
        let doStmts l =
          C.Expr.MkBlock [for s in l -> this.DoStatement s]
        let inits = doStmts forStatement.InitStatements // there can be declarations there, so do it first
        stmtRes <- C.Expr.Macro (this.StmtCommon forStatement,
                                 "for", 
                                 [this.DoLoopContract forStatement;
                                  inits;
                                  this.DoExpression forStatement.Condition;
                                  doStmts forStatement.IncrementStatements;
                                  this.DoStatement forStatement.Body
                                  ])

      member this.Visit (gotoStatement:IGotoStatement) : unit = 
        stmtRes <- C.Goto(this.StmtCommon gotoStatement, { Name = gotoStatement.TargetStatement.Label.Value })

      member this.Visit (gotoSwitchCaseStatement:IGotoSwitchCaseStatement) : unit = assert false

      member this.Visit (getTypeOfTypedReference:IGetTypeOfTypedReference) : unit = assert false

      member this.Visit (getValueOfTypedReference:IGetValueOfTypedReference) : unit = assert false

      member this.Visit (greaterThan:IGreaterThan) : unit =
        this.DoBinary (">", greaterThan)

      member this.Visit (greaterThanOrEqual:IGreaterThanOrEqual) : unit =
        this.DoBinary (">=", greaterThanOrEqual)

      member this.Visit (labeledStatement:ILabeledStatement) : unit = 
        let lblStmt = C.Label(this.StmtCommon labeledStatement, { Name = labeledStatement.Label.Value })
        stmtRes <- C.Expr.MkBlock([lblStmt; this.DoStatement(labeledStatement.Statement)])

      member this.Visit (leftShift:ILeftShift) : unit =
        this.DoBinary ("<<", leftShift)

      member this.Visit (lessThan:ILessThan) : unit =
        this.DoBinary ("<", lessThan)

      member this.Visit (lessThanOrEqual:ILessThanOrEqual) : unit =
        this.DoBinary ("<=", lessThanOrEqual)

      member this.Visit (localDeclarationStatement:ILocalDeclarationStatement) : unit =
        let loc = localDeclarationStatement.LocalVariable
        let declaredAsVolatile = 
          match loc with 
            | :? Microsoft.Research.Vcc.VccLocalDefinition as vcLoc -> vcLoc.IsVolatile
            | _ -> false
        let t = 
          match this.DoType (loc.Type) with
            | C.PtrSoP(t, isSpec) when declaredAsVolatile -> C.Type.MkPtr(C.Volatile(t), isSpec)
            | t -> t
        let varkind =             
          match loc with
            | :?  Microsoft.Research.Vcc.VccLocalDefinition as vcl -> if vcl.IsSpec then C.VarKind.SpecLocal else C.VarKind.Local
            | _ -> C.VarKind.Local
        let var = C.Variable.CreateUnique loc.Name.Value t varkind
        localsMap.Add(loc, var)
        let sc = this.StmtCommon localDeclarationStatement
        let decl = C.Expr.VarDecl (sc, var)
        localVars <- decl :: localVars
        // it seems that if there is an initalizer, it sometimes
        // gets called separatly and the node returned from here is gone.
        // so we return a comment instead of the decl, for if it is lost there is no big deal
        let decl = C.Expr.Comment (sc, "var " + var.ToString())
        let init = localDeclarationStatement.InitialValue
        if init = null then
          stmtRes <- decl
        else
          let assign = C.Expr.Macro (sc, "=", [C.Expr.Ref({ sc with Type = var.Type }, var); 
                                                            this.DoExpression init])
          let assign = if var.Kind = C.VarKind.SpecLocal then C.Expr.Macro(assign.Common, "spec", [assign]) else assign
          stmtRes <- assign

      member this.Visit (lockStatement:ILockStatement) : unit = assert false

      member this.Visit (logicalNot:ILogicalNot) : unit =
        this.DoUnary ("!", logicalNot, false)

      member this.Visit (makeTypedReference:IMakeTypedReference) : unit = assert false

      member this.Visit (methodCall:IMethodCall) : unit =
        let ec = this.ExprCommon methodCall
        let methodToCall = 
          match methodCall with
            // the two don't agree if there is an elipsis in function prototype (i.e. there are additional parameters)
            | :? VccMethodCall as meth -> meth.ResolvedMethod
            | _ -> methodCall.MethodToCall.ResolvedMethod            
        let methodName = methodToCall.Name.Value
        let containingTypeDefinitionName = TypeHelper.GetTypeName(methodToCall.ContainingTypeDefinition)        

        let opMap = Map.ofList [ "op_Equality", ("==", false) ; "op_Inequality", ("!=", false); "op_Addition", ("+", false);
                                 "op_Subtraction", ("-", false); "op_Division", ("/", true); "op_Modulus", ("%", true);
                                 "op_Multiply", ("*", false); "op_LessThan", ("<", false); "op_LessThanOrEqual", ("<=", false);
                                 "op_GreaterThan", (">", false); "op_GreaterThanOrEqual", (">=", false)
                               ]

        this.EnsureMethodIsVisited(methodToCall)

        let (|MapTypeString|_|) = function
          | (s:string) when s.StartsWith(SystemDiagnosticsContractsCodeContractMap) -> Some ()
          | _ -> None

        let (|BigIntOp|_|) = function
          | "op_Equality" 
          | "op_Inequality" 
          | "op_Addition" 
          | "op_Subtraction" 
          | "op_Division" 
          | "op_Modulus" 
          | "op_Multiply" 
          | "op_LessThan" 
          | "op_LessThanOrEqual" 
          | "op_GreaterThan" 
          | "op_GreaterThanOrEqual" -> Some ()
          | _ -> None

        let (|ObjsetOp|_|) = function
          | "op_BitwiseAnd"
          | "op_BitwiseOr"
          | "op_ExclusiveOr"
          | "op_LessThanOrEqual" -> Some ()
          | _ -> None

        let oopsNumArgs() = oopsLoc methodCall ("unexpected number of arguments for "+ methodName); die ()

        let args() = [ for e in methodCall.Arguments -> this.DoExpression e]       

        let trBigIntOp methodName = 
          match args() with
            | [e1; e2] -> 
              let op, isChecked = opMap.[methodName]
              exprRes <- C.Expr.Prim (ec, C.Op(op, if isChecked then C.CheckedStatus.Checked else C.CheckedStatus.Unchecked), [e1; e2])
            | _ -> oopsNumArgs()
            
        let trSetOp methodName =
          match args() with
           | [e1; e2] ->
             match methodName with
               | "op_LessThanOrEqual" -> exprRes <- C.Call(ec, findFunctionOrDie "\\set_in" methodCall, [], [e1; e2])
               | "op_BitwiseAnd"      -> exprRes <- C.Call(ec, findFunctionOrDie "\\set_intersection" methodCall, [], [e1; e2])
               | "op_BitwiseOr"       -> exprRes <- C.Call(ec, findFunctionOrDie "\\set_union" methodCall, [], [e1; e2])
               | "op_ExclusiveOr"     -> exprRes <- C.Call(ec, findFunctionOrDie "\\set_difference" methodCall, [], [e1; e2])
               | _ -> die()
           | _ -> oopsNumArgs()

        let trTrivialCast() =             
          match args() with
            | [e] -> exprRes <- e
            | _ -> oopsNumArgs()

        let trCast() =             
          match args() with
            | [e] -> exprRes <- C.Expr.Cast(ec, C.CheckedStatus.Checked, e)
            | _ -> oopsNumArgs()


        match containingTypeDefinitionName, methodName with
          | SystemDiagnosticsContractsCodeContract, ("Exists" | "ForAll" | "Lambda") ->
            exprRes <- C.Expr.Quant (ec, this.DoQuant (methodCall))
          | SystemDiagnosticsContractsCodeContract, "InLambda" -> exprRes <- C.Expr.Macro (ec, "in_lambda", args())
          | SystemDiagnosticsContractsCodeContractTypedPtr, ("op_Implicit" | "op_Explicit") -> trTrivialCast()
          | SystemDiagnosticsContractsCodeContractTypedPtr, ("op_Equality" | "op_Inequality") -> trBigIntOp methodName
          | SystemDiagnosticsContractsCodeContractBigInt, "op_Implicit" -> trTrivialCast()
          | SystemDiagnosticsContractsCodeContractBigInt, "op_Explicit" -> trCast()
          | SystemDiagnosticsContractsCodeContractBigInt, BigIntOp -> trBigIntOp methodName
          | SystemDiagnosticsContractsCodeContractObjset, ObjsetOp -> trSetOp methodName
          | "Microsoft.Research.Vcc.Runtime", "__noop" -> exprRes <- C.Expr.Macro (ec, "noop", [])
          | MapTypeString, "get_Item" ->
            let th = this.DoExpression methodCall.ThisArgument
            match args() with
              | [x] -> exprRes <- C.Expr.Macro (ec, "map_get", [th; x])
              | _ -> oopsNumArgs()
          | MapTypeString, "set_Item" ->
            let th = this.DoExpression methodCall.ThisArgument
            match args() with
              | [x; y] -> exprRes <- C.Expr.Macro (ec, "map_set", [th; x; y])
              | _ -> oopsNumArgs()
          | ( SystemDiagnosticsContractsCodeContract | SystemDiagnosticsContractsCodeContractTypedPtr | MapTypeString), _ ->
            oopsLoc methodCall ("unexpected method \'" + containingTypeDefinitionName + "." + methodName + "\'"); die()
          | _, "_vcc_in_state" ->
            match args() with
              | [e1; e2] -> exprRes <- C.Expr.Old ({ec with Type = e2.Type}, e1, e2)
              | _ -> oopsNumArgs()
          | _, "_vcc_approves" ->
            match args() with
              | [e1; e2] -> exprRes <- C.Expr.Macro (ec, "approves", [e1; e2])
              | _ -> oopsNumArgs() 
          | _, ("_vcc_deep_struct_eq" | "_vcc_shallow_struct_eq" | "_vcc_known") ->
            match args() with
              | [e1; e2] as args -> exprRes <- C.Expr.Macro(ec, methodName, args)
              | _ -> oopsNumArgs()
          | _, "_vcc_atomic_op" ->
            exprRes <- C.Expr.Macro(ec, "atomic_op",  args())
          | _, "_vcc_atomic_op_result" ->
            exprRes <- C.Expr.Macro(ec, "atomic_op_result", [])
          | _, "_vcc_is_low" ->
            match args() with
              | [e] as args -> exprRes <- C.Expr.Macro (ec, "_vcc_is_low", args)
              | _ -> oopsNumArgs()
          | _, "_vcc_test_classifier" ->
            match args() with
              | [classif;cond] as args -> exprRes <- C.Expr.Macro (ec, "_vcc_test_classifier", args)
              | _ -> oopsNumArgs()
          | _, "_vcc_current_context" ->
            match args() with
              | [] -> exprRes <- C.Expr.Macro (ec, "_vcc_current_context", [])
              | _ -> oopsNumArgs()
          | _ ->
            let args = args()
            let nonVoidParCount = [for p in methodToCall.Parameters do if p.Type.ResolvedType.TypeCode <> PrimitiveTypeCode.Void then yield p].Length;
            if args.Length <> nonVoidParCount && not methodToCall.AcceptsExtraArguments then
              helper.Error(token methodCall, 9636, 
                           "wrong number of arguments in call to function '" + (methodToCall.Name.ToString()) + "'; was " 
                           + (args.Length.ToString()) + ", should be " + (methodToCall.ParameterCount.ToString()), Some(token methodToCall))
            if methodName = "_vcc_is" then
              match args with
                | [_; C.Expr.Call(_, _, _, [C.Expr.Cast(_,_,e)])] ->
                  match e.Type with
                    | C.Ptr(C.Ptr(_)) -> helper.Warning(e.Common.Token, 9107, "'is' applied to a pointer type; this is probably not what you intended")
                    | _ -> ()
                | _ -> ()
            let mtc, tArgs =
              match methodToCall with
                | :? IGenericMethodInstance as gmi -> gmi.GenericMethod.ResolvedMethod, [ for t in gmi.GenericArguments -> this.DoType t ]
                | _ -> methodToCall, []
            exprRes <- C.Expr.Call (ec, this.LookupMethod mtc, tArgs, args)

      member this.Visit (modulus:IModulus) : unit =
        this.DoBinary ("%", modulus, false)

      member this.Visit (multiplication:IMultiplication) : unit =
        this.DoBinary ("*", multiplication, multiplication.CheckOverflow)

      member this.Visit (namedArgument:INamedArgument) : unit = assert false

      member this.Visit (notEquality:INotEquality) : unit =
        this.DoBinary ("!=", notEquality)        

      member this.Visit (oldValue:IOldValue) : unit =
        let ec = this.ExprCommon oldValue
        let findTypeOrDie name =
          match typeNameMap.TryGetValue(name) with
            | (true, f) -> f
            | _ -> oopsLoc oldValue ("cannot find internal type " + name + ". Forgotten #include <vcc.h>?"); die()
        let ts = findTypeOrDie (if helper.Options.Vcc2 then "\\state" else "state_t")
        let expr = this.DoExpression oldValue.Expression
        // the type of expr and old(expr) may disagree in CCI, so we fix it up here
        exprRes <- C.Expr.Old ({ec with Type = expr.Type}, C.Expr.Macro ({ec with Type = C.Type.Ref(ts) }, "prestate", []), expr)

      member this.Visit (onesComplement:IOnesComplement) : unit =
        this.DoUnary ("~", onesComplement, false)

      member this.Visit (outArgument:IOutArgument) : unit = 
        exprRes <- C.Expr.Macro(this.ExprCommon outArgument, "out", [this.DoExpression outArgument.Expression])

      member this.Visit (pointerCall:IPointerCall) : unit = 
        exprRes <- C.Expr.Macro (this.ExprCommon pointerCall, "fnptr_call", 
                                 this.DoExpression pointerCall.Pointer :: 
                                   [for e in pointerCall.Arguments -> this.DoExpression e])

      member this.Visit (refArgument:IRefArgument) : unit = die()

      member this.Visit (resourceUseStatement:IResourceUseStatement) : unit = die()

      member this.Visit (returnValue:IReturnValue) : unit =
        let ec = this.ExprCommon returnValue
        exprRes <- C.Expr.Result ec
        
      member this.Visit (rethrowStatement:IRethrowStatement) : unit = die()

      member this.Visit (returnStatement:IReturnStatement) : unit =
        let expr = returnStatement.Expression
        stmtRes <- C.Return (this.StmtCommon returnStatement, if expr = null then None else Some (this.DoExpression expr))

      member this.Visit (rightShift:IRightShift) : unit =
        this.DoBinary (">>", rightShift)

      member this.Visit (runtimeArgumentHandleExpression:IRuntimeArgumentHandleExpression) : unit = die()

      member this.Visit (sizeOf:ISizeOf) : unit =
       match sizeOf.TypeToSize.ResolvedType with
         | :? IGenericMethodParameter as tVar ->
          let ec = this.ExprCommon sizeOf
          exprRes <- C.Expr.SizeOf(ec, C.Type.TypeVar({ Name = tVar.Name.Value }))
         | _ ->  exprRes <- C.Expr.IntLiteral (this.ExprCommon sizeOf, 
                                               new bigint(int64 (TypeHelper.SizeOfType (sizeOf.TypeToSize.ResolvedType))))

      member this.Visit (specStmt:IVccSpecStatement) : unit =
        let stmt = this.DoStatement specStmt.WrappedStatement
        stmtRes <- C.Macro(stmt.Common, "spec", [stmt])

      member this.Visit (wrapStmt:IVccWrapStatement) : unit =
        let expr = this.DoExpression wrapStmt.Object
        stmtRes <- C.Call(this.StmtCommon wrapStmt, findFunctionOrDie "\\wrap" wrapStmt, [], [expr])

      member this.Visit (unwrapStmt:IVccUnwrapStatement) : unit =
        let expr = this.DoExpression unwrapStmt.Object
        stmtRes <- C.Call(this.StmtCommon unwrapStmt, findFunctionOrDie "\\unwrap" unwrapStmt, [], [expr])

      member this.Visit (unwrappingStmt:IVccUnwrappingStatement) : unit =
        let wrap = findFunctionOrDie "\\wrap" unwrappingStmt
        let unwrap = findFunctionOrDie "\\unwrap" unwrappingStmt
        let cmn = this.StmtCommon unwrappingStmt
        let expr = this.DoExpression(unwrappingStmt.Object)
        let body = this.DoStatement(unwrappingStmt.Body)
        stmtRes <- C.Expr.Block(cmn, [ C.Expr.Call((stmtToken "unwrap(@@)" expr), unwrap, [], [expr]); body; C.Expr.Call((stmtToken "wrap(@@)" expr), wrap, [], [expr]) ], None )

      member this.Visit (atomicStmt : IVccAtomicStatement) : unit =
        let args = [ for arg in atomicStmt.Objects -> this.DoExpression arg ]
        let body = this.DoStatement atomicStmt.Body
        stmtRes <- C.Expr.Atomic (this.StmtCommon atomicStmt, args, body)

      member this.Visit (stackArrayCreate:IStackArrayCreate) : unit = 
        match stackArrayCreate with
        | :? CreateStackArray as createStackArray -> 
          let numberOfElements = this.DoExpression(createStackArray.Size.ProjectAsIExpression())
          let elementType = this.DoType(createStackArray.ElementType.ResolvedType)
          let isSpec = 
            match createStackArray with 
              | :? VccCreateStackArray as vccCreateStackArray -> if vccCreateStackArray.IsSpec then cTrue else cFalse
              | _ -> cFalse
          exprRes <- C.Macro({numberOfElements.Common with Type = C.PhysPtr elementType}, "stack_allocate_array", [numberOfElements; isSpec]) 
        | _ -> assert false

      member this.Visit (subtraction:ISubtraction) : unit =
        this.DoBinary ("-", subtraction, subtraction.CheckOverflow)

      member this.Visit (switchCase:ISwitchCase) : unit = assert false // never encountered during traversal

      member this.Visit (switchStatement:ISwitchStatement) : unit = 
        let doCase (sc : ISwitchCase) =
          let (caseLabel,castExprStmt) =
            if sc.Expression = CodeDummy.Constant then ("default", [])
            else ("case", [this.DoExpression sc.Expression])
          let body = castExprStmt @ [ for stmt in sc.Body -> this.DoStatement stmt]       
          C.Expr.Macro({ C.voidBogusEC () with Token = token sc }, caseLabel, body)
        let condExprStmt = this.DoExpression switchStatement.Expression
        let cases = [for sc in switchStatement.Cases -> doCase sc]
        stmtRes <- C.Expr.Macro(this.StmtCommon switchStatement, "switch", condExprStmt :: cases)

      member this.Visit (targetExpression:ITargetExpression) : unit =
        this.DoField targetExpression targetExpression.Instance targetExpression.Definition

      member this.Visit (thisReference:IThisReference) : unit =
        exprRes <- C.Expr.This (this.ExprCommon thisReference)

      member this.Visit (throwStatement:IThrowStatement) : unit = assert false

      member this.Visit (tryCatchFilterFinallyStatement:ITryCatchFinallyStatement) : unit = assert false

      member this.Visit (tokenOf:ITokenOf) : unit = assert false

      member this.Visit (typeOf:ITypeOf) : unit = assert false

      member this.Visit (unaryNegation:IUnaryNegation) : unit =
        this.DoUnary ("-", unaryNegation, unaryNegation.CheckOverflow)

      member this.Visit (unaryPlus:IUnaryPlus) : unit =
        this.DoUnary ("+", unaryPlus, false)

      member this.Visit (vectorLength:IVectorLength) : unit = assert false

      member this.Visit (whileDoStatement:IWhileDoStatement) : unit =
        let cond = this.DoExpression (whileDoStatement.Condition)
        let body = this.DoStatement (whileDoStatement.Body)
        let cmn = this.StmtCommon whileDoStatement

        match cond with
          | C.Call (_, { Name = "_vcc_atomic" }, _, args) ->
            stmtRes <- C.Expr.Atomic (cmn, args, body)
          | C.Call (_, { Name = "_vcc_expose" }, _, [arg]) ->
            let wrap = findFunctionOrDie "_vcc_wrap" whileDoStatement
            let unwrap = findFunctionOrDie "_vcc_unwrap" whileDoStatement
            stmtRes <- C.Expr.Block(cmn, [ C.Expr.Call((stmtToken "unwrap(@@)" arg), unwrap, [], [arg]);  body; C.Expr.Call((stmtToken "wrap(@@)" arg), wrap, [], [arg]) ], None )
          | _ ->
            let contract = this.DoLoopContract whileDoStatement
            stmtRes <- C.Expr.Macro (cmn, "while", [contract; cond; body])

      member this.Visit (yieldBreakStatement:IYieldBreakStatement) : unit = assert false

      member this.Visit (yieldReturnStatement:IYieldReturnStatement) : unit = assert false       

      member this.Visit (dupValue:IDupValue) : unit = assert false

      member this.Visit (popValue:IPopValue) : unit = assert false

      member this.Visit (pushStmt:IPushStatement) : unit = assert false