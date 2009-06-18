//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Resources;
using Microsoft.Cci.Ast;
using Microsoft.Research.Vcc.Preprocessing;
using System.Text;
using System.Diagnostics.SymbolStore;
using Microsoft.Cci;
using Microsoft.Research.Vcc.Parsing;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc {

  /// <summary>
  /// An object that represents a source document, such as file, which is parsed by a Spec# compiler to produce the Spec# specific object model
  /// from which the language agnostic object model can be obtained.
  /// </summary>
  public interface IVccSourceDocument : ISourceDocument {
    /// <summary>
    /// The Vcc compilation part that corresponds to this Vcc source document.
    /// </summary>
    VccCompilationPart VccCompilationPart {
      get;
      // ^ ensures result.SourceLocation.SourceDocument == this;
    }
  }

  public sealed class VccCompilation : Compilation {

    /// <summary>
    /// Do not use this constructor unless you are implementing the Compilation property of the Module class.
    /// I.e. to construct a Compilation instance, construct a Module instance and use its Compilation property. 
    /// </summary>
    internal VccCompilation(ISourceEditHost hostEnvironment, Unit result, VccOptions options, IEnumerable<CompilationPart> parts)
      : base(hostEnvironment, result, options)
      //^ requires result is VccModule || result is VccAssembly;
    {
      this.parts = parts;
    }

    protected override List<CompilationPart> GetPartList() {
      return new List<CompilationPart>(this.parts);
    }
    readonly IEnumerable<CompilationPart> parts;

    internal readonly VccOptions options = new VccOptions();

    public override Compilation UpdateCompilationParts(IEnumerable<CompilationPart> parts) {
      VccAssembly/*?*/ oldAssembly = this.Result as VccAssembly;
      if (oldAssembly != null) {
        VccAssembly newAssembly = new VccAssembly(oldAssembly.Name, oldAssembly.Location, this.HostEnvironment, this.options, oldAssembly.AssemblyReferences, oldAssembly.ModuleReferences, parts);
        return newAssembly.Compilation;
      }
      //^ assume this.Result is VccModule; //follows from constructor precondition and immutability.
      VccModule oldModule = (VccModule)this.Result;
      VccModule newModule = new VccModule(oldModule.Name, oldModule.Location, this.HostEnvironment, this.options, Dummy.Assembly, oldModule.AssemblyReferences, oldModule.ModuleReferences, parts);
      return newModule.Compilation;
    }
  }

  public sealed class VccCompilationPart : CompilationPart {

    public VccCompilationPart(VccCompilationHelper helper, ISourceLocation sourceLocation)
      : base(helper, sourceLocation, new VccGlobalDeclarationContainerClass(helper.Compilation.HostEnvironment))
      //^ requires sourceLocation.SourceDocument is VccCompositeDocument;
    {
    }

    internal VccArray GetFixedSizeArrayType(ITypeDefinition elementType, uint numberOfElements) {
      uint arraySizeInBytes = numberOfElements * TypeHelper.SizeOfType(elementType);
      VccArray/*?*/ result;
      lock (this) {
        Dictionary<ITypeDefinition, VccArray>/*?*/ arrayTypeTable2;
        if (this.arrayTypeTable.TryGetValue(arraySizeInBytes, out arrayTypeTable2)) {
          //^ assume arrayTypeTable2 != null;
          if (arrayTypeTable2.TryGetValue(elementType, out result)) {
            //^ assume result != null;
            return result;
          }
        } else {
          arrayTypeTable2 = new Dictionary<ITypeDefinition, VccArray>();
          this.arrayTypeTable.Add(arraySizeInBytes, arrayTypeTable2);
        }
        NameDeclaration fieldName = new NameDeclaration(this.Helper.NameTable.GetNameFor("_ElementType"), SourceDummy.SourceLocation);
        // Mangle the new type's name. Add an underscore to avoid name clash. 
        NameDeclaration typeName = new NameDeclaration(this.Helper.NameTable.GetNameFor("_FixedArrayOfSize"+arraySizeInBytes+"_"+ elementType), SourceDummy.SourceLocation);
        FieldDeclaration dummyField = 
          new FieldDeclaration(null, FieldDeclaration.Flags.Unsafe, TypeMemberVisibility.Private, TypeExpression.For(elementType), fieldName, null, SourceDummy.SourceLocation);
        List<ITypeDeclarationMember> members = new List<ITypeDeclarationMember>(1);
        members.Add(dummyField);
        result = new VccArray(typeName, members, arraySizeInBytes);
        result.SetContainingTypeDeclaration(this.GlobalDeclarationContainer, true);
        arrayTypeTable2.Add(elementType, result);
      }
      this.VccHelper.AddFixedSizeArrayToPointerMapEntry(result.TypeDefinition, PointerType.GetPointerType(elementType, this.Compilation.HostEnvironment.InternFactory));
      this.GlobalDeclarationContainer.AddHelperMember(result);
      return result;
    }
    readonly Dictionary<uint, Dictionary<ITypeDefinition, VccArray>> arrayTypeTable = new Dictionary<uint, Dictionary<ITypeDefinition, VccArray>>();

    //^ [MustOverride]
    public override CompilationPart MakeShallowCopyFor(Compilation targetCompilation)
      //^^ ensures result.GetType() == this.GetType();
    {
      if (this.Compilation == targetCompilation) return this;
      ISourceLocation sloc = this.SourceLocation;
      VccCompositeDocument/*?*/ oldDocument = sloc.SourceDocument as VccCompositeDocument;
      //^ assume oldDocument != null; //follows from constructor precondition and immutability of sloc
      CompilationPart result = oldDocument.MakeShallowCopyFor(targetCompilation).VccCompilationPart;
      //^ assume result.GetType() == this.GetType();
      return result;
    }

    public override INamespaceDeclarationMember/*?*/ ParseAsNamespaceDeclarationMember(ISourceLocation sourceLocationBeforeEdit, ISourceDocumentEdit edit)
      //^^ requires this.SourceLocation.SourceDocument == sourceLocationBeforeEdit.SourceDocument;
      //^^ requires this.SourceLocation.SourceDocument == edit.SourceLocation.SourceDocument;
      //^^ requires sourceLocationBeforeEdit.Contains(edit.SourceLocation);
      //^^ requires edit.SourceDocumentAfterEdit.IsUpdatedVersionOf(sourceLocationBeforeEdit.SourceDocument);
      //^^ ensures result == null || result is NamespaceDeclarationMember || result is NamespaceTypeDeclaration || result is NestedNamespaceDeclaration;
    {
      //VccCompositeDocument/*?*/ updatedDoc = edit.SourceDocumentAfterEdit as VccCompositeDocument;
      ////^ assume updatedDoc != null; //follows from constructor precondition and immutability of this.SourceLocation
      ////^ assume updatedDoc.IsUpdatedVersionOf(sourceLocationBeforeEdit.SourceDocument); //follows from precondition
      //ISourceLocation updatedSourceLocation = updatedDoc.GetCorrespondingSourceLocation(sourceLocationBeforeEdit);
      //List<IErrorMessage> scannerAndParserErrors = updatedDoc.ScannerAndParserErrors;
      //Parser parser = new Parser(this.Compilation, updatedSourceLocation, scannerAndParserErrors);
      //INamespaceDeclarationMember/*?*/ result = null; // parser.ParseNamespaceDeclarationMember();
      //if (result != null) {
      //  ErrorEventArgs errorEventArguments = new ErrorEventArgs(ErrorReporter.Instance, updatedDoc.UnpreprocessedDocument.SourceLocation, updatedDoc.PreprocessorErrors.AsReadOnly());
      //  this.Compilation.HostEnvironment.ReportErrors(errorEventArguments);
      //  errorEventArguments = new ErrorEventArgs(ErrorReporter.Instance, updatedSourceLocation, scannerAndParserErrors.AsReadOnly());
      //  this.Compilation.HostEnvironment.ReportErrors(errorEventArguments);
      //}
      //return result;
      return null;
    }

    public override RootNamespaceDeclaration ParseAsRootNamespace() {
      ISourceLocation sloc = this.SourceLocation;
      //^ assume sloc.SourceDocument is VccCompositeDocument;  //follows from constructor precondition and immutability of sloc
      VccRootNamespaceDeclaration result = new VccRootNamespaceDeclaration(this, sloc);
      this.rootNamespace = result;
      List<IErrorMessage> scannerAndParserErrors = ((VccCompositeDocument)this.SourceLocation.SourceDocument).ScannerAndParserErrors;
      scannerAndParserErrors.Clear();
      Parser parser = new Parser(this.Compilation, this.SourceLocation, scannerAndParserErrors); //TODO: get options from Compilation
      result.Parse(parser, this);
      ErrorEventArgs errorEventArguments = new ErrorEventArgs(ErrorReporter.Instance, this.SourceLocation, scannerAndParserErrors.AsReadOnly());
      this.Compilation.HostEnvironment.ReportErrors(errorEventArguments);
      errorEventArguments = new ErrorEventArgs(ErrorReporter.Instance, this.UnpreprocessedDocument.SourceLocation, this.PreprocessorErrors);
      this.Compilation.HostEnvironment.ReportErrors(errorEventArguments);
      return result;
    }

    public override ITypeDeclarationMember/*?*/ ParseAsTypeDeclarationMember(ISourceLocation sourceLocationBeforeEdit, ISourceDocumentEdit edit, IName typeName)
      //^^ requires this.SourceLocation.SourceDocument == sourceLocationBeforeEdit.SourceDocument;
      //^^ requires this.SourceLocation.SourceDocument == edit.SourceLocation.SourceDocument;
      //^^ requires sourceLocationBeforeEdit.Contains(edit.SourceLocation);
      //^^ requires edit.SourceDocumentAfterEdit.IsUpdatedVersionOf(sourceLocationBeforeEdit.SourceDocument);
      //^^ ensures result == null || result is TypeDeclarationMember || result is NestedTypeDeclaration;
    {
      //ISourceLocation updatedSourceLocation = edit.SourceDocumentAfterEdit.GetCorrespondingSourceLocation(sourceLocationBeforeEdit); //unsatisfied precondition: requires this.IsUpdatedVersionOf(sourceLocationInPreviousVersionOfDocument.SourceDocument);
      //List<IErrorMessage> scannerAndParserErrors = ((VccCompositeDocument)edit.SourceDocumentAfterEdit).ScannerAndParserErrors;
      //Parser parser = new Parser(this.Compilation, updatedSourceLocation, scannerAndParserErrors);
      //ITypeDeclarationMember/*?*/ result = null; // parser.ParseTypeDeclarationMember(typeName);
      //if (result != null) {
      //  VccCompositeDocument sdoc = (VccCompositeDocument)edit.SourceDocumentAfterEdit;
      //  ErrorEventArgs errorEventArguments = new ErrorEventArgs(ErrorReporter.Instance, sdoc.UnpreprocessedDocument.SourceLocation, sdoc.PreprocessorErrors.AsReadOnly());
      //  this.Compilation.HostEnvironment.ReportErrors(errorEventArguments);
      //  errorEventArguments = new ErrorEventArgs(ErrorReporter.Instance, updatedSourceLocation, scannerAndParserErrors.AsReadOnly());
      //  this.Compilation.HostEnvironment.ReportErrors(errorEventArguments);
      //}
      //return result;
      return null;
    }

    internal List<IErrorMessage> PreprocessorErrors {
      get {
        ISourceLocation sloc = this.SourceLocation;
        VccCompositeDocument/*?*/ sdoc = sloc.SourceDocument as VccCompositeDocument;
        //^ assume sdoc != null; //follows from constructor precondition and immutability of sloc
        return sdoc.PreprocessorErrors;
      }
    }

    public override RootNamespaceDeclaration RootNamespace {
      get
        //^ ensures result is VccRootNamespaceDeclaration;
      {
        if (this.rootNamespace == null) {
          lock (GlobalLock.LockingObject) {
            if (this.rootNamespace == null) {
              ISourceLocation sloc = this.SourceLocation;
              //^ assume sloc.SourceDocument is VccCompositeDocument;  //follows from constructor precondition and immutability of sloc
              this.rootNamespace = new VccRootNamespaceDeclaration(this, sloc);
            }
          }
        }
        //^ assume this.rootNamespace is VccRootNamespaceDeclaration; //The above assignment is the sole initialization of this.rootNamespace
        return this.rootNamespace;
      }
    }

    internal List<IErrorMessage> ScannerAndParserErrors {
      get {
        ISourceLocation sloc = this.SourceLocation;
        VccCompositeDocument/*?*/ sdoc = sloc.SourceDocument as VccCompositeDocument;
        //^ assume sdoc != null; //follows from constructor precondition and immutability of sloc
        return sdoc.ScannerAndParserErrors; 
      }
    }

    public override void SetContainingTypeDeclaration(ITypeDeclarationMember newMember, TypeDeclaration newNsType, bool recurse) {
      TypedefDeclaration/*?*/ typedefDeclaration = newMember as TypedefDeclaration;
      if (typedefDeclaration != null)
        typedefDeclaration.SetContainingTypeDeclaration(newNsType);
      else
        base.SetContainingTypeDeclaration(newMember, newNsType, recurse);
    }

    public override CompilationPart UpdateRootNamespace(RootNamespaceDeclaration rootNamespace)
      //^^ requires this.RootNamespace.GetType() == rootNamespace().GetType();
    {
      List<CompilationPart> newParts = new List<CompilationPart>(this.Compilation.Parts);
      Compilation newCompilation = this.Compilation.UpdateCompilationParts(newParts);
      //^ assume this.Helper is VccCompilationHelper; //The constructor's type signature ensures this.
      VccCompilationHelper helper = (VccCompilationHelper)this.Helper.MakeShallowCopyFor(newCompilation);
      //^ assume rootNamespace is VccRootNamespaceDeclaration; //follows from the precondition and the post condition of this.RootNamespace.
      ISourceLocation sloc = rootNamespace.SourceLocation;
      //^ assume sloc.SourceDocument is VccCompositeDocument; //follows from the precondition of the constructors of VccRootNamespaceDeclaration.
      VccCompilationPart result = new VccCompilationPart(helper, sloc);
      result.rootNamespace = rootNamespace;
      for (int i = 0, n = newParts.Count; i < n; i++) {
        if (newParts[i] == this) { newParts[i] = result; break; }
      }
      return result;
    }

    internal ISourceDocument UnpreprocessedDocument {
      get {
        ISourceLocation sloc = this.SourceLocation;
        VccCompositeDocument/*?*/ sdoc = sloc.SourceDocument as VccCompositeDocument;
        //^ assume sdoc != null; //follows from constructor precondition and immutability of sloc
        return sdoc.UnpreprocessedDocument;
      }
    }

    internal VccCompilationHelper VccHelper {
      get {
        return (VccCompilationHelper)this.Helper;
      }
    }

  }

  public sealed class VccCompilationHelper : LanguageSpecificCompilationHelper {

    public VccCompilationHelper(Compilation compilation)
      : base(compilation, "Vcc") {
    }

    private VccCompilationHelper(Compilation targetCompilation, VccCompilationHelper template) 
      : base(targetCompilation, template) {
    }

    static readonly string SystemDiagnosticsContractsCodeContractTypedPtrString = Microsoft.Cci.Ast.NamespaceHelper.SystemDiagnosticsContractsCodeContractString + ".TypedPtr";
    static readonly string SystemDiagnosticsContractsCodeContractMapString = Microsoft.Cci.Ast.NamespaceHelper.SystemDiagnosticsContractsCodeContractString + ".Map";
    static readonly string SystemDiagnosticsContractsCodeContractBigIntString = Microsoft.Cci.Ast.NamespaceHelper.SystemDiagnosticsContractsCodeContractString + ".BigInt";

    internal void AddFixedSizeArrayToPointerMapEntry(ITypeDefinition fixedSizeArray, IPointerType pointerType) {
      lock (this.arrayToPointerMap) {
        this.arrayToPointerMap.Add(fixedSizeArray, pointerType);
      }
    }
    private readonly Dictionary<ITypeDefinition, IPointerType> arrayToPointerMap = new Dictionary<ITypeDefinition, IPointerType>();

    internal IPointerType/*?*/ ArrayPointerFor(ITypeDefinition fixedSizeArray) {
      IPointerType/*?*/ result = null;
      lock (this.arrayToPointerMap) {
        this.arrayToPointerMap.TryGetValue(fixedSizeArray, out result);
      }
      return result;
    }

    //^ [Pure]
    protected override Expression Conversion(Expression expression, ITypeDefinition targetType, bool isExplicitConversion) {

      if (TypeHelper.TypesAreEquivalent(targetType, this.PlatformType.SystemBoolean) &&
          TypeHelper.TypesAreEquivalent(expression.Type, this.PlatformType.SystemBoolean))
        return expression;

      if (TypeHelper.TypesAreEquivalent(targetType, this.PlatformType.SystemBoolean))
        return this.ConversionExpression(expression, this.PlatformType.SystemBoolean.ResolvedType);
      
      if (TypeHelper.TypesAreEquivalent(expression.Type, this.PlatformType.SystemBoolean)) 
        return this.ConversionExpression(expression, targetType);

      if (targetType.IsEnum && TypeHelper.IsPrimitiveInteger(expression.Type))
        return base.Conversion(expression, targetType, true);

      if (TypeHelper.GetTypeName(targetType) == SystemDiagnosticsContractsCodeContractBigIntString && TypeHelper.IsPrimitiveInteger(expression.Type))
        return this.ConversionExpression(expression, targetType);

      IPointerType/*?*/ sourcePointerType = expression.Type as IPointerType;
      if (sourcePointerType != null) {
        if ((isExplicitConversion && TypeHelper.IsPrimitiveInteger(targetType)) || sourcePointerType.TargetType.ResolvedType.TypeCode == PrimitiveTypeCode.Void && (targetType is IPointerType || targetType is IFunctionPointer))
          return this.ConversionExpression(expression, targetType);
        IPointerType/*?*/ targetPointerType = targetType as IPointerType;
        if ((targetPointerType != null) && (isExplicitConversion || this.VcCompatible || targetPointerType.TargetType.ResolvedType.TypeCode == PrimitiveTypeCode.Void))
          return this.ConversionExpression(expression, targetType);
      } else if (TypeHelper.GetTypeName(expression.Type) == SystemDiagnosticsContractsCodeContractTypedPtrString) {
        if (targetType is IPointerType && !TypeHelper.TypesAreEquivalent(targetType.ResolvedType, this.PlatformType.SystemVoidPtr.ResolvedType))
          return this.ConversionExpression(expression, targetType);
      } else { 
        if (targetType is IPointerType || targetType is IFunctionPointer || TypeHelper.GetTypeName(targetType) == SystemDiagnosticsContractsCodeContractTypedPtrString) {
          if (isExplicitConversion && TypeHelper.IsPrimitiveInteger(expression.Type) || expression is CompileTimeConstant && ExpressionHelper.IsIntegralZero((CompileTimeConstant)expression))
            return this.ConversionExpression(expression, targetType);
          sourcePointerType = this.ArrayPointerFor(expression.Type);
        } else if (TypeHelper.IsPrimitiveInteger(targetType)) {
          if (isExplicitConversion && expression.Type is IFunctionPointer)
            return this.ConversionExpression(this.ConversionExpression(expression, this.PlatformType.SystemVoidPtr.ResolvedType), targetType);
          sourcePointerType = this.ArrayPointerFor(expression.Type);
        }
        if (sourcePointerType != null) {
          AddressOf addressOf = new AddressOf(new AddressableExpression(expression), expression.SourceLocation);
          addressOf.SetContainingExpression(expression);
          return this.Conversion(this.Conversion(addressOf, sourcePointerType, true), targetType, isExplicitConversion);
        }
      }

      if (expression.Type == Dummy.Type) {
        IFunctionPointer/*?*/ functionPointer = targetType as IFunctionPointer;
        if (functionPointer != null) return this.ConversionFromMethodGroupToFunctionPointer(expression, functionPointer);
        IMethodDefinition/*?*/ method = this.ResolveToMethod(expression);
        if (method != null) {
          IPointerType/*?*/ pointer = targetType as IPointerType;
          bool intConversion = isExplicitConversion && TypeHelper.IsPrimitiveInteger(targetType);
          if (intConversion) pointer = this.PlatformType.SystemVoidPtr.ResolvedType as IPointerType;
          if (pointer != null && pointer.TargetType.ResolvedType.TypeCode == PrimitiveTypeCode.Void) {
            Expression result = new Conversion(expression, pointer, expression.SourceLocation);
            if (intConversion) result = this.ConversionExpression(result, targetType);
            return result;
          }
        }
      }

      return base.Conversion(expression, targetType, isExplicitConversion);
    }

    private IMethodDefinition/*?*/ ResolveToMethod(Expression expression) {
      VccSimpleName/*?*/ simpleName = expression as VccSimpleName;
      if (simpleName != null)
        return simpleName.Resolve() as IMethodDefinition;
      else {
        AddressOf/*?*/ addressOfExpression = expression as AddressOf;
        if (addressOfExpression != null)
          return addressOfExpression.Address.Definition as IMethodDefinition;
        Parenthesis/*?*/ parenthesis = expression as Parenthesis;
        if (parenthesis != null)
          return this.ResolveToMethod(parenthesis.ParenthesizedExpression);
        return null;
      }
    }

    public override string GetTypeName(ITypeDefinition typeDefinition)
    {
      switch (typeDefinition.TypeCode)
      {
        case PrimitiveTypeCode.Boolean:
          return "bool";
        case PrimitiveTypeCode.Char:
          return "wchar";
        case PrimitiveTypeCode.Int8:
          return "__int8";
        case PrimitiveTypeCode.Int16:
          return "__int16";
        case PrimitiveTypeCode.Int32:
          return "__int32";
        case PrimitiveTypeCode.Int64:
          return "__int64";
        case PrimitiveTypeCode.UInt8:
          return "unsigned __int8";
        case PrimitiveTypeCode.UInt16:
          return "unsigned __int16";
        case PrimitiveTypeCode.UInt32:
          return "unsigned __int32";
        case PrimitiveTypeCode.UInt64:
          return "unsigned __int64";
        default:
          if (TypeHelper.GetTypeName(typeDefinition) == SystemDiagnosticsContractsCodeContractTypedPtrString)
            return "obj_t";
          else
            return base.GetTypeName(typeDefinition);
      }
    }

    protected override bool ReportAreYouMissingACast
    {
      get { return false; }
    }

    protected override Expression ConversionFromMethodGroupToFunctionPointer(Expression expression, IFunctionPointerTypeReference functionPointer) { //TODO: pass in the source context of the conversion
      expression = this.AddAddressOfIfExpressionIsFunctionName(expression);
      return base.ConversionFromMethodGroupToFunctionPointer(expression, functionPointer);
    }

    private Expression AddAddressOfIfExpressionIsFunctionName(Expression expression) {
      IMethodDefinition/*?*/ method = this.ResolveToMethod(expression);
      if (method != null && !(expression is AddressOf)) {
        Expression addressOf = new VccAddressOf(new AddressableExpression(expression), expression.SourceLocation);
        addressOf.SetContainingExpression(expression);
        return addressOf;
      }
      return expression;
    }

    protected override TypeNameFormatter CreateTypeNameFormatter() {
      return new VccTypeNameFormatter();
    }


    /// <summary>
    /// Resolve overloaded methods.
    /// 
    /// We override this method for C arithmetic/comparison semantics. Vcc uses stricter rules
    /// that guard against unsafe arithmetics. We implement C's rules for compatibility reasons. C's
    /// rules are:
    /// 
    /// - If either operand is a long double, then the other one is converted to long double and that 
    /// is the type of the result. 
    /// - Otherwise, if either operand is a double, then the other one is converted to double, and that is the type of the result. 
    /// - Otherwise, if either operand is a float, then the other one is converted to float, and that is the type of the result. 
    /// - Otherwise the integral promotions are applied to both operands and the following conversions are applied: 
    ///    - If either operand is an unsigned long int, then the other one is converted to unsigned long int, and that is the type of the result. 
    ///    - Otherwise, if either operand is a long int, then the other one is converted to long int, and that is the type of the result. 
    ///    - Otherwise, if either operand is an unsigned int, then the other one is converted to unsigned int, and that is the type of the result. 
    ///    - Otherwise, both operands must be of type int, so that is the type of the result. 
    /// </summary>
    /// <param name="candidateMethods">A set of candidate methods.</param>
    /// <param name="arguments"> The actual arguments passed to the overloaded method. </param>
    /// <param name="allowTypeMismatches"> Whether type mismatch is allowed in resolution.</param>
    /// <returns></returns>
    //^ [Pure]
    public override IMethodDefinition ResolveOverload(IEnumerable<IMethodDefinition> candidateMethods, IEnumerable<Expression> arguments, bool allowTypeMismatches) {

      if (this.VcCompatible && !allowTypeMismatches) {
        bool useDouble = false ; // the condition that we shall use float64 op float 64, similar below.
        bool useFloat = false ;
        bool useUnsignedLong = false ; // u 64
        bool useLong = false; // int 64, not C long
        bool useUnsigned = false; // u 32
        bool useInt = false;
        bool notArith = false; // The rules are only in effect if we are dealing with arithmetics or comparison.
        int numOfArgs =0;
        IEnumerator<Expression> enumerator = arguments.GetEnumerator();
        Expression/*?*/ arg1 = null;
        Expression/*?*/ arg2 = null;
        ITypeDefinition/*?*/ type1 = null;
        ITypeDefinition/*?*/ type2 = null;

        if (enumerator.MoveNext()) {
          numOfArgs++; 
          arg1 = enumerator.Current;
          type1 = arg1.Type;
        }
        if (enumerator.MoveNext()) {
          numOfArgs++; 
          arg2 = enumerator.Current;
          type2 = arg2.Type;
        }
        // pointers involved, use verifiedc semantics
        if (type1 is IPointerType || type2 is IPointerType) {
          return base.ResolveOverload(candidateMethods, arguments, allowTypeMismatches);
        }
        bool type1IsIntOrFloat = (type1 == null) || this.IsIntOrBooleanType(type1) || this.IsFloatType(type1);
        bool type2IsIntOrFloat = (type2 == null) || this.IsIntOrBooleanType(type2) || this.IsFloatType(type2);
        // if more than 2 arguments, give up, that is, let the verified c semantics take over.
        if (!enumerator.MoveNext() && type1IsIntOrFloat && type2IsIntOrFloat) {
          if (numOfArgs == 1) {
            //^ assert type1 != null;
            // possibly unary negation, we allow the operand to be unsigned
            if (TypeHelper.TypesAreEquivalent(type1, this.PlatformType.SystemUInt64)) {
              useLong = true;
            }
            if (TypeHelper.TypesAreEquivalent(type1, this.PlatformType.SystemUInt32)) {
              useInt = true;
            }

            // at most one of useint and uselong is true. 
            foreach (IMethodDefinition m in candidateMethods) {
              if (m == this.Compilation.BuiltinMethods.OpInt64 && useLong) {
                return m;
              }

              if (m == this.Compilation.BuiltinMethods.OpInt32 && useInt) {
                return m;
              }
            }
          } else {
            if (type1 == null || type2 == null) {
              return base.ResolveOverload(candidateMethods, arguments, allowTypeMismatches);
            }

            if (TypeHelper.TypesAreEquivalent(type1, this.PlatformType.SystemFloat64)
              || TypeHelper.TypesAreEquivalent(type2, this.PlatformType.SystemFloat64)) {
              useDouble = true;
            } else if (TypeHelper.TypesAreEquivalent(type1, this.PlatformType.SystemFloat32)
              || TypeHelper.TypesAreEquivalent(type2, this.PlatformType.SystemFloat32)) {
              useFloat = true;
            } else if (TypeHelper.TypesAreEquivalent(type1, this.PlatformType.SystemUInt64)
              || TypeHelper.TypesAreEquivalent(type2, this.PlatformType.SystemUInt64)) {
              useUnsignedLong = true;
            } else if (TypeHelper.TypesAreEquivalent(type1, this.PlatformType.SystemInt64)
              || TypeHelper.TypesAreEquivalent(type2, this.PlatformType.SystemInt64)) {
              useLong = true;
            } else if (TypeHelper.TypesAreEquivalent(type1, this.PlatformType.SystemUInt32)
              || TypeHelper.TypesAreEquivalent(type2, this.PlatformType.SystemUInt32)) {
              useUnsigned = true;
            } else if (TypeHelper.TypesAreAssignmentCompatible(type1, this.PlatformType.SystemUInt64.ResolvedType)
                  && TypeHelper.TypesAreAssignmentCompatible(type2, this.PlatformType.SystemUInt64.ResolvedType)) {
              useInt = true;
            } else {
              notArith = true;
            }

            if (!notArith) {
              // at most one of useDouble, useFloat, useUnsignedLong, useLong, useUnsigned, useInt is true
              foreach (IMethodDefinition method in candidateMethods) {
                if (method == this.Compilation.BuiltinMethods.Float64opFloat64 && useDouble) {
                  return method;
                }
                if (method == this.Compilation.BuiltinMethods.Float32opFloat32 && useFloat) {
                  return method;
                }
                if (method == this.Compilation.BuiltinMethods.UInt64opUInt64 && useUnsignedLong) {
                  return method;
                }
                if (method == this.Compilation.BuiltinMethods.Int64opInt64 && useLong) {
                  return method;
                }
                if (method == this.Compilation.BuiltinMethods.UInt32opUInt32 && useUnsigned) {
                  return method;
                }
                if (method == this.Compilation.BuiltinMethods.Int32opInt32 && useInt) {
                  return method;
                }
                // if we dont find any match, then the verifiedc semantics takes over. 
              }
            }
          }
        }
      }

      return base.ResolveOverload(candidateMethods, arguments, allowTypeMismatches);
    }

    public override IEnumerable<ITypeDefinitionMember> GetExtensionMembers(ITypeDefinition type, IName memberName, bool ignoreCase) {
      foreach (ITypeDefinitionMember anonMember in type.GetMembersNamed(Dummy.Name, false)) {
        IFieldDefinition/*?*/ anonField = anonMember as IFieldDefinition;
        if (anonField != null){
          foreach (ITypeDefinitionMember member in anonField.Type.ResolvedType.GetMembersNamed(memberName, false))
            yield return member;
          //^ assume anonField != null;
          foreach (ITypeDefinitionMember member in this.GetExtensionMembers(anonField.Type.ResolvedType, memberName, false))
            yield return member;
        }
      }
      //TODO: for mapping to IL, as well as use of anon structs inside other structs, it is also necessary to override the Instance property of QualifiedName.
    }

    /// <summary>
    /// Returns the collection of methods with the same name as the given method and declared by the same type as the given method (or by a base type)
    /// and that might be called with the given number of arguments.
    /// </summary>
    //^ [Pure]
    public override IEnumerable<IMethodDefinition> GetMethodGroupMethods(IMethodDefinition methodGroupRepresentative, uint argumentCount) {
      if (methodGroupRepresentative.IsForwardReference) return IteratorHelper.GetSingletonEnumerable(methodGroupRepresentative);
      return base.GetMethodGroupMethods(methodGroupRepresentative, argumentCount);
    }

    /// <summary>
    /// Returns a language specific string that corresponds to a source expression that would evaluate to the given type definition when appearing in an appropriate context.
    /// </summary>
    //^ [Pure]
    public override string GetTypeName(ITypeDefinition typeDefinition, NameFormattingOptions formattingOptions) {
      TypeDefinition/*?*/ tdef = typeDefinition as TypeDefinition;
      if (tdef != null) {
        foreach (TypeDeclaration typeDeclaration in tdef.TypeDeclarations) {
          VccArray/*?*/ vcArr = typeDeclaration as VccArray;
          if (vcArr != null) {
            foreach (ITypeDeclarationMember member in vcArr.TypeDeclarationMembers) {
              IFieldDefinition/*?*/ field = member.TypeDefinitionMember as IFieldDefinition;
              if (field != null) {
                uint elementTypeSize = TypeHelper.SizeOfType(field.Type.ResolvedType);
                if (elementTypeSize <= 0) elementTypeSize = 1;
                return this.GetTypeName(field.Type.ResolvedType, formattingOptions) + "[" + vcArr.SizeOf/elementTypeSize +"]";
              }
            }
          }
        }
      }
      return base.GetTypeName(typeDefinition, formattingOptions);
    }

    //^ [Pure]
    public override Expression ImplicitConversion(Expression expression, ITypeDefinition targetType) {

      if (this.VcCompatible) {
        // allow double -> single
        if (TypeHelper.TypesAreEquivalent(expression.Type, this.PlatformType.SystemFloat64)) {
          if (TypeHelper.TypesAreEquivalent(targetType, this.PlatformType.SystemFloat32)) {
            return ExplicitConversion(expression, targetType);
          }
        }
      }

      if (TypeHelper.TypesAreEquivalent(targetType, this.PlatformType.SystemBoolean)) {
        if (TypeHelper.IsPrimitiveInteger(expression.Type) || this.IsFloatType(expression.Type)) {
          Expression convertedTarget = base.ImplicitConversion(expression, targetType);
          // The converted result may not be in {0,1} in the framework. While in C, the
          // value "true" has to be 1. 
          Expression t = new CompileTimeConstant(true, SourceDummy.SourceLocation);
          Expression f = new CompileTimeConstant(false, SourceDummy.SourceLocation);

          Expression result = new Conditional(convertedTarget, t, f, convertedTarget.SourceLocation);
          result.SetContainingExpression(expression);
          t.SetContainingExpression(result);
          f.SetContainingExpression(result);
          return result;
        }
      }
      return base.ImplicitConversion(expression, targetType);
    }

    /// <summary>
    /// Try to unify the given argument type with the given parameter type by replacing any occurrences of type parameters in parameterType with corresponding type
    /// arguments obtained from inferredTypeArgumentsFor. Returns true if unification fails. Updates inferredTypeArgumentsFor with any new inferences made during
    /// a successful unification.
    /// </summary>
    //^ [Pure]
    public override bool InferTypesAndReturnTrueIfInferenceFails(Dictionary<IGenericMethodParameter, ITypeDefinition> inferredTypeArgumentFor, ITypeDefinition argumentType, ITypeDefinition parameterType) {
      bool result = base.InferTypesAndReturnTrueIfInferenceFails(inferredTypeArgumentFor, argumentType, parameterType);
      if (result) {
        IPointerType/*?*/ pType = parameterType as IPointerType;
        if (pType != null) {
          IPointerType/*?*/ apType = argumentType as IPointerType;
          if (apType == null) return true; //Type inference fails
          return this.InferTypesAndReturnTrueIfInferenceFails(inferredTypeArgumentFor, apType.TargetType.ResolvedType, pType.TargetType.ResolvedType);
        }
      }
      return result;
    }

    public override ITypeReference GetPointerTargetType(ITypeDefinition type)
    {
      NestedTypeDefinition/*?*/ nestedType = type as NestedTypeDefinition;
      if (nestedType != null && nestedType.Name.Value.StartsWith("_FixedArrayOfSize", StringComparison.Ordinal)) {
        foreach (ITypeDefinitionMember member in nestedType.Members) {
          IFieldDefinition/*?*/ field = member as IFieldDefinition;
          if (field != null && field.Name.Value == "_ElementType")
            return field.Type;
        }
      }
      return base.GetPointerTargetType(type);
    }

    public override bool IsPointerType(ITypeDefinition type)
    {
      NestedTypeDefinition/*?*/ nestedType = type as NestedTypeDefinition;
      if (nestedType != null && nestedType.Name.Value.StartsWith("_FixedArrayOfSize", StringComparison.Ordinal)) {
        return true;
      }
      return base.IsPointerType(type);
    }

    private bool IsIntOrBooleanType(ITypeDefinition type) {
      if (TypeHelper.TypesAreEquivalent(type, this.PlatformType.SystemBoolean)) {
        return true;
      }
      if (TypeHelper.IsPrimitiveInteger(type)) return true;
      return false;
    }

    private bool IsFloatType(ITypeDefinition type) {
      if (TypeHelper.TypesAreEquivalent(type, this.PlatformType.SystemFloat32) ||
        TypeHelper.TypesAreEquivalent(type, this.PlatformType.SystemFloat64))
        return true;
      return false;
    }

    public override Expression ImplicitConversionInAssignmentContext(Expression expression, ITypeDefinition targetType) {
      return this.ImplicitConversionInAssignmentContext(expression, targetType, this.VcCompatible);
    }

    public Expression ImplicitConversionInAssignmentContext(Expression expression, ITypeDefinition targetType, bool allowUnsafeNumericConversions) {
      if (TypeHelper.TypesAreEquivalent(expression.Type, targetType)) return expression;
      if (allowUnsafeNumericConversions) {
        if (this.IsIntOrBooleanType(expression.Type) && this.IsFloatType(targetType))
          return this.ExplicitConversion(expression, targetType);
        if (this.IsFloatType(expression.Type) && TypeHelper.IsPrimitiveInteger(targetType))
          return this.ExplicitConversion(expression, targetType);
        if (TypeHelper.IsPrimitiveInteger(expression.Type) && TypeHelper.IsPrimitiveInteger(targetType)) {
          object/*?*/ val = expression.Value;
          if (val != null) {
            CompileTimeConstant cconst = new CompileTimeConstant(val, true, true, expression.SourceLocation);
            cconst.SetContainingExpression(expression);
            expression = cconst;
          }
          return this.ExplicitConversion(expression, targetType);
        }
        CompileTimeConstant/*?*/ cst = expression as CompileTimeConstant;
        if (cst != null && targetType.TypeCode == PrimitiveTypeCode.Pointer && ExpressionHelper.IsIntegralZero(cst)) {
          return cst;
        }
      }
      if (targetType.IsEnum && TypeHelper.IsPrimitiveInteger(expression.Type))
        return this.ExplicitConversion(expression, targetType);
      if (expression.Type.IsEnum && TypeHelper.IsPrimitiveInteger(targetType))
        return this.ExplicitConversion(expression, targetType);
      if (TypeHelper.IsPrimitiveInteger(expression.Type) && TypeHelper.IsPrimitiveInteger(targetType)) {
        object/*?*/ val = expression.Value;
        if (val != null) {
          CompileTimeConstant cconst = new CompileTimeConstant(val, true, expression.CouldBeInterpretedAsNegativeSignedInteger, expression.SourceLocation);
          cconst.SetContainingExpression(expression);
          if (this.ImplicitConversionExists(cconst, targetType)) expression = cconst;
        } else if (expression.IntegerConversionIsLossless(targetType))
          return this.ExplicitConversion(expression, targetType);
      }
      return base.ImplicitConversionInAssignmentContext(expression, targetType);
    }

    /// <summary>
    /// Returns true if an implicit conversion is available to convert the value of the given expression to a corresponding value of the given target type.
    /// </summary>
    //^ [Pure]
    public override bool ImplicitConversionExists(Microsoft.Cci.Ast.Expression expression, ITypeDefinition targetType) {
      if (TypeHelper.TypesAreEquivalent(targetType, this.PlatformType.SystemBoolean)) {
        ITypeDefinition sourceType = expression.Type;
        return (sourceType.TypeCode != PrimitiveTypeCode.NotPrimitive || !sourceType.IsStruct)
          && sourceType.TypeCode != PrimitiveTypeCode.Void 
          && !TypeHelper.GetTypeName(sourceType).StartsWith(SystemDiagnosticsContractsCodeContractMapString);
      }
      CompileTimeConstant/*?*/ cconst = expression as CompileTimeConstant;
      if (cconst != null) {
        if (targetType is IPointerType && ExpressionHelper.IsIntegralZero(cconst)) return true;
        // Disable int -> enum so that any enum operation becomes int operation
        if (targetType.IsEnum && ExpressionHelper.IsIntegralZero(cconst)) return false;
        if (TypeHelper.IsUnsignedPrimitiveInteger(cconst.Type) && !TypeHelper.IsUnsignedPrimitiveInteger(targetType) && 
          cconst is CompileTimeConstantWhoseSignDependsOnAnotherExpression)
          return false;
      }
      if (expression.Type == Dummy.Type) {
        IPointerType/*?*/ targetPointerType = targetType as IPointerType;
        if (targetPointerType != null && targetPointerType.TargetType.ResolvedType.TypeCode == PrimitiveTypeCode.Void) {
          if (this.ResolveToMethod(expression) != null) return true;
        }
      }
      return base.ImplicitConversionExists(expression, targetType);
    }

    //^ [Pure]
    public override bool ImplicitConversionExists(ITypeDefinition sourceType, ITypeDefinition targetType) {
      if (TypeHelper.TypesAreEquivalent(targetType, this.PlatformType.SystemBoolean)) {
        return sourceType.TypeCode != PrimitiveTypeCode.NotPrimitive || !sourceType.IsStruct;
      }
      if (TypeHelper.TypesAreEquivalent(sourceType, this.PlatformType.SystemBoolean)) {
        return TypeHelper.IsPrimitiveInteger(targetType);
      }
      IPointerType/*?*/ sourcePointerType = sourceType as IPointerType;
      if (sourcePointerType != null) {
        if (TypeHelper.TypesAreEquivalent(sourcePointerType.TargetType.ResolvedType, sourcePointerType.PlatformType.SystemVoid) && targetType is IPointerType)
          return true;
      } else {
        IPointerType/*?*/ targetPointerType = targetType as IPointerType;
        if (targetPointerType != null) {
          sourcePointerType = this.ArrayPointerFor(sourceType);
          if (sourcePointerType != null) return this.ImplicitConversionExists(sourcePointerType, targetPointerType);
        }
      }
      /*
       * Rule for int -> enum */
      //if (!this.VcCompatible) {
      //  if (targetType.IsEnum && TypeHelper.IsPrimitiveInteger(sourceType))
      //    return this.ImplicitConversionExists(sourceType, targetType.UnderlyingType);
      //}

      /* Rule for enum -> int. */
      // if (this.VcCompatible) {
        if (sourceType.IsEnum && TypeHelper.IsPrimitiveInteger(targetType)) {
          return this.ImplicitConversionExists(sourceType.UnderlyingType.ResolvedType, targetType);
        }
      // }

      return base.ImplicitConversionExists(sourceType, targetType);
    }

    /// <summary>
    /// Returns true if argument has a better implicit conversion to par1type than it has to par2Type.
    /// </summary>
    //^ [Pure]
    protected override bool ImplicitConversionFromArgumentToType1isBetterThanImplicitConversionToType2(Microsoft.Cci.Ast.Expression argument, ITypeDefinition par1Type, ITypeDefinition par2Type) {
      if (par1Type.TypeCode != par2Type.TypeCode){
        if (TypeHelper.IsPrimitiveInteger(par1Type)) {
          if (par2Type.TypeCode == PrimitiveTypeCode.Boolean && TypeHelper.IsPrimitiveInteger(argument.Type) && argument.Type.TypeCode != PrimitiveTypeCode.Boolean)
            return true;
          if (par2Type is IPointerType && argument is CompileTimeConstant && TypeHelper.IsPrimitiveInteger(argument.Type))
            return true;
        } else if (par1Type.IsEnum && TypeHelper.IsPrimitiveInteger(par2Type)) {
          return true;
        }
      }
      return base.ImplicitConversionFromArgumentToType1isBetterThanImplicitConversionToType2(argument, par1Type, par2Type);
    }

    /// <summary>
    /// Returns true if a value of an enumeration type can be implicitly converted to an integer type.
    /// </summary>
    protected override bool ImplicitEnumToIntegerConversionIsAllowed {
      get { return true; }
    }

    //^ [Pure]
    public override LanguageSpecificCompilationHelper MakeShallowCopyFor(Compilation targetCompilation)
      //^^ ensures result.GetType() == this.GetType();
    {
      if (this.Compilation == targetCompilation) return this;
      return new VccCompilationHelper(targetCompilation, this);
    }

    public ITypeDefinition Runtime {
      get {
        if (this.runtime == null)
          this.runtime = this.GetRuntime();
        return this.runtime;
      }
    }
    //^ [Once]
    ITypeDefinition/*?*/ runtime;

    //^ [Confined]
    private ITypeDefinition GetRuntime() {
      IName microsoft  = this.NameTable.GetNameFor("Microsoft");
      IName research = this.NameTable.GetNameFor("Research");
      IName vcc = this.NameTable.GetNameFor("Vcc");
      IName runtime = this.NameTable.GetNameFor("Runtime");

      INamespaceDefinition/*?*/ ns = this.Compilation.UnitSet.UnitSetNamespaceRoot;
      foreach (INamespaceMember member in ns.GetMembersNamed(microsoft, false)) {
        ns = member as INamespaceDefinition;
        if (ns == null) continue;
        foreach (INamespaceMember member2 in ns.GetMembersNamed(research, false)) {
          ns = member2 as INamespaceDefinition;
          if (ns == null) continue;
          foreach (INamespaceMember member3 in ns.GetMembersNamed(vcc, false)) {
            ns = member3 as INamespaceDefinition;
            if (ns == null) continue;
            foreach (INamespaceMember mem in ns.GetMembersNamed(runtime, false)) {
              ITypeDefinition/*?*/ type = mem as ITypeDefinition;
              if (type != null) return type;
            }
          }
        }
      }
      return Dummy.Type;
      //TODO: error if type cannot be found
    }

    internal Dictionary<string, GlobalVariableDeclaration> StringTable = new Dictionary<string, GlobalVariableDeclaration>();

    /// <summary>
    /// Checks if selecting this overload would cause something undesirable to happen. For example "int op uint" may become "long op long" which
    /// is desirable for C# but undesirable for C.
    /// </summary>
    public override bool StandardBinaryOperatorOverloadIsUndesirable(IMethodDefinition standardBinaryOperatorMethod, Expression leftOperand, Expression rightOperand) {
      if (TypeHelper.SizeOfType(leftOperand.Type) == 4 && TypeHelper.SizeOfType(rightOperand.Type) == 4) {
        return standardBinaryOperatorMethod == this.Compilation.BuiltinMethods.Int64opInt64;
      }
      return false;
    }

    /// <summary>
    /// Returns true if the field has (or should have) a compile time value that should be used in expressions whenever the field is referenced.
    /// For example, if field.IsCompileTimeConstant is true then the CLR mandates that the value should be used since the field will have no runtime memory associated with it.
    /// </summary>
    public override bool UseCompileTimeValueOfField(IFieldDefinition field) {
      return field.IsCompileTimeConstant || (field.IsReadOnly && TypeHelper.IsCompileTimeConstantType(field.Type.ResolvedType) && field.CompileTimeValue != Dummy.Constant);
    }

    /// <summary>
    /// Whether the compilation has to be observe Visual C++ rules, which is different
    /// than verifiedc, which borrows many rules from c#, for example, 
    /// conversion. 
    /// </summary>
    public bool VcCompatible {
      get {
        VccOptions/*?*/ vcoptions = this.Compilation.Options as VccOptions;
        if (vcoptions == null) return false;
        else return vcoptions.VCCompatible;
      }
    }

  }

  public sealed class VccTypeNameFormatter : TypeNameFormatter {

    protected override string GetFunctionPointerTypeName(IFunctionPointerTypeReference functionPointerType, NameFormattingOptions formattingOptions) {
      StringBuilder sb = new StringBuilder();
      sb.Append(this.GetTypeName(functionPointerType.Type, formattingOptions));
      bool first = true;
      sb.Append(" (*)(");
      foreach (IParameterTypeInformation par in functionPointerType.Parameters) {
        if (first) first = false; else sb.Append(", ");
        sb.Append(this.GetTypeName(par.Type, formattingOptions));
      }
      sb.Append(')');
      return sb.ToString();
    }

    //^ [Pure]
    protected override string GetNestedTypeName(INestedTypeReference nestedType, NameFormattingOptions formattingOptions) {
      NestedTypeDefinition/*?*/ ntDef = nestedType as NestedTypeDefinition;
      if (ntDef != null) {
        foreach (NestedTypeDeclaration ntDecl in ntDef.TypeDeclarations){
          VccArray/*?*/ vcArray = ntDecl as VccArray;
          if (vcArray != null) {
            foreach (ITypeDeclarationMember member in vcArray.TypeDeclarationMembers) {
              IFieldDefinition/*?*/ field = member.TypeDefinitionMember as IFieldDefinition;
              if (field != null) {
                uint elementTypeSize = TypeHelper.SizeOfType(field.Type.ResolvedType);
                if (elementTypeSize <= 0) elementTypeSize = 1;
                return this.GetTypeName(field.Type.ResolvedType, formattingOptions) + "[" + vcArray.SizeOf/elementTypeSize +"]";
              }
            }
          }
        }
      }
      return base.GetNestedTypeName(nestedType, formattingOptions);
    }

  }

  public sealed class VccUnpreprocessedSourceDocument : PrimarySourceDocument, IVccUnpreprocessedSourceDocument {

    public VccUnpreprocessedSourceDocument(VccCompilationHelper helper, IName name, string location, System.IO.StreamReader streamReader)
      : base(name, location, streamReader) {
      this.helper = helper;
    }

    public VccUnpreprocessedSourceDocument(VccCompilationHelper helper, IName name, string location, string text)
      : base(name, location, text) {
      this.helper = helper;
    }

    public VccUnpreprocessedSourceDocument(VccCompilationHelper helper, string text, SourceDocument previousVersion, int position, int oldLength, int newLength)
      : base(text, previousVersion, position, oldLength, newLength) {
      this.helper = helper;
    }

    readonly VccCompilationHelper helper;

    //public override CompilationPart CompilationPart {
    //  get
    //    //^^ ensures result.SourceLocation.SourceDocument == this;
    //  {
    //    //^ assume false; //unpreprocessed source documents should be fed to the preprocessor only and should never be asked to parse themselves.
    //    throw new System.InvalidOperationException();
    //  }
    //}

    public Preprocessor GetPreprocessorFor(string location, IDictionary<string, string> preprocessorSymbols)
      // ^ requires System.IO.File.Exists(location);
    {
      IName name = this.helper.NameTable.GetNameFor(System.IO.Path.GetFileNameWithoutExtension(location));
      System.IO.StreamReader reader = System.IO.File.OpenText(location);
      VccUnpreprocessedSourceDocument doc = new VccUnpreprocessedSourceDocument(this.helper, name, location, reader);
      VccSourceDocument sdoc = new VccSourceDocument(this.helper, doc, preprocessorSymbols);
      return sdoc.GetAndCacheNewPreprocessor();
    }

    //^ [Pure]
    public override ISourceLocation GetSourceLocation(int position, int length)
      //^^ requires 0 <= position && (position < this.Length || position == 0);
      //^^ requires 0 <= length;
      //^^ requires length <= this.Length;
      //^^ requires position+length <= this.Length;
      //^^ ensures result.SourceDocument == this;
      //^^ ensures result.StartIndex == position;
      //^^ ensures result.Length == length;
    {
      return new PrimarySourceLocation(this, position, length);
    }

    public VccUnpreprocessedSourceDocument GetUpdatedDocument(int position, int length, string updatedText)
      //^ requires 0 <= position && position < this.Length;
      //^ requires 0 <= length && length <= this.Length;
      //^ requires 0 <= position+length && position+length <= this.Length;
    {
      string oldText = this.GetText();
      if (position > oldText.Length) 
        position = oldText.Length; //This should only happen if the source document got clobbered after the precondition was established.
      //^ assume 0 <= position; //Follows from the precondition and the previous statement
      if (position+length > oldText.Length)
        length = oldText.Length-position;
      //^ assume 0 <= position+length; //established by the precondition and not changed by the previous two statements.
      string newText = oldText.Substring(0, position)+updatedText+oldText.Substring(position+length);
      return new VccUnpreprocessedSourceDocument(this.helper, newText, this, position, length, updatedText.Length);
    }

    public override string SourceLanguage {
      get { return "Vcc"; }
    }

    public override Guid DocumentType {
      get { return SymDocumentType.Text; }
    }

    public override Guid Language {
      get { return SymLanguageType.C; }
    }

    public override Guid LanguageVendor {
      get { return SymLanguageVendor.Microsoft; }
    }
  }

  public abstract class VccCompositeDocument : CompositeSourceDocument {

    protected VccCompositeDocument(IName name)
      : base(name) {
    }

    protected VccCompositeDocument(SourceDocument previousVersion, int position, int oldLength, int newLength)
      : base(previousVersion, position, oldLength, newLength) {
    }

    /// <summary>
    /// Makes a shallow copy of this source document (creating a new
    /// </summary>
    //^^ [MustOverride]
    public abstract VccCompositeDocument MakeShallowCopyFor(Compilation targetCompilation);

    internal abstract List<IErrorMessage> PreprocessorErrors { get; }

    internal abstract List<IErrorMessage> ScannerAndParserErrors { get; }

    internal abstract ISourceDocument UnpreprocessedDocument { get; }

    public abstract VccCompilationPart VccCompilationPart {
      get;
      //^ ensures result.SourceLocation.SourceDocument == this;
    }
    
  }

  public abstract class VccCompositeDocument<PrimaryDocumentType, VersionType> : VccCompositeDocument, IVccSourceDocument
    where PrimaryDocumentType : class, IVccUnpreprocessedSourceDocument
  {

    protected VccCompositeDocument(VccCompilationHelper helper, PrimaryDocumentType/*!*/ documentToPreprocess, IDictionary<string, string>/*?*/ preprocessorDefinedSymbols)
      : base(documentToPreprocess.Name)
    {
      this.helper = helper;
      this.documentToPreprocess = documentToPreprocess;
      this.preprocessorDefinedSymbols = preprocessorDefinedSymbols;
    }

    protected VccCompositeDocument(VccCompilationHelper helper, PrimaryDocumentType/*!*/ documentToPreprocess, PreprocessorInformation/*?*/ preprocessorInformation, 
      VccCompositeDocument<PrimaryDocumentType, VersionType> previousVersion, int position, int oldLength, int newLength)
      : base(previousVersion, position, oldLength, newLength)
    {
      this.helper = helper;
      this.documentToPreprocess = documentToPreprocess;
      this.preprocessorInformation = preprocessorInformation;
    }

    //public override CompilationPart CompilationPart {
    //  get { return this.VccCompilationPart; }
    //}

    public override VccCompilationPart VccCompilationPart {
      get
        //^ ensures result.SourceLocation.SourceDocument == this;
      {
        if (this.compilationPart == null) {
          lock (GlobalLock.LockingObject) {
            if (this.compilationPart == null) {
              ISourceLocation sloc = this.SourceLocation;
              //^ assume sloc.SourceDocument is VccCompositeDocument;
              this.compilationPart = new VccCompilationPart(this.helper, sloc);
            }
          }
        }
        return this.compilationPart;
      }
    }
    private VccCompilationPart/*?*/ compilationPart;

    public PrimaryDocumentType/*!*/ DocumentToPreprocess {
      //^ [Pure]
      get { return this.documentToPreprocess; }
      protected set { this.documentToPreprocess = value; }
    }
    private PrimaryDocumentType/*!*/ documentToPreprocess;

    /// <summary>
    /// Returns a source document that represents the replacement of the text of this.DocumentToPreprocess from position to position+length with updatedText.
    /// I.e. the given position and length corresponds to the text of the document before preprocessing, but the resulting
    /// edit is an edit to the document after preprocessing (i.e. this document, not the one that preprocessor consumes).
    /// The compilation containing the compilation part that corresponds to the result of this call is registered with the
    /// host environment as being the latest version of the compilation.
    /// </summary>
    /// <param name="position">The position in this.DocumentToPreprocess, of the first character to be replaced by this edit.</param>
    /// <param name="length">The number of characters in this.DocumentToPreprocess that will be replaced by this edit.</param>
    /// <param name="updatedText">The replacement string.</param>
    protected IVccSourceDocument GetUpdatedDocument(int position, int length, string updatedText, VersionType version)
      //^ requires 0 <= position && position < this.DocumentToPreprocess.Length;
      //^ requires 0 <= length && length <= this.DocumentToPreprocess.Length;
      //^ requires 0 <= position+length && position+length <= this.DocumentToPreprocess.Length;
      //^ ensures result.IsUpdatedVersionOf(this);
      //^ ensures result.GetType() == this.GetType();
    {
      VccCompositeDocument<PrimaryDocumentType, VersionType> result;
      List<CompilationPart> nextParts = new List<CompilationPart>(this.VccCompilationPart.Compilation.Parts);
      Compilation nextCompilation = this.VccCompilationPart.Compilation.UpdateCompilationParts(nextParts);
      VccCompilationHelper nextHelper = (VccCompilationHelper)this.Helper.MakeShallowCopyFor(nextCompilation);
      PreprocessorInformation ppInfo = this.PreprocessorInformation;
      foreach (ISourceLocation includedLocation in ppInfo.IncludedLocations) {
        if (includedLocation.StartIndex <= position && position+length <= includedLocation.StartIndex+includedLocation.Length) {
          //Included section spans the edit.
          PrimaryDocumentType nextVersionOfDocumentToPreprocess = this.GetNextVersionOfDocumentToPreprocess(position, length, updatedText, version);
          PreprocessorInformation nextVersionOfPreprocessorInformation = new PreprocessorInformation(nextVersionOfDocumentToPreprocess, ppInfo);
          result = this.GetNextVersion(nextHelper, nextVersionOfDocumentToPreprocess, nextVersionOfPreprocessorInformation, position, length, updatedText.Length);
          goto updateCompilationPart;
        }
      }
      foreach (ISourceLocation excludedLocation in ppInfo.excludedLocations) {
        if (excludedLocation.StartIndex <= position && position+length <= excludedLocation.StartIndex+excludedLocation.Length) {
          //Excluded section spans the edit.
          PrimaryDocumentType nextVersionOfDocumentToPreprocess = this.GetNextVersionOfDocumentToPreprocess(position, length, updatedText, version);
          PreprocessorInformation nextVersionOfPreprocessorInformation = new PreprocessorInformation(nextVersionOfDocumentToPreprocess, ppInfo);
          result = this.GetNextVersion(nextHelper, nextVersionOfDocumentToPreprocess, nextVersionOfPreprocessorInformation, 0, 0, 0);
          goto updateCompilationPart;
        }
      }
      {
        //Not a common case and could be an edit that results in a different list of included locations.
        //Re-preprocess and produce an edit that replaces the entire resulting document (and thus forces the entire CompilationUnit to be rebuilt).
        PrimaryDocumentType nextVersionOfDocumentToPreprocess = this.GetNextVersionOfDocumentToPreprocess(position, length, updatedText, version);
        result = this.GetNewVersion(nextHelper, nextVersionOfDocumentToPreprocess);
        result = this.GetNextVersion(nextHelper, nextVersionOfDocumentToPreprocess, result.PreprocessorInformation, 0, this.Length, result.Length);
        goto updateCompilationPart;
      }
    updateCompilationPart:
      EditEventArgs editEventArgs;
      EditEventArgs/*?*/ symbolTableEditEventArgs;
      ISourceLocation oldLocationBeforePreprocessing = this.DocumentToPreprocess.GetSourceLocation(position, length);
      ISourceLocation oldLocationAfterPreprocessing = this.GetLocationAfterPreprocessing(oldLocationBeforePreprocessing);
      VccSourceDocumentEdit edit = new VccSourceDocumentEdit(oldLocationAfterPreprocessing, result);
      edit.compilationPartAfterEdit = result.compilationPart = (VccCompilationPart)this.VccCompilationPart.UpdateWith(edit, nextParts, out editEventArgs, out symbolTableEditEventArgs);
      this.Helper.Compilation.HostEnvironment.RegisterAsLatest(result.VccCompilationPart.Compilation);
      this.Helper.Compilation.HostEnvironment.ReportEdits(editEventArgs);
      if (symbolTableEditEventArgs != null)
        this.Helper.Compilation.HostEnvironment.ReportSymbolTableEdits(symbolTableEditEventArgs);
      return result;
    }

    internal sealed class VccSourceDocumentEdit : AstSourceDocumentEdit {
      /// <summary>
      /// Allocates an object that describes an edit to a source file.
      /// </summary>
      internal VccSourceDocumentEdit(ISourceLocation sourceLocationBeforeEdit, ISourceDocument sourceDocumentAfterEdit)
        : base(sourceLocationBeforeEdit, sourceDocumentAfterEdit)
        //^ requires sourceDocumentAfterEdit.IsUpdatedVersionOf(sourceLocationBeforeEdit.SourceDocument);
      {
      }

      /// <summary>
      /// The compilation part that is the result of applying this edit.
      /// </summary>
      public override CompilationPart CompilationPartAfterEdit {
        get {
          //^ assume this.compilationPartAfterEdit != null;
          return this.compilationPartAfterEdit;
        }
      }
      internal CompilationPart/*?*/ compilationPartAfterEdit;
    }

    /// <summary>
    /// Returns a location in the preprocessed document that corresponds to the given location from the unpreprocessed document.
    /// </summary>
    /// <param name="sourceLocation">A locotion in a source document that forms part of this composite document.</param>
    public ISourceLocation GetLocationAfterPreprocessing(ISourceLocation sourceLocation)
      //^ requires sourceLocation.SourceDocument == this.DocumentToPreprocess;
    {
      int start = 0;
      int length = 0;
      PreprocessorInformation ppInfo = this.PreprocessorInformation;
      foreach (ISourceLocation includedLocation in ppInfo.IncludedLocations) {
        if (includedLocation.StartIndex+includedLocation.Length <= sourceLocation.StartIndex) {
          //The included location is before the source location. Just add its length to start.
          start += includedLocation.Length;
        } else if (includedLocation.StartIndex <= sourceLocation.StartIndex) {
          if (includedLocation.StartIndex+includedLocation.Length >= sourceLocation.StartIndex+sourceLocation.Length) {
            //The included location overlaps the entire source location
            start += sourceLocation.StartIndex-includedLocation.StartIndex;
            length = sourceLocation.Length;
            break;
          } else {
            //The included location overlaps a prefix of the source location
            start += sourceLocation.StartIndex-includedLocation.StartIndex;
            length += includedLocation.Length-(sourceLocation.StartIndex-includedLocation.StartIndex);
          }
        } else if (includedLocation.StartIndex >= sourceLocation.StartIndex+sourceLocation.Length) {
          //The included location occurs after the end of the source location.
          break;
        } else {
          if (includedLocation.StartIndex+includedLocation.Length < sourceLocation.StartIndex+sourceLocation.Length) {
            //The included location is contained within the source location.
            length += includedLocation.Length;
          } else {
            //The included location overlaps a suffix of the source location
            length += includedLocation.StartIndex-sourceLocation.StartIndex;
            break;
          }
        }
      }
      return this.GetSourceLocation(start, length);
    }

    /// <summary>
    /// Returns a new preprocessed document of the same type as this document, but using the given helper and underlying document to preprocess.
    /// </summary>
    /// <param name="helper">A Vcc specific helper object that is used to provide the value of the CompilationPart property.</param>
    /// <param name="documentToPreprocess">The unpreprocessed document on which the newly allocated document should be based.</param>
    protected abstract VccCompositeDocument<PrimaryDocumentType, VersionType> GetNewVersion(VccCompilationHelper helper, PrimaryDocumentType documentToPreprocess);
    // ^ ensures result.GetType() == this.GetType(); //TODO: this crashes the non null analyzer

    /// <summary>
    /// Returns a new version of this.DocumentToPreprocess where the substring designated by position and length has been replaced by the given replacement text.
    /// </summary>
    /// <param name="position">The index of the first character to replace.</param>
    /// <param name="length">The number of characters to replace.</param>
    /// <param name="updatedText">The replacement string.</param>
    /// <param name="version">A version object (may be null) to associate with the result.</param>
    protected abstract PrimaryDocumentType/*!*/ GetNextVersionOfDocumentToPreprocess(int position, int length, string updatedText, VersionType version);
    // ^ requires 0 <= position && position < this.DocumentToPreprocess.Length; //Spec# bug: this contract is not fully analyzed by the time it is inherited by the override
    // ^ requires 0 <= length && length <= this.DocumentToPreprocess.Length;
    // ^ requires 0 <= position+length && position+length <= this.DocumentToPreprocess.Length;

    /// <summary>
    /// Returns a new version of this document where the substring designated by position and length has been replaced by the given replacement text.
    /// </summary>
    /// <param name="helper">A Vcc specific helper object that is used to provide the value of the CompilationPart property.</param>
    /// <param name="nextVersionOfDocumentToPreprocess">The unpreprocessed document on which the newly allocated document should be based.</param>
    /// <param name="nextVersionOfPreprocessorInformation">A preprocessing information object that was incrementally derived from it previous version.</param>
    /// <param name="position">The first character in the previous version of the new document that will be changed in the new document.</param>
    /// <param name="oldLength">The number of characters in the previous verion of the new document that will be changed in the new document.</param>
    /// <param name="newLength">The number of replacement characters in the new document. 
    /// (The length of the string that replaces the substring from position to position+length in the previous version of the new document.)</param>
    protected abstract VccCompositeDocument<PrimaryDocumentType, VersionType> GetNextVersion(VccCompilationHelper helper,
      PrimaryDocumentType nextVersionOfDocumentToPreprocess, PreprocessorInformation/*?*/ nextVersionOfPreprocessorInformation, int position, int oldLength, int newLength);
    // ^ ensures result.GetType() == this.GetType(); //TODO: this crashes the non null analyzer

    protected override IEnumerable<ISourceLocation> GetFragments()  {
      if (this.preprocessorInformation != null)
        return this.preprocessorInformation.IncludedLocations; //should get here when constructing a PDB
      if (this.preprocessor == null) {
        //Fast case for constructing symbol table in VS
        Preprocessor preprocessor = this.GetAndCacheNewPreprocessor(); //avoid calling this.PreprocessorInformation as it will process the entire document before returning.
        return preprocessor.GetIncludedSections();
      }
      //Should only get here when getting source location information about an error.
      return this.PreprocessorInformation.IncludedLocations;
    }

    internal Preprocessor GetAndCacheNewPreprocessor()
      //^ ensures this.preprocessor == result;
    {
      if (this.preprocessorDefinedSymbols == null)
        return this.preprocessor = new Preprocessor(this.DocumentToPreprocess, (VccOptions)this.VccCompilationPart.Compilation.Options);
      else
        return this.preprocessor = new Preprocessor(this.DocumentToPreprocess, this.preprocessorDefinedSymbols, new List<IErrorMessage>());
    }

    protected VccCompilationHelper Helper {
      get { return this.helper; }
    }
    readonly VccCompilationHelper helper;

    private Preprocessor Preprocessor {
      get
        //^ ensures result.PreprocessingIsComplete;
      {
        if (this.preprocessor == null || !this.preprocessor.PreprocessingIsComplete) {
          lock (GlobalLock.LockingObject) {
            if (this.preprocessor == null || !this.preprocessor.PreprocessingIsComplete) {
              Preprocessor preprocessor = this.GetAndCacheNewPreprocessor();
              //^ assert this.preprocessor != null;
              for (IEnumerator<ISourceLocation> includedSectionEnumerator = preprocessor.GetIncludedSections().GetEnumerator(); includedSectionEnumerator.MoveNext(); ) ;
              //^ assume this.preprocessor.PreprocessingIsComplete;
            }
          }
        }
        return this.preprocessor; 
      }
    }
    private Preprocessor/*?*/ preprocessor;

    IDictionary<string, string>/*?*/ preprocessorDefinedSymbols;

    internal override List<IErrorMessage> PreprocessorErrors {
      get {
        if (this.preprocessor == null) {
          lock (GlobalLock.LockingObject) {
            if (this.preprocessor == null) {
              if (this.preprocessorInformation == null)
                return new List<IErrorMessage>(0);
              return this.preprocessorInformation.errors;
            }
          }
        }
        return this.preprocessor.errors;
      }
    }

    public PreprocessorInformation PreprocessorInformation {
      get {
        if (this.preprocessorInformation == null) {
          lock (GlobalLock.LockingObject) {
            if (this.preprocessorInformation == null)
              this.preprocessorInformation = this.Preprocessor.PreprocessorInformation;
          }
        }
        return this.preprocessorInformation;
      }
    }
    private PreprocessorInformation/*?*/ preprocessorInformation;

    internal override List<IErrorMessage> ScannerAndParserErrors {
      get { return this.scannerAndParserErrors; }
    }
    private readonly List<IErrorMessage> scannerAndParserErrors = new List<IErrorMessage>();

    public override string SourceLanguage {
      get { return "Vcc"; }
    }

    internal override ISourceDocument UnpreprocessedDocument {
      get { return this.documentToPreprocess; }
    }

  }

  public interface IVccUnpreprocessedSourceDocument : IPrimarySourceDocument {
    Preprocessor GetPreprocessorFor(string location, IDictionary<string, string> preprocessorSymbols);
    // ^ requires System.IO.File.Exists(location);
  }

  public sealed class VccSourceDocument : VccCompositeDocument<VccUnpreprocessedSourceDocument, object> {

    public VccSourceDocument(VccCompilationHelper helper, IName name, string location, System.IO.StreamReader streamReader)
      : base(helper, new VccUnpreprocessedSourceDocument(helper, name, location, streamReader), null) {
    }

    public VccSourceDocument(VccCompilationHelper helper, IName name, string location, string text)
      : base(helper, new VccUnpreprocessedSourceDocument(helper, name, location, text), null) {
    }

    public VccSourceDocument(VccCompilationHelper helper, VccUnpreprocessedSourceDocument documentToPreprocess, IDictionary<string, string> preprocessorDefinedSymbols)
      : base(helper, documentToPreprocess, preprocessorDefinedSymbols) {
    }

    private VccSourceDocument(VccCompilationHelper helper, VccUnpreprocessedSourceDocument documentToPreprocess)
      : base(helper, documentToPreprocess, null) {
    }

    private VccSourceDocument(VccCompilationHelper helper, VccUnpreprocessedSourceDocument nextVersionOfDocumentToPreprocess, PreprocessorInformation/*?*/ nextVersionOfPreprocessorInformation,
      VccSourceDocument previousVersion, int position, int oldLength, int newLength)
      : base(helper, nextVersionOfDocumentToPreprocess, nextVersionOfPreprocessorInformation, previousVersion, position, oldLength, newLength) {
    }

    /// <summary>
    /// Returns a source document edit that represents the replacement of the text of this.DocumentToPreprocess from position to position+length with updatedText.
    /// I.e. the given position and length corresponds to the text of the document before preprocessing, but the resulting
    /// edit is an edit to the document after preprocessing (i.e. this document, not the one that preprocessor consumes).
    /// </summary>
    /// <param name="position">The position in this.DocumentToPreprocess, of the first character to be replaced by this edit.</param>
    /// <param name="length">The number of characters in this.DocumentToPreprocess that will be replaced by this edit.</param>
    /// <param name="updatedText">The replacement string.</param>
    public IVccSourceDocument GetUpdatedDocument(int position, int length, string updatedText)
      //^ requires 0 <= position && position < this.DocumentToPreprocess.Length;
      //^ requires 0 <= length && length <= this.DocumentToPreprocess.Length;
      //^ requires 0 <= position+length && position+length <= this.DocumentToPreprocess.Length;
      //^ ensures result.IsUpdatedVersionOf(this);
    {
      return base.GetUpdatedDocument(position, length, updatedText, this);
    }

    /// <summary>
    /// Returns a new preprocessed document of the same type as this document, but using the given helper and underlying document to preprocess.
    /// </summary>
    /// <param name="helper">A Vcc specific helper object that is used to provide the value of the CompilationPart property.</param>
    /// <param name="documentToPreprocess">The unpreprocessed document on which the newly allocated document should be based.</param>
    protected override VccCompositeDocument<VccUnpreprocessedSourceDocument, object> GetNewVersion(VccCompilationHelper helper, VccUnpreprocessedSourceDocument documentToPreprocess) {
      return new VccSourceDocument(helper, documentToPreprocess);
    }

    protected override VccUnpreprocessedSourceDocument GetNextVersionOfDocumentToPreprocess(int position, int length, string updatedText, object version) 
      //^^ requires 0 <= position && position < this.DocumentToPreprocess.Length;
      //^^ requires 0 <= length && length <= this.DocumentToPreprocess.Length;
      //^^ requires 0 <= position+length && position+length <= this.DocumentToPreprocess.Length;
    {
      VccUnpreprocessedSourceDocument documentToPreprocess = this.DocumentToPreprocess;
      //^ assume 0 <= position && position < documentToPreprocess.Length; //follows from precondition
      //^ assume 0 <= length && length <= this.DocumentToPreprocess.Length; //follows from precondition
      //^ assume 0 <= position+length && position+length <= this.DocumentToPreprocess.Length;
      return documentToPreprocess.GetUpdatedDocument(position, length, updatedText);
    }

    protected override VccCompositeDocument<VccUnpreprocessedSourceDocument, object> GetNextVersion(VccCompilationHelper helper,
      VccUnpreprocessedSourceDocument nextVersionOfDocumentToPreprocess, PreprocessorInformation/*?*/ nextVersionOfPreprocessorInformation, int position, int oldLength, int newLength)
      //^^ ensures result.GetType() == this.GetType();
    {
      return new VccSourceDocument(helper, nextVersionOfDocumentToPreprocess, nextVersionOfPreprocessorInformation, this, position, oldLength, newLength); 
    }

    /// <summary>
    /// Makes a shallow copy of this source document (creating a new
    /// </summary>
    public override VccCompositeDocument MakeShallowCopyFor(Compilation targetCompilation) {
      VccCompilationHelper helperCopy = (VccCompilationHelper)this.Helper.MakeShallowCopyFor(targetCompilation);
      PreprocessorInformation/*?*/ nextVersionOfPreprocessorInformation = new PreprocessorInformation(this.DocumentToPreprocess, this.PreprocessorInformation);
      return new VccSourceDocument(helperCopy, this.DocumentToPreprocess, nextVersionOfPreprocessorInformation, this, 0, 0, 0);
    }

    internal override ISourceDocument UnpreprocessedDocument {
      get { return this.DocumentToPreprocess; }
    }
  }

  public sealed class DummyVccCompilation : Compilation {

    public DummyVccCompilation(ICompilation compilation, IMetadataHost compilationHost)
      : base(new DummyEditHostEnvironment(compilationHost), new DummyUnit(compilation, compilationHost), new VccOptions()) {
    }

    protected override List<CompilationPart> GetPartList() {
      return new List<CompilationPart>(0);
    }

    public override Compilation UpdateCompilationParts(IEnumerable<CompilationPart> parts) {
      return this;
    }
  }

  internal sealed class DummyEditHostEnvironment : SourceEditHostEnvironment {

    internal DummyEditHostEnvironment(IMetadataHost compilationHost)
      : base(compilationHost.NameTable, 4) {
      this.compilationHost = compilationHost;
    }

    readonly IMetadataHost compilationHost;

    public override IUnit LoadUnitFrom(string location) {
      return this.compilationHost.LoadUnitFrom(location);
    }
  }

  internal sealed class VccErrorMessage : ErrorMessage {

    private VccErrorMessage(ISourceLocation sourceLocation, long code, string messageKey, params string[] messageArguments)
      : base(sourceLocation, code, messageKey, messageArguments) {
    }

    public VccErrorMessage(ISourceLocation sourceLocation, Error error, params string[] messageArguments)
      : this(sourceLocation, (long)error, error.ToString(), messageArguments) {
    }

    public override object ErrorReporter {
      get { return Microsoft.Research.Vcc.ErrorReporter.Instance; }
    }

    public override string ErrorReporterIdentifier {
      get { return "Vcc"; }
    }

    public override bool IsWarning {
      get {
        return this.Severity != 0; //TODO: check options
      }
    }

    public override ISourceErrorMessage MakeShallowCopy(ISourceDocument targetDocument)
      //^^ requires targetDocument == this.SourceLocation.SourceDocument || targetDocument.IsUpdatedVersionOf(this.SourceLocation.SourceDocument);
      //^^ ensures targetDocument == this.SourceLocation.SourceDocument ==> result == this;
    {
      if (this.SourceLocation.SourceDocument == targetDocument) return this;
      ISourceLocation sloc = this.SourceLocation;
      //^ assume targetDocument.IsUpdatedVersionOf(sloc.SourceDocument); //follows from precondition
      return new VccErrorMessage(targetDocument.GetCorrespondingSourceLocation(sloc), this.Code, this.MessageKey, this.MessageArguments());
    }

    public override string Message {
      get {
        ResourceManager resourceManager = new ResourceManager("Microsoft.Research.Vcc.ErrorMessages", this.GetType().Assembly);
        return base.GetMessage(resourceManager);
      }
    }

    public int Severity {
      get {
        switch ((Error)this.Code) {
          case Error.PossibleMistakenNullStatement:
          case Error.VccAttributeOnTypeDef:
            return 1;
          default:
            return 0;
        }
      }
    }
  }
}
