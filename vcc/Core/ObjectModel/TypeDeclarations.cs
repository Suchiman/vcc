//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using Microsoft.Cci.Ast;
using Microsoft.Cci;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc {
  internal sealed class VccGlobalDeclarationContainerClass : GlobalDeclarationContainerClass {

    public VccGlobalDeclarationContainerClass(IMetadataHost compilationHost)
      : base(compilationHost) {
    }

    private VccGlobalDeclarationContainerClass(NamespaceDeclaration containingNamespaceDeclaration, VccGlobalDeclarationContainerClass template)
      : base(containingNamespaceDeclaration, template)
      //^ ensures this.containingNamespaceDeclaration == containingNamespaceDeclaration;
    {
    }

    public override INamespaceDeclarationMember MakeShallowCopyFor(NamespaceDeclaration targetNamespaceDeclaration)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingNamespaceDeclaration == targetNamespaceDeclaration;
    {
      if (targetNamespaceDeclaration == this.ContainingNamespaceDeclaration) return this;
      return new VccGlobalDeclarationContainerClass(targetNamespaceDeclaration, this);
    }


    public override void SetCompilationPart(CompilationPart compilationPart, bool recurse) {
      base.SetCompilationPart(compilationPart, recurse);
      VccTypeContract/*?*/ typeContract = this.Compilation.ContractProvider.GetTypeContractFor(this) as VccTypeContract;
      if (typeContract != null) typeContract.SetContainingType(this);
    }

    public override void SetMemberContainingTypeDeclaration(ITypeDeclarationMember member) {
      TypedefDeclaration/*?*/ typedef = member as TypedefDeclaration;
      if (typedef != null) {
        typedef.SetContainingTypeDeclaration(this); 
        return;
      }
      FunctionDeclaration/*?*/ functionDeclaration = member as FunctionDeclaration;
      if (functionDeclaration != null) {
        functionDeclaration.SetContainingTypeDeclaration(this); 
        return;
      }
      base.SetMemberContainingTypeDeclaration(member);
    }

  }

  internal class VccArray : NestedStructDeclaration {
    internal VccArray(NameDeclaration name, List<ITypeDeclarationMember> members, uint arraySizeInBytes)
      : base(null, Flags.Sealed|(Flags)TypeMemberVisibility.Public, name, new List<GenericTypeParameterDeclaration>(0), new List<TypeExpression>(0), members, SourceDummy.SourceLocation)
      //^ requires arraySizeInBytes > 0;
    {
      this.arraySizeInBytes = arraySizeInBytes;
    }

    protected VccArray(TypeDeclaration containingTypeDeclaration, VccArray template)
      : base(containingTypeDeclaration, template)
    {
    }

    protected VccArray(TypeDeclaration containingTypeDeclaration, VccArray template, List<ITypeDeclarationMember> members)
      : base(containingTypeDeclaration, template, members)
    {
    }

    //^ [MustOverride]
    protected override NestedTypeDeclaration MakeShallowCopy(List<ITypeDeclarationMember> members)
      //^^ ensures result.GetType() == this.GetType();
    {
      return new VccArray(this.ContainingTypeDeclaration, this, members);
    }

    //^ [MustOverride]
    public override ITypeDeclarationMember MakeShallowCopyFor(TypeDeclaration targetTypeDeclaration) {
      if (targetTypeDeclaration == this.ContainingTypeDeclaration) return this;
      return new VccArray(targetTypeDeclaration, this);
    }

    /// <summary>
    /// Size of an object of this type. In bytes. If zero, the size is unspecified and will be determined at runtime.
    /// </summary>
    public override uint SizeOf {
      get
        //^^ ensures result >= 0;
      {
        return this.arraySizeInBytes;
      }
    }
    readonly uint arraySizeInBytes;
    //^ invariant arraySizeInBytes > 0;

  }

  internal class VccEnumDeclaration : NamespaceEnumDeclaration {
    internal VccEnumDeclaration(NameDeclaration name, TypeExpression underlyingType, List<ITypeDeclarationMember> members, ISourceLocation sourceLocation)
      : base(null, Flags.None, name, underlyingType, members, sourceLocation) 
    {
    }

    protected VccEnumDeclaration(NamespaceDeclaration containingNamespaceDeclaration, VccEnumDeclaration template)
      : base(containingNamespaceDeclaration, template) {
    }

    //^ [MustOverride]
    public override INamespaceDeclarationMember MakeShallowCopyFor(NamespaceDeclaration targetNamespaceDeclaration)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingNamespaceDeclaration == targetNamespaceDeclaration;
    {
      if (targetNamespaceDeclaration == this.ContainingNamespaceDeclaration) return this;
      return new VccEnumDeclaration(targetNamespaceDeclaration, this);
    }
   }

  internal abstract class VccStructuredTypeDeclaration : NamespaceStructDeclaration, IDeclaration
  {
    protected VccStructuredTypeDeclaration(NameDeclaration name, List<ITypeDeclarationMember> members, IEnumerable<DeclspecSpecifier> extendedAttributes, ISourceLocation sourceLocation)
      : base(null, Flags.None, name, new List<GenericTypeParameterDeclaration>(0), new List<TypeExpression>(0), members, sourceLocation) {
      this.extendedAttributes = extendedAttributes;
    }

    protected VccStructuredTypeDeclaration(NamespaceDeclaration containingNamespaceDeclaration, VccStructuredTypeDeclaration template)
      : base(containingNamespaceDeclaration, template) {
      this.extendedAttributes = new List<DeclspecSpecifier>(template.extendedAttributes);
      this.isSpec = template.isSpec;
    }

    protected override List<ICustomAttribute> GetAttributes() {
      var result = base.GetAttributes();
      IEnumerable<SourceCustomAttribute> attributesFromDeclSpec = FunctionDefinition.ConvertSpecifiersIntoAttributes(
        IteratorHelper.GetConversionEnumerable<DeclspecSpecifier, Specifier>(this.extendedAttributes),
        new DummyExpression(this.DummyBlock, SourceDummy.SourceLocation));
      foreach (SourceCustomAttribute extAttr in attributesFromDeclSpec)
        result.Add(new CustomAttribute(extAttr));
      return result;
    }

    /// <summary>
    /// Layout of the type declaration.
    /// </summary>
    public override LayoutKind Layout {
      get { return LayoutKind.Explicit; }
    }

    public override void SetCompilationPart(CompilationPart compilationPart, bool recurse) {
      base.SetCompilationPart(compilationPart, recurse);
      VccTypeContract/*?*/ typeContract = this.Compilation.ContractProvider.GetTypeContractFor(this) as VccTypeContract;
      if (typeContract != null) typeContract.SetContainingType(this);
    }

    readonly IEnumerable<DeclspecSpecifier> extendedAttributes;

    private readonly bool isSpec;

    public override TypeMemberVisibility GetDefaultVisibility() {
      return TypeMemberVisibility.Public;
    }

    private bool cycleInAlignment;

    public override ushort Alignment {
      get {
        if (cycleInAlignment) return 1;
        else {
          cycleInAlignment = true;
          var result = TypeHelper.TypeAlignment(this.TypeDefinition, false);
          cycleInAlignment = false;
          return result;
        }
      }
    }
  }

  internal abstract class VccNestedStructuredTypeDeclaration : NestedStructDeclaration {
    protected VccNestedStructuredTypeDeclaration(NameDeclaration name, List<ITypeDeclarationMember> members, IEnumerable<DeclspecSpecifier> extendedAttributes, ISourceLocation sourceLocation)
      : base(null, Flags.None|(Flags)TypeMemberVisibility.Public, name, new List<GenericTypeParameterDeclaration>(0), new List<TypeExpression>(0), members, sourceLocation)
    {
      this.extendedAttributes = extendedAttributes;
    }

    protected VccNestedStructuredTypeDeclaration(TypeDeclaration containingTypeDeclaration, VccNestedStructuredTypeDeclaration template)
      : base(containingTypeDeclaration, template)
    {
      this.extendedAttributes = new List<DeclspecSpecifier>(template.extendedAttributes);
    }

    protected VccNestedStructuredTypeDeclaration(TypeDeclaration containingTypeDeclaration, VccNestedStructuredTypeDeclaration template, List<ITypeDeclarationMember> members)
      : base(containingTypeDeclaration, template, members)
    {
      this.extendedAttributes = new List<DeclspecSpecifier>(template.ExtendedAttributes);
    }

    public IEnumerable<DeclspecSpecifier> ExtendedAttributes
    {
      get
      {
        return this.extendedAttributes;
      }
    }

    readonly IEnumerable<DeclspecSpecifier> extendedAttributes;

    protected override List<ICustomAttribute> GetAttributes() {
      var result = base.GetAttributes();
      IEnumerable<SourceCustomAttribute> attributesFromDeclSpec = FunctionDefinition.ConvertSpecifiersIntoAttributes(
        IteratorHelper.GetConversionEnumerable<DeclspecSpecifier, Specifier>(this.extendedAttributes),
        new DummyExpression(this.DummyBlock, SourceDummy.SourceLocation));
      foreach (SourceCustomAttribute extAttr in attributesFromDeclSpec)
        result.Add(new CustomAttribute(extAttr));
      return result;
    }

    /// <summary>
    /// Layout of the type declaration.
    /// </summary>
    public override LayoutKind Layout {
      get { return LayoutKind.Explicit; }
    }

    // to prevent warning about unverifiable code
    private IEnumerable<ICustomAttribute> BaseAttributes
    {
      get { return base.Attributes; }
    }

    public override TypeMemberVisibility GetDefaultVisibility()
    {
      return TypeMemberVisibility.Public;
    }

    public override ushort Alignment {
      get { return TypeHelper.TypeAlignment(this.NestedTypeDefinition, false); }
    }
  }

  internal class VccStructDeclaration : VccStructuredTypeDeclaration {

    internal VccStructDeclaration(NameDeclaration name, List<ITypeDeclarationMember> members, IEnumerable<DeclspecSpecifier> extendedAttributes, ISourceLocation sourceLocation)
      : base(name, members, extendedAttributes, sourceLocation) {
    }

    protected VccStructDeclaration(NamespaceDeclaration containingNamespaceDeclaration, VccStructDeclaration template)
      : base(containingNamespaceDeclaration, template) {
    }

    /// <summary>
    /// Use compute field offset to get the offset. 
    /// </summary>
    /// <param name="item"> Field whose offset is of concern. </param>
    /// <returns></returns>
    public override uint GetFieldOffset(object item) {
      FieldDeclaration field = item as FieldDeclaration;
      if (field != null)
        return MemberHelper.ComputeFieldOffset(field.FieldDefinition, field.FieldDefinition.ContainingTypeDefinition);
      else
        return MemberHelper.ComputeFieldOffset((INestedTypeDefinition)item, this.TypeDefinition);
    }

    private bool cycleInSizeOf;

    public override uint SizeOf {
      get {
        if (cycleInSizeOf) return 1;
        else {
          cycleInSizeOf = true;
          var result = TypeHelper.SizeOfType(this.TypeDefinition, false);
          cycleInSizeOf = false;
          return result;
        }
      }
    }


    //^ [MustOverride]
    public override INamespaceDeclarationMember MakeShallowCopyFor(NamespaceDeclaration targetNamespaceDeclaration)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingNamespaceDeclaration == targetNamespaceDeclaration;
    {
      if (targetNamespaceDeclaration == this.ContainingNamespaceDeclaration) return this;
      return new VccStructDeclaration(targetNamespaceDeclaration, this);
    }
  }

  internal class VccNestedStructDeclaration : VccNestedStructuredTypeDeclaration {

    internal VccNestedStructDeclaration(NameDeclaration name, List<ITypeDeclarationMember> members, IEnumerable<DeclspecSpecifier> extendedAttributes, ISourceLocation sourceLocation)
      : base(name, members, extendedAttributes, sourceLocation)
    {
    }

    protected VccNestedStructDeclaration(TypeDeclaration containingTypeDeclaration, VccNestedStructDeclaration template)
      : base(containingTypeDeclaration, template)
    {
    }

    protected VccNestedStructDeclaration(TypeDeclaration containingTypeDeclaration, VccNestedStructDeclaration template, List<ITypeDeclarationMember> members)
      : base(containingTypeDeclaration, template, members)
    {
    }

    private bool cycleInSizeOf;

    public override uint SizeOf {
      get {
        if (cycleInSizeOf) return 1;
        else {
          cycleInSizeOf = true;
          var result = TypeHelper.SizeOfType(this.NestedTypeDefinition, false);
          cycleInSizeOf = false;
          return result;
        }
      }
    }

    public override uint GetFieldOffset(object item)
    {
      uint result;
      FieldDeclaration field = item as FieldDeclaration;
      INestedTypeDefinition nestedTypeDef = item as INestedTypeDefinition;
      if (field != null)
        result = MemberHelper.ComputeFieldOffset(field.FieldDefinition, field.FieldDefinition.ContainingTypeDefinition);
      else
        result = MemberHelper.ComputeFieldOffset(nestedTypeDef, this.TypeDefinition);

      // This distinguishes between anonymous nested types, where we need to walk upwards to the surrounding type,
      // and nested types that have their own field name, where we must not do this
      // the current AST representation requires such a complicated approach
      bool nestedTypeHasFieldName = false;
      ITypeDefinition containingDef = this.ContainingTypeDeclaration.TypeDefinition;
      foreach (ITypeDefinitionMember member in containingDef.Members) {
        IFieldDefinition fieldMember = member as IFieldDefinition;
        if (fieldMember != null && fieldMember.Type.ResolvedType == this.TypeDefinition && !String.IsNullOrEmpty(fieldMember.Name.Value)) {
          nestedTypeHasFieldName = true;
          break;
        }
      }
      if (!nestedTypeHasFieldName)
        result += this.ContainingTypeDeclaration.GetFieldOffset(this.TypeDefinition);
      return result;
    }

    //^ [MustOverride]
    protected override NestedTypeDeclaration MakeShallowCopy(List<ITypeDeclarationMember> members)
    //^^ ensures result.GetType() == this.GetType();
    {
      return new VccNestedStructDeclaration(this.ContainingTypeDeclaration, this, members);
    }

    //^ [MustOverride]
    public override ITypeDeclarationMember MakeShallowCopyFor(TypeDeclaration targetTypeDeclaration)
    {
      if (targetTypeDeclaration == this.ContainingTypeDeclaration) return this;
      return new VccNestedStructDeclaration(targetTypeDeclaration, this);
    }

    public override void SetCompilationPart(CompilationPart compilationPart, bool recurse)
    {
      base.SetCompilationPart(compilationPart, recurse);
      VccTypeContract/*?*/ typeContract = this.Compilation.ContractProvider.GetTypeContractFor(this) as VccTypeContract;
      if (typeContract != null) typeContract.SetContainingType(this);
    }
  }

  internal class VccUnionDeclaration : VccStructuredTypeDeclaration {
    internal VccUnionDeclaration(NameDeclaration name, List<ITypeDeclarationMember> members, IEnumerable<DeclspecSpecifier> extendedAttributes, ISourceLocation sourceLocation)
      : base(name, members, extendedAttributes, sourceLocation) 
    {
    }

    protected VccUnionDeclaration(NamespaceDeclaration containingNamespaceDeclaration, VccUnionDeclaration template)
      : base(containingNamespaceDeclaration, template) {
    }


    //^ [MustOverride]
    public override INamespaceDeclarationMember MakeShallowCopyFor(NamespaceDeclaration targetNamespaceDeclaration)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingNamespaceDeclaration == targetNamespaceDeclaration;
    {
      if (targetNamespaceDeclaration == this.ContainingNamespaceDeclaration) return this;
      return new VccUnionDeclaration(targetNamespaceDeclaration, this);
    }

    public override uint SizeOf {
      get { return ComputeSizeOf(this.TypeDeclarationMembers); }
    }

    internal static uint ComputeSizeOf(IEnumerable<ITypeDeclarationMember> members)
    {
      uint size = 0;
      foreach (ITypeDeclarationMember member in members) {
        AnonymousFieldDefinition anonFieldDef = member as AnonymousFieldDefinition;
        if (anonFieldDef != null) {
          uint memberSize = TypeHelper.SizeOfType(anonFieldDef.Type.ResolvedType);
          size = Math.Max(size, memberSize);
          continue;
        }
        FieldDefinition/*?*/ fieldDef = member as FieldDefinition;
        if (fieldDef != null && !fieldDef.IsStatic && !fieldDef.IsCompileTimeConstant) {
          uint memberSize = 0;
          VccArrayTypeExpression/*?*/ arrayType = fieldDef.Type as VccArrayTypeExpression;
          if (arrayType != null && arrayType.Size != null) {
            uint numElements = (uint)arrayType.SizeAsInt32;
            memberSize = numElements * TypeHelper.SizeOfType(arrayType.ElementType.ResolvedType);
          } else {
            memberSize = TypeHelper.SizeOfType(fieldDef.Type.ResolvedType);
          }
          size = Math.Max(size, memberSize);
          continue;
        }
        BitFieldDefinition bfieldDef = member as BitFieldDefinition;
        if (bfieldDef != null && !bfieldDef.IsStatic && !bfieldDef.IsCompileTimeConstant) {
          uint memberSize = TypeHelper.SizeOfType(bfieldDef.Type.ResolvedType);
          size = Math.Max(size, memberSize);
          continue;
        }
      }
      return size;
    }
  }

  internal class VccNestedUnionDeclaration : VccNestedStructuredTypeDeclaration {

    internal VccNestedUnionDeclaration(NameDeclaration name, List<ITypeDeclarationMember> members, IEnumerable<DeclspecSpecifier> extendedAttributes, ISourceLocation sourceLocation)
      : base(name, members, extendedAttributes, sourceLocation)
    {
    }

    protected VccNestedUnionDeclaration(TypeDeclaration containingTypeDeclaration, VccNestedUnionDeclaration template)
      : base(containingTypeDeclaration, template)
    {
    }

    protected VccNestedUnionDeclaration(TypeDeclaration containingTypeDeclaration, VccNestedUnionDeclaration template, List<ITypeDeclarationMember> members)
      : base(containingTypeDeclaration, template, members)
    {
    }

    public override uint SizeOf {
      get { return VccUnionDeclaration.ComputeSizeOf(this.TypeDeclarationMembers); }
    }

    public override uint GetFieldOffset(object item)
    {
      return this.ContainingTypeDeclaration.GetFieldOffset(this.TypeDefinition);
    }

    //^ [MustOverride]
    protected override NestedTypeDeclaration MakeShallowCopy(List<ITypeDeclarationMember> members)
    //^^ ensures result.GetType() == this.GetType();
    {
      return new VccNestedUnionDeclaration(this.ContainingTypeDeclaration, this, members);
    }

    //^ [MustOverride]
    public override ITypeDeclarationMember MakeShallowCopyFor(TypeDeclaration targetTypeDeclaration)
    {
      if (targetTypeDeclaration == this.ContainingTypeDeclaration) return this;
      return new VccNestedUnionDeclaration(targetTypeDeclaration, this);
    }

    public override void SetCompilationPart(CompilationPart compilationPart, bool recurse)
    {
      base.SetCompilationPart(compilationPart, recurse);
      VccTypeContract/*?*/ typeContract = this.Compilation.ContractProvider.GetTypeContractFor(this) as VccTypeContract;
      if (typeContract != null) typeContract.SetContainingType(this);
    }
  }
}