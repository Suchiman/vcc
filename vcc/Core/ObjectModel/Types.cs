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

namespace Microsoft.Research.Vcc
{
  public interface IVccPointerType : IPointerType
  {
    bool IsSpec { get; }
  }

  internal class VccPointerType : SystemDefinedStructuralType, IVccPointerType
  {
    readonly ITypeReference targetType;

    internal VccPointerType(ITypeReference targetType, IInternFactory internFactory)
      : base(internFactory) {
      this.targetType = targetType;
    }

    public ITypeReference TargetType {
      get { return this.targetType; }
    }

    public virtual bool IsSpec {
      get { return false; }
    }

    public override void Dispatch(IMetadataVisitor visitor) {
      visitor.Visit(this);
    }

    public override IPlatformType PlatformType {
      get { return this.TargetType.ResolvedType.PlatformType; }
    }

    public override string ToString() {
      return this.TargetType.ResolvedType.ToString() + (this.IsSpec ? "^" : "*");
    }

    public override PrimitiveTypeCode TypeCode {
      get { return PrimitiveTypeCode.Pointer; }
    }

    public static VccPointerType GetPointerType(ITypeReference targetType, bool isSpec, IInternFactory internFactory) {
      if (isSpec) return new VccSpecPointerType(targetType, internFactory);
      else return new VccPointerType(targetType, internFactory);
    }
  }

  internal class VccSpecPointerType : VccPointerType, IModifiedTypeReference
  {
    internal VccSpecPointerType(ITypeReference targetType, IInternFactory internFactory)
      : base(targetType, internFactory) {
    }

    public override void Dispatch(IMetadataVisitor visitor) {
      visitor.Visit((IPointerType)this);
    }

    public override bool IsSpec {
      get { return true; }
    }

    public ITypeReference UnmodifiedType {
      get { return new VccPointerType(this.TargetType, this.InternFactory); }
    }

    public override IEnumerable<ICustomModifier> CustomModifiers {
      get { return IteratorHelper.GetSingletonEnumerable<ICustomModifier>(new CustomModifier(true, this.PlatformType.SystemDiagnosticsContractsContract)); }
    }
  }

  internal class VccModifiedPointerType : VccPointerType, IModifiedTypeReference
  {
    public VccModifiedPointerType(ITypeReference targetType, IEnumerable<ICustomModifier> customModifiers, IInternFactory internFactory)
      : base(targetType, internFactory) {
      this.customModifiers = customModifiers;
    }

    public override void Dispatch(IMetadataVisitor visitor) {
      visitor.Visit((IModifiedTypeReference)this);
    }

    public override bool IsModified {
      get { return true; }
    }

    public override IEnumerable<ICustomModifier> CustomModifiers {
      get { return this.customModifiers; }
    }
    readonly IEnumerable<ICustomModifier> customModifiers;

    public ITypeReference UnmodifiedType {
      get { return new VccPointerType(this.TargetType, this.InternFactory); }
    }

    public static VccModifiedPointerType GetPointerType(ITypeReference targetType, IEnumerable<ICustomModifier> customModifiers, bool isSpec, IInternFactory internFactory) {
      if (isSpec) return new VccModifiedPointerType(targetType, customModifiers, internFactory);
      else return new VccModifiedSpecPointerType(targetType, customModifiers, internFactory);
    }
  }

  internal class VccModifiedSpecPointerType : VccModifiedPointerType
  {
    internal VccModifiedSpecPointerType(ITypeReference targetType, IEnumerable<ICustomModifier> customModifiers, IInternFactory internFactory)
      : base(targetType, customModifiers, internFactory) {
    }

    public override bool IsSpec {
      get { return true; }
    }

    public override IEnumerable<ICustomModifier> CustomModifiers {
      get {
        return IteratorHelper.Concat<ICustomModifier>(
          IteratorHelper.GetSingletonEnumerable<ICustomModifier>(new CustomModifier(true, this.PlatformType.SystemDiagnosticsContractsContract)),
          base.CustomModifiers);
      }
    }
  }
}
