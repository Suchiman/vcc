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
    readonly bool isSpec;
    readonly ITypeReference targetType;

    public VccPointerType(ITypeReference targetType, bool isSpec, IInternFactory internFactory) 
      : base(internFactory)
    {
      this.targetType = targetType;
      this.isSpec = isSpec;
    }

    public ITypeReference TargetType {
      get { return this.targetType; }
    }

    public bool IsSpec {
      get { return this.isSpec; }
    }

    public override void Dispatch(IMetadataVisitor visitor) {
      visitor.Visit(this);
    }

    public override IPlatformType PlatformType {
      get { return this.TargetType.ResolvedType.PlatformType; }
    }

    public override string ToString() {
      return this.TargetType.ResolvedType.ToString() + (this.isSpec ? "^" : "*");
    }

    public override PrimitiveTypeCode TypeCode {
      get { return PrimitiveTypeCode.Pointer; }
    }
  }

  internal sealed class VccModifiedPointerType : VccPointerType, IVccPointerType
  {
    public VccModifiedPointerType(ITypeReference targetType, IEnumerable<ICustomModifier> customModifiers, bool isSpec, IInternFactory internFactory)
      : base(targetType, isSpec, internFactory) {
      this.customModifiers = customModifiers;
    }

    public override bool IsModified {
      get { return true; }
    }

    public override IEnumerable<ICustomModifier> CustomModifiers {
      get { return this.customModifiers; }
    }
    readonly IEnumerable<ICustomModifier> customModifiers;
  }
}