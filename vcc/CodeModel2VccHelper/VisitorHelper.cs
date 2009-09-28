//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using BoundExpression = Microsoft.Cci.MutableCodeModel.BoundExpression;
using Microsoft.Cci;
using Microsoft.Boogie;

namespace Microsoft.Research.Vcc
{
  public static class VisitorHelper
  {

    public static string IntegerTypeSuffix(ITypeReference type)
    {
      if (type.ResolvedType.IsEnum) type = type.ResolvedType.UnderlyingType;

      switch (type.TypeCode) {
        case PrimitiveTypeCode.Char:
        case PrimitiveTypeCode.UInt8:
        case PrimitiveTypeCode.UInt16:
        case PrimitiveTypeCode.UInt32:
          return ".u4";
        case PrimitiveTypeCode.UInt64:
          return ".u8";
        case PrimitiveTypeCode.Int8:
        case PrimitiveTypeCode.Int16:
        case PrimitiveTypeCode.Int32:
          return ".i4";
        case PrimitiveTypeCode.Int64:
          return ".i8";
        default:
          throw new InvalidOperationException("integer type required");
      }
    }

    public static IExpression GetReturnValue(ISourceMethodBody methodBody)
    {
      foreach (IStatement statement in methodBody.Block.Statements) {
        IReturnStatement/*?*/ retStatement = statement as IReturnStatement;
        if (retStatement != null) {
          IExpression retval = retStatement.Expression;
          if (retval != null) return retval;
        }
      }
      return CodeDummy.Expression;
    }

    public static Token GetTokenFor(IEnumerable<ILocation> locations)
    {
      return VisitorHelper.GetTokenFor(locations, false);
    }

    //private class GetTokenForClosure
    //{
    //  private readonly IEnumerable<ILocation> locations;
    //  private readonly bool useEndOfLocation;

    //  public GetTokenForClosure(IEnumerable<ILocation> locations, bool useEndOfLocation) {
    //    this.locations = locations;
    //    this.useEndOfLocation = useEndOfLocation;
    //  }

    //  public Token GetToken() {
    //    Token result = Token.NoToken;
    //    foreach (ILocation loc in this.locations) {
    //      IPrimarySourceLocation/*?*/ sloc = loc as IPrimarySourceLocation;
    //      if (sloc == null) {
    //        IDerivedSourceLocation/*?*/ dloc = loc as IDerivedSourceLocation;
    //        if (dloc != null) {
    //          foreach (IPrimarySourceLocation ploc in dloc.PrimarySourceLocations) {
    //            sloc = ploc;
    //            break;
    //          }
    //        } else
    //          continue;
    //      }
    //      if (sloc == null) continue;
    //      if (this.useEndOfLocation)
    //        sloc = sloc.PrimarySourceDocument.GetPrimarySourceLocation(sloc.EndIndex, 1);
    //      result = new SourceLocationWrapper(sloc);
    //      break;
    //    }
    //    return result;
    //  }
    //}

    public static Token GetTokenFor(IEnumerable<ILocation> locations, bool useEndOfLocation)
    {
      if (IteratorHelper.EnumerableIsEmpty(locations)) return Token.NoToken;
      return new LazyToken(
        delegate() {
          Token result = Token.NoToken;
          foreach (ILocation loc in locations) {
            IPrimarySourceLocation/*?*/ sloc = loc as IPrimarySourceLocation;
            if (sloc == null) {
              IDerivedSourceLocation/*?*/ dloc = loc as IDerivedSourceLocation;
              if (dloc != null) {
                foreach (IPrimarySourceLocation ploc in dloc.PrimarySourceLocations) {
                  sloc = ploc;
                  break;
                }
              } else
                continue;
            }
            if (sloc == null) continue;
            if (useEndOfLocation)
              sloc = sloc.PrimarySourceDocument.GetPrimarySourceLocation(sloc.EndIndex, 1);
            result = new SourceLocationWrapper(sloc);
            break;
          }
          return result;
        });
    }

    public static int RoundToBvSize(ITypeDefinition type)
    {
      return TypeHelper.SizeOfType(type) * 8 <= 32 ? 32 : 64;
    }

    public static bool TryInterpretAsFixedSizeArrayAccess(
      IAddressDereference addressDereference,
      out ITypeDefinition arrayType,
      out ITypeDefinition elementType,
      out IBoundExpression boundExpr,
      out IExpression offsetAndScale)
    {
      arrayType = null;
      elementType = null;
      boundExpr = null;
      offsetAndScale = null;
      IAddition ptrAddition = addressDereference.Address as IAddition;
      if (ptrAddition == null) return false;
      IConversion convToElementPtrType = ptrAddition.LeftOperand as IConversion;
      if (convToElementPtrType == null) return false;
      IConversion convToVoidStar = convToElementPtrType.ValueToConvert as IConversion;
      if (convToVoidStar == null) return false;
      IAddressOf addressOf = convToVoidStar.ValueToConvert as IAddressOf;
      if (addressOf == null) return false;
      IAddressableExpression addressableExpr = addressOf.Expression as IAddressableExpression;
      if (addressableExpr == null) return false;
      // if we have come this far, we know that the original expresion has been of the form
      // s.a[i], translate it accordingly
      IFieldDefinition fieldDef = addressableExpr.Definition as IFieldDefinition;
      if (fieldDef == null) return false;
      ITypeDefinition fieldType = fieldDef.Type.ResolvedType;
      if (!IsFixedSizeArray(fieldType)) return false;
      arrayType = fieldType;
      elementType = GetFixedSizeArrayElementType(fieldType);
      BoundExpression bndExpr = new BoundExpression();
      bndExpr.Definition = addressableExpr.Definition;
      bndExpr.Instance = addressableExpr.Instance;
      boundExpr = bndExpr;
      offsetAndScale = ptrAddition.RightOperand;
      return true;
    }

    public static bool TryInterpretAsFixedSizeArrayAccess(IExpression expr, out IBoundExpression boundExpr)
    {
      boundExpr = null;
      IAddressDereference addressDereference = expr as IAddressDereference;
      if (addressDereference == null) return false;
      ITypeDefinition arrayType;
      ITypeDefinition elementType;
      IExpression offsetAndScale;
      return TryInterpretAsFixedSizeArrayAccess(addressDereference, out arrayType, out elementType, out boundExpr, out offsetAndScale);
    }

    public static bool IsFixedSizeArray(ITypeDefinition parType)
    {
      if (parType.SizeOf == 0) return false;
      INestedTypeDefinition/*?*/ nestedType = parType as INestedTypeDefinition;
      if (nestedType == null) return false;
      return nestedType.Name.Value.StartsWith("_FixedArrayOfSize");
    }

    public static ITypeDefinition GetFixedSizeArrayElementType(ITypeDefinition fixedSizeArrayType)
    {
      foreach (ITypeDefinitionMember member in fixedSizeArrayType.Members) {
        IFieldDefinition/*?*/ field = member as IFieldDefinition;
        if (field != null && field.Name.Value == "_ElementType")
          return field.Type.ResolvedType;
      }
      return null;
    }

    public static ISourceLocation LocationFromToken(Token tok)
    {
      SourceLocationWrapper wrap = tok as SourceLocationWrapper;
      if (wrap != null) return wrap.sourceLocation;
      ForwardingToken fwd = tok as ForwardingToken;
      if (fwd != null) return LocationFromToken(fwd.tok);
      LazyToken lazyToken = tok as LazyToken;
      if (lazyToken != null) return LocationFromToken(lazyToken.DelayedToken);
      return SourceDummy.SourceLocation;
    }
  }
}
