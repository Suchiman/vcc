//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Resources;
using Microsoft.Cci.Ast;
using System.Diagnostics;
using IMethodContract=Microsoft.Cci.Contracts.IMethodContract;
using Microsoft.Cci;
using Microsoft.Research.Vcc.Parsing;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc {

  /// <summary>
  /// A polymorphic compile constant that should be interpreted as either signed or unsigned, depending on the type of another expression with which it is combined in a binary operator expression.
  /// </summary>
  class CompileTimeConstantWhoseSignDependsOnAnotherExpression : CompileTimeConstant {

    /// <summary>
    /// Initializes a numeric literal that could be interpreted as either signed or unsigned, depending on the type of another expression with which it is combined in a binary operator expression.
    /// </summary>
    /// <param name="compileTimeConstant">A polymorphic compile time constant.</param>
    /// <param name="expression">An expression that determines which sign this polymorhpic sign agnostic constant will assume when asked what its type is.</param>
    /// <param name="sourceLocation">The location in the source text of the expression that corresponds to this constant.</param>
    public CompileTimeConstantWhoseSignDependsOnAnotherExpression(CompileTimeConstant compileTimeConstant, Expression expression)
      : base(null, true, compileTimeConstant.SourceLocation)
      //^ requires compileTimeConstant.ValueIsPolymorhpicCompileTimeConstant;
    {
      this.compileTimeConstant = compileTimeConstant;
      this.expression = expression;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected CompileTimeConstantWhoseSignDependsOnAnotherExpression(BlockStatement containingBlock, CompileTimeConstantWhoseSignDependsOnAnotherExpression template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
      this.compileTimeConstant = (CompileTimeConstant)template.compileTimeConstant.MakeCopyFor(containingBlock);
      this.expression = template.expression.MakeCopyFor(containingBlock);
    }

    /// <summary>
    /// A polymorphic compile time constant.
    /// </summary>
    CompileTimeConstant compileTimeConstant;

    /// <summary>
    /// True if the constant is a positive integer that could be interpreted as a negative signed integer.
    /// For example, 0x80000000, could be interpreted as a convenient way of writing int.MinValue.
    /// </summary>
    public override bool CouldBeInterpretedAsNegativeSignedInteger {
      get { return this.compileTimeConstant.CouldBeInterpretedAsNegativeSignedInteger;  }
    }

    /// <summary>
    /// True if this expression is a constant negative integer that could also be interpreted as a unsigned integer.
    /// For example, 1 &lt;&lt; 31 could also be interpreted as a convenient way of writing 0x80000000.
    /// </summary>
    public override bool CouldBeInterpretedAsUnsignedInteger {
      get { return this.compileTimeConstant.CouldBeInterpretedAsUnsignedInteger; }
    }

    /// <summary>
    /// An expression that determines which sign this polymorhpic sign agnostic constant will assume when asked what its type is.
    /// </summary>
    Expression expression;

    /// <summary>
    /// Computes the compile time value of the expression. Can be null.
    /// </summary>
    protected override object/*?*/ GetValue() {
      object/*?*/ value = this.compileTimeConstant.Value;
      if (TypeHelper.IsUnsignedPrimitiveInteger(this.expression.Type)) {
        IPlatformType platformType = this.PlatformType;
        IConvertible/*?*/ ic = value as IConvertible;
        if (ic == null) return platformType.SystemObject;
        switch (ic.GetTypeCode()) {
          case System.TypeCode.Int16: return (ushort)ic.ToInt16(null);
          case System.TypeCode.Int32: return (uint)ic.ToInt32(null);
          case System.TypeCode.Int64: return (ulong)ic.ToInt64(null);
          case System.TypeCode.SByte: return (byte)ic.ToSByte(null);
        }
      }
      return value;
    }

    /// <summary>
    /// Infers the type of value that this expression will evaluate to. At runtime the actual value may be an instance of subclass of the result of this method.
    /// Calling this method does not cache the computed value and does not generate any error messages. In some cases, such as references to the parameters of lambda
    /// expressions during type overload resolution, the value returned by this method may be different from one call to the next.
    /// When type inference fails, Dummy.Type is returned.
    /// </summary>
    public override ITypeDefinition InferType() {
      if (TypeHelper.IsUnsignedPrimitiveInteger(this.expression.Type)) {
        IPlatformType platformType = this.PlatformType;
        IConvertible/*?*/ ic = this.compileTimeConstant.Value as IConvertible;
        if (ic == null) return platformType.SystemObject.ResolvedType;
        switch (ic.GetTypeCode()) {
          case System.TypeCode.Int16: return platformType.SystemUInt16.ResolvedType;
          case System.TypeCode.Int32: return platformType.SystemUInt32.ResolvedType;
          case System.TypeCode.Int64: return platformType.SystemUInt64.ResolvedType;
          case System.TypeCode.SByte: return platformType.SystemUInt8.ResolvedType;
        }
      }
      return base.InferType();
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (containingBlock == this.ContainingBlock) return this;
      return new CompileTimeConstantWhoseSignDependsOnAnotherExpression(containingBlock, this);
    }

    /// <summary>
    /// Completes the two stage construction of this object. This allows bottom up parsers to construct an Expression before constructing the containing Expression.
    /// This method should be called once only and must be called before this object is made available to client code. The construction code itself should also take
    /// care not to call any other methods or property/event accessors on the object until after this method has been called.
    /// </summary>
    public override void SetContainingExpression(Expression containingExpression) {
      base.SetContainingExpression(containingExpression);
      this.compileTimeConstant.SetContainingExpression(containingExpression);
      this.expression.SetContainingExpression(containingExpression);
    }

  }
  
  /// <summary>
  /// An expression that adds or concatenates the value of the left operand to the value of the right operand. When overloaded, this expression corresponds to a call to op_Addition.
  /// </summary>
  public class VccAddition : Addition {
    /// <summary>
    /// Allocates an expression that adds or concatenates the value of the left operand to the value of the right operand. When overloaded, this expression corresponds to a call to op_Addition.
    /// </summary>
    /// <param name="leftOperand">The left operand.</param>
    /// <param name="rightOperand">The right operand.</param>
    /// <param name="sourceLocation">The source location of the operation.</param>
    public VccAddition(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    private VccAddition(BlockStatement containingBlock, VccAddition template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Returns a collection of methods that represents the overloads for ptr + index.
    /// </summary>
    private IEnumerable<IMethodDefinition> GetLeftPointerAdditionMethods(ITypeDefinition pointerType) {
      BuiltinMethods dummyMethods = this.Compilation.BuiltinMethods;
      yield return dummyMethods.GetDummyOp(pointerType, pointerType, this.PlatformType.SystemInt32.ResolvedType);
      yield return dummyMethods.GetDummyOp(pointerType, pointerType, this.PlatformType.SystemUInt32.ResolvedType);
      yield return dummyMethods.GetDummyOp(pointerType, pointerType, this.PlatformType.SystemInt64.ResolvedType);
      yield return dummyMethods.GetDummyOp(pointerType, pointerType, this.PlatformType.SystemUInt64.ResolvedType);
    }

    /// <summary>
    /// Returns a collection of methods that represents the overloads for index + ptr.
    /// </summary>
    private IEnumerable<IMethodDefinition> GetRightPointerAdditionMethods(ITypeDefinition pointerType) {
      BuiltinMethods dummyMethods = this.Compilation.BuiltinMethods;
      yield return dummyMethods.GetDummyOp(pointerType, this.PlatformType.SystemInt32.ResolvedType, pointerType);
      yield return dummyMethods.GetDummyOp(pointerType, this.PlatformType.SystemUInt32.ResolvedType, pointerType);
      yield return dummyMethods.GetDummyOp(pointerType, this.PlatformType.SystemInt64.ResolvedType, pointerType);
      yield return dummyMethods.GetDummyOp(pointerType, this.PlatformType.SystemUInt64.ResolvedType, pointerType);
    }

    ITypeDefinition/*?*/ LeftOperandFixedArrayElementType {
      get {
        ITypeDefinition leftOperandType = this.LeftOperand.Type;
        if (/*leftOperandType.IsSpecialName &&*/ leftOperandType.IsStruct && leftOperandType.SizeOf > 0) {
          IFieldDefinition/*?*/ field = TypeHelper.GetField(leftOperandType, this.Helper.NameTable.GetNameFor("_ElementType"));
          if (field != null) return field.Type.ResolvedType;
        }
        return null;
      }
    }

    /// <summary>
    /// Returns the user defined operator overload method, or a dummy method corresponding to an IL operation, that best
    /// matches the operand types of this operation.
    /// </summary>
    protected override IMethodDefinition LookForOverloadMethod() {
      IMethodDefinition result = base.LookForOverloadMethod();
      IPointerType/*?*/ resultType = result.Type.ResolvedType as IPointerType;
      if (resultType != null) {
        if (this.LeftOperand.ValueIsPolymorphicCompileTimeConstant && this.LeftOperand.CouldBeInterpretedAsNegativeSignedInteger)
          return this.Compilation.BuiltinMethods.GetDummyOp(resultType, this.LeftOperand.Type, resultType);
        else if (this.RightOperand.ValueIsPolymorphicCompileTimeConstant && this.RightOperand.CouldBeInterpretedAsNegativeSignedInteger)
          return this.Compilation.BuiltinMethods.GetDummyOp(resultType, resultType, this.RightOperand.Type);
      }
      return VccBitwiseAnd.ProvideUnsignedBias(result, this.LeftOperand, this.RightOperand, this.Compilation.BuiltinMethods);
    }

    /// <summary>
    /// Returns true if no information is lost if the integer value of this expression is converted to the target integer type.
    /// </summary>
    /// <param name="targetType"></param>
    /// <returns></returns>
    public override bool IntegerConversionIsLossless(ITypeDefinition targetType)
    {
      return (this.LeftOperand.IntegerConversionIsLossless(targetType) &&
              this.RightOperand.IntegerConversionIsLossless(targetType)) 
             || base.IntegerConversionIsLossless(targetType);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccAddition(containingBlock, this);
    }

    ITypeDefinition/*?*/ RightOperandFixedArrayElementType {
      get {
        ITypeDefinition rightOperandType = this.RightOperand.Type;
        if (/*leftOperandType.IsSpecialName &&*/ rightOperandType.IsStruct && rightOperandType.SizeOf > 0) {
          IFieldDefinition/*?*/ field = TypeHelper.GetField(rightOperandType, this.Helper.NameTable.GetNameFor("_ElementType"));
          if (field != null) return field.Type.ResolvedType;
        }
        return null;
      }
    }

    /// <summary>
    /// A list of dummy methods that correspond to operations that are built into IL. The dummy methods are used, via overload resolution,
    /// to determine how the operands are to be converted before the operation is carried out.
    /// </summary>
    protected override IEnumerable<IMethodDefinition> StandardOperators {
      get {
        ITypeDefinition/*?*/ leftOperandFixedArrayElementType = this.LeftOperandFixedArrayElementType;
        if (leftOperandFixedArrayElementType != null)
          return this.GetLeftPointerAdditionMethods(PointerType.GetPointerType(leftOperandFixedArrayElementType, this.Compilation.HostEnvironment.InternFactory));
        ITypeDefinition/*?*/ rightOperandFixedArrayElementType = this.RightOperandFixedArrayElementType;
        if (rightOperandFixedArrayElementType != null)
          return this.GetRightPointerAdditionMethods(PointerType.GetPointerType(rightOperandFixedArrayElementType, this.Compilation.HostEnvironment.InternFactory));
        return base.StandardOperators;
      }
    }

  }

  /// <summary>
  /// An expression that adds or concatenates the value of the left operand with the value of the right operand.
  /// The result of the expression is assigned to the left operand, which must be a target expression.
  /// Both operands must be primitives types.
  /// </summary>
  public class VccAdditionAssignment : AdditionAssignment {

    /// <summary>
    /// Allocates an expression that adds or concatenates the value of the left operand with the value of the right operand.
    /// The result of the expression is assigned to the left operand, which must be a target expression.
    /// Both operands must be primitives types.
    /// </summary>
    /// <param name="leftOperand">The left operand and target of the assignment.</param>
    /// <param name="rightOperand">The right operand.</param>
    /// <param name="sourceLocation">The source location of the operation.</param>
    public VccAdditionAssignment(TargetExpression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccAdditionAssignment(BlockStatement containingBlock, VccAdditionAssignment template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Calls the visitor.Visit(AdditionAssignment) method.
    /// </summary>
    public override void Dispatch(SourceVisitor visitor) {
      visitor.Visit(this);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (this.ContainingBlock == containingBlock) return this;
      AdditionAssignment result = new VccAdditionAssignment(containingBlock, this);
      //^ assume result.ContainingBlock == containingBlock; //This should be a post condition of the constructor, but such post conditions are not currently permitted by the methodology.
      return result;
    }

    /// <summary>
    /// Creates an addition expression with the given left operand and this.RightOperand.
    /// The method does not use this.LeftOperand.Expression, since it may be necessary to factor out any subexpressions so that
    /// they are evaluated only once. The given left operand expression is expected to be the expression that remains after factoring.
    /// </summary>
    /// <param name="leftOperand">An expression to combine with this.RightOperand into a binary expression.</param>
    protected override Expression CreateBinaryExpression(Expression leftOperand) {
      Expression result = new VccAddition(leftOperand, this.RightOperand, this.SourceLocation);
      result.SetContainingExpression(this);
      return result;
    }
  }

  public class VccAddressableExpression : AddressableExpression
  {
    /// <summary>
    /// Allocates an expression that denotes a value that has an address in memory, such as a local variable, parameter, field, array element, pointer target, or method.
    /// </summary>
    /// <param name="expression">An expression that is expected to denote a value that has an address in memory.</param>
    public VccAddressableExpression(Expression expression)
      : base(expression)
    {
    }

    /// <summary>
    /// Allocates an expression that denotes a value that has an address in memory, such as a local variable, parameter, field, array element, pointer target, or method.
    /// </summary>
    /// <param name="expression">An expression that is expected to denote a value that has an address in memory.</param>
    internal VccAddressableExpression(Expression expression, bool fromProjectionOfFixedSizeArray)
      : base(expression)
    {
      this.fromProjectionOfFixedSizeArray = fromProjectionOfFixedSizeArray;
    }

    /// <summary>
    /// Allocates an expression that denotes a value that has an address in memory, such as a local variable, parameter, field, array element, pointer target, or method.
    /// </summary>
    /// <param name="expression">The expression that is used as the target of an explicit or implicit assignment.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated target expression.</param>
    public VccAddressableExpression(Expression expression, ISourceLocation sourceLocation)
      : base(expression, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccAddressableExpression(BlockStatement containingBlock, VccAddressableExpression template)
      : base(containingBlock, template)
      //^ requires template.Expression.ContainingBlock != containingBlock;
    {
      this.fromProjectionOfFixedSizeArray = template.fromProjectionOfFixedSizeArray;
    }

    public override object Resolve()
    {
      if (this.fromProjectionOfFixedSizeArray) {
        object baseResolve = base.Resolve();
        return (baseResolve != null) ? baseResolve : this.Expression;
      }
      else {
        return base.Resolve();
      }
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      return this.fromProjectionOfFixedSizeArray ? false : base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

    protected override AddressableExpression CreateAddressableExpr(Expression expr)
    {
      return new VccAddressableExpression(expr, this.fromProjectionOfFixedSizeArray);
    }

    private readonly bool fromProjectionOfFixedSizeArray;

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.Expression.ContainingBlock) return this;
      return new VccAddressableExpression(containingBlock, this);
    }
  }

  /// <summary>
  /// An expression that deferences an address (pointer).
  /// </summary>
  public class VccAddressDereference : AddressDereference, IAddressDereference {

    /// <summary>
    /// Allocates an expression that deferences an address (pointer).
    /// </summary>
    /// <param name="address">The address to dereference.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccAddressDereference(Expression address, ISourceLocation sourceLocation)
      : base(address, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccAddressDereference(BlockStatement containingBlock, VccAddressDereference template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      bool result = base.CheckForErrorsAndReturnTrueIfAnyAreFound();
      if (this.Type == Dummy.Type) this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.IllegalIndirection));
      result |= this.ConvertedAddress.HasErrors();
      return result;
    }

    /// <summary>
    /// The address to dereference, after it has been converted to an actual address. For example, a fixed size array is a struct to the CLR
    /// and its address must be taken and converted to a pointer before it can be deferenced.
    /// </summary>
    public Expression ConvertedAddress {
      get {
        if (this.convertedAddress == null)
          this.convertedAddress = this.GetConvertedAddress();
        return this.convertedAddress;
      }
    }
    //^ [Once]
    Expression/*?*/ convertedAddress;

    /// <summary>
    /// Returns this.Address unless it is a fixed size array. In that case, returns the address of the array value, after conversion to a pointer
    /// to the element type of the array.
    /// </summary>
    private Expression GetConvertedAddress() {
      if (this.Address.Type is IPointerType || this.Type == Dummy.Type) return this.Address;
      VccAddressOf addressOf = new VccAddressOf(new AddressableExpression(this.Address), this.Address.SourceLocation);
      addressOf.SetContainingExpression(this);
      return addressOf;
    }

    /// <summary>
    /// Infers the type of value that this expression will evaluate to. At runtime the actual value may be an instance of subclass of the result of this method.
    /// Calling this method does not cache the computed value and does not generate any error messages. In some cases, such as references to the parameters of lambda
    /// expressions during type overload resolution, the value returned by this method may be different from one call to the next.
    /// When type inference fails, Dummy.Type is returned.
    /// </summary>
    public override ITypeDefinition InferType() {
      ITypeDefinition result = base.InferType();
      if (result == Dummy.Type) {
        NestedTypeDefinition/*?*/ addressType = this.Address.Type as NestedTypeDefinition;
        if (addressType != null && addressType.Name.Value.StartsWith("_FixedArrayOfSize", StringComparison.Ordinal)) {
          foreach (ITypeDefinitionMember member in addressType.Members) {
            IFieldDefinition/*?*/ field = member as IFieldDefinition;
            if (field != null && field.Name.Value == "_ElementType")
              return field.Type.ResolvedType;
          }
        }
      }
      return result;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccAddressDereference(containingBlock, this);
    }

    #region IAddressDereference Members

    IExpression IAddressDereference.Address {
      get { return this.ConvertedAddress.ProjectAsIExpression(); }
    }

    #endregion
  }
  
  /// <summary>
  /// An expression that takes the address of a target expression.
  /// </summary>
  public class VccAddressOf : AddressOf {

    /// <summary>
    /// Allocates an expression that takes the address of a target expression.
    /// </summary>
    /// <param name="address">An expression that represents an addressable location in memory.</param>
    /// <param name="sourceLocation"></param>
    public VccAddressOf(AddressableExpression address, ISourceLocation sourceLocation)
      : base(address, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    public VccAddressOf(BlockStatement containingBlock, VccAddressOf template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Infers the type of value that this expression will evaluate to. At runtime the actual value may be an instance of subclass of the result of this method.
    /// Calling this method does not cache the computed value and does not generate any error messages. In some cases, such as references to the parameters of lambda
    /// expressions during type overload resolution, the value returned by this method may be different from one call to the next.
    /// When type inference fails, Dummy.Type is returned.
    /// </summary>
    public override ITypeDefinition InferType() {
      IPointerType/*?*/ pointerType = ((VccCompilationHelper)this.Helper).ArrayPointerFor(this.Address.Type);
      if (pointerType != null) return pointerType;
      return base.InferType();
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      var fieldDef = this.Address.Definition as IFieldDefinition;
      if (fieldDef != null && fieldDef.IsBitField) {
        this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.AddressOfBitField));
        return true;
      }

      if (this.Address.Definition is VccThisReference) {
        this.Helper.ReportError(new AstErrorMessage(this, Cci.Ast.Error.CannotTakeAddress));
        return true;
      }

      return base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccAddressOf(containingBlock, this);
    }

  }

  /// <summary>
  /// An expression that computes the memory size of instances of a given type at runtime.
  /// </summary>
  public class VccAlignOf : Expression {

    /// <summary>
    /// Allocates an expression that computes the memory size of instances of a given type at runtime.
    /// </summary>
    /// <param name="expression">The type to size, or the value to size.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccAlignOf(TypeExpression type, ISourceLocation sourceLocation)
      : base(sourceLocation)
    {
      this.typeExpression = type;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccAlignOf(BlockStatement containingBlock, VccAlignOf template)
      : base(containingBlock, template)
    //^ requires template.ContainingBlock != containingBlock;
    //^ ensures this.containingBlock == containingBlock;
    {
      this.typeExpression = (TypeExpression)template.typeExpression.MakeCopyFor(containingBlock);
    }

    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      return this.TypeExpression.HasErrors();
    }

    /// <summary>
    /// The type of which the alignment to get.
    /// </summary>
    public TypeExpression TypeExpression
    {
      get { return this.typeExpression; }
    }
    readonly TypeExpression typeExpression;

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccAlignOf(containingBlock, this);
    }

    /// <summary>
    /// Returns an object that implements IExpression and that represents this expression after language specific rules have been
    /// applied to it in order to determine its semantics. The resulting expression is a standard representation of the semantics
    /// of this expression, suitable for use by language agnostic clients and complete enough for translation of the expression
    /// into IL.
    /// </summary>
    protected override IExpression ProjectAsNonConstantIExpression()
    {     
      CompileTimeConstant result = new CompileTimeConstant(TypeHelper.TypeAlignment(this.typeExpression.ResolvedType), this.SourceLocation);
      result.SetContainingExpression(this);
      return result;
    }


    /// <summary>
    /// Completes the two stage construction of this object. This allows bottom up parsers to construct an Expression before constructing the containing Expression.
    /// This method should be called once only and must be called before this object is made available to client code. The construction code itself should also take
    /// care not to call any other methods or property/event accessors on the object until after this method has been called.
    /// </summary>
    public override void SetContainingExpression(Expression containingExpression)
    {
      base.SetContainingExpression(containingExpression);
      this.typeExpression.SetContainingExpression(this);
    }

    /// <summary>
    /// The type of value that the expression will evaluate to, as determined at compile time.
    /// </summary>
    public override ITypeDefinition Type
    {
      get { return this.PlatformType.SystemUInt32.ResolvedType; }
    }

    /// <summary>
    /// Returns true if the expression represents a compile time constant without an explicitly specified type. For example, 1 rather than 1L.
    /// Constant expressions such as 2*16 are polymorhpic if both operands are polymorhic.
    /// </summary>
    public override bool ValueIsPolymorphicCompileTimeConstant
    {
      get
      {
        return this.Value != null;
      }
    }
  }

  public class VccArrayTypeExpression : ArrayTypeExpression {
    /// <summary>
    /// Allocates an expression that denotes an array type.
    /// </summary>
    /// <param name="elementType">The type of the elements of this array.</param>
    /// <param name="rank">The number of array dimensions.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccArrayTypeExpression(TypeExpression elementType, Expression/*?*/ size, ISourceLocation sourceLocation)
      : base(elementType, 1, sourceLocation)
    {
      this.Size = size;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    private VccArrayTypeExpression(BlockStatement containingBlock, VccArrayTypeExpression template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
      this.Size = template.Size;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccArrayTypeExpression(containingBlock, this);
    }

    protected override ITypeDefinition Resolve() {
      if (this.Size == null || this.SizeAsInt32 == 0) 
        return PointerType.GetPointerType(this.ElementType.ResolvedType, this.Compilation.HostEnvironment.InternFactory);
      return this.VccCompilationPart.GetFixedSizeArrayType(this.ElementType.ResolvedType, (uint)this.SizeAsInt32).TypeDefinition;
    }

    public override void SetContainingExpression(Expression containingExpression) {
      if (this.Size != null) 
        this.Size.SetContainingExpression(containingExpression);
      base.SetContainingExpression(containingExpression);
    }

    /// <summary>
    /// The number of embedded "atomic" (that is, non-array) elements for one element of the current array.
    /// For int A[2][3][4], it is the number of atomic elements of A[0], which will be 12. 
    /// </summary>
    internal int SizeOfEmbeddedArrays {
      get {
        VccArrayTypeExpression embeddedArray = ElementType as VccArrayTypeExpression;
        int result = 1;
        if (embeddedArray != null) {
          result = embeddedArray.SizeAsInt32 * embeddedArray.SizeOfEmbeddedArrays;
        }
        return result;
      }
    }

    internal int SizeAsInt32 {
      get
        //^ requires this.Size != null;
        //^ ensures result >= 0;
      {
        if (this.sizeAsInt32 == null) {
          int size = 0;
          Expression convertedExpression = this.Helper.ImplicitConversion(this.Size, this.PlatformType.SystemInt32.ResolvedType);
          object/*?*/ val = convertedExpression.Value;
          if (val == null) {
            if (!this.Size.HasErrors() && !convertedExpression.HasErrors()) {
              this.ContainingBlock.Helper.ReportError(new VccErrorMessage(this.Size.SourceLocation, Error.ExpectedConstantExpression));
            }
          }
          if (val is int) {
            size = (int)val;
            if (size < 0) size = 0;
            //TODO: generate an error message about negative array size
          }
          this.sizeAsInt32 = size;
        }
        return (int)this.sizeAsInt32;
      }
    }
    int? sizeAsInt32;

    internal Expression/*?*/ Size;

    private VccCompilationPart VccCompilationPart {
      get {
        return (VccCompilationPart)this.ContainingBlock.CompilationPart;
      }
    }

    // Call once
    internal void ResetSizeWhenProvidedByInitializer(Expression e) {
      if (Size != null) return;
      Size = e;
    }
  }

  /// <summary>
  /// An expression that assigns the value of the source (right) operand to the location represented by the target (left) operand.
  /// The expression result is the value of the source expression.
  /// </summary>
  public class VccAssignment : Assignment, IAssignment {
    /// <summary>
    /// Allocates an expression that assigns the value of the source (right) operand to the location represented by the target (left) operand.
    /// The expression result is the value of the source expression.
    /// </summary>
    /// <param name="target">The target of the assignment, for example simple name or a qualified name or an indexer.</param>
    /// <param name="source">An expression that results in a value that is to be assigned to the target.</param>
    /// <param name="sourceLocation">The source location of the operation.</param>
    public VccAssignment(TargetExpression target, Expression source, ISourceLocation sourceLocation)
      : base(target, source, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccAssignment(BlockStatement containingBlock, VccAssignment template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      return this.TargetWithoutCasts.HasErrors() || this.ConvertedSourceExpression is DummyExpression;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccAssignment(containingBlock, this);
    }

    protected override IExpression ProjectAsNonConstantIExpression() {
      IExpression result = base.ProjectAsNonConstantIExpression();
      if (this.TargetWithoutCasts != this.Target)
        result = new AssignmentConversion(result, this.Type);
      return result;
    }

    private TargetExpression TargetWithoutCasts {
      get {
        if (this.targetWithoutCasts == null) {
          Expression expr = this.Target.Expression;
          while (true) {
            Parenthesis/*?*/ parExpr = expr as Parenthesis;
            if (parExpr != null) { expr = parExpr.ParenthesizedExpression; continue; }
            Cast/*?*/ castExpr = expr as Cast;
            if (castExpr != null) { expr = castExpr.ValueToCast; continue; }
            break;
          }
          if (expr != this.Target.Expression) {
            TargetExpression targetWithoutCasts = new TargetExpression(expr);
            targetWithoutCasts.SetContainingExpression(this);
            this.targetWithoutCasts = targetWithoutCasts;
          } else
            this.targetWithoutCasts = this.Target;
        }
        return this.targetWithoutCasts;
      }
    }
    TargetExpression/*?*/ targetWithoutCasts;

    class AssignmentConversion : IConversion {

      internal AssignmentConversion(IExpression valueToConvert, ITypeDefinition type) {
        this.valueToConvert = valueToConvert;
        this.type = type;
      }

      public IExpression ValueToConvert {
        get { return this.valueToConvert; }
      }
      readonly IExpression valueToConvert;

      public bool CheckNumericRange {
        get { return false; }
      }

      public void Dispatch(ICodeVisitor visitor) {
        visitor.Visit(this);
      }

      public bool HasErrors() {
        return this.ValueToConvert.HasErrors();
      }

      public IEnumerable<ILocation> Locations {
        get { return this.ValueToConvert.Locations; }
      }

      public ITypeDefinition Type {
        get { return this.type; }
      }
      ITypeDefinition type;

       public ITypeReference TypeAfterConversion {
        get { return this.Type; }
      }

      public bool IsPure
      {
        get { return this.ValueToConvert.IsPure; }
      }

      #region IExpression Members

      ITypeReference IExpression.Type {
        get { return this.Type; }
      }

      #endregion
    }

    #region IAssignment Members

    IExpression IAssignment.Source {
      [DebuggerNonUserCode]
      get { 
        return this.Helper.ExplicitConversion(this.ConvertedSourceExpression, this.TargetWithoutCasts.Type).ProjectAsIExpression(); 
      }
    }

    ITargetExpression IAssignment.Target {
      [DebuggerNonUserCode]
      get { return this.TargetWithoutCasts; }
    }

    #endregion
  }

  public class VccByteStringLiteral : CompileTimeConstant {

    public VccByteStringLiteral(string value, ISourceLocation sourceLocation)
      : base(value, sourceLocation) {
    }

    private VccByteStringLiteral(BlockStatement containingBlock, VccByteStringLiteral template)
      : base(containingBlock, template) {
    }

    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      //TODO: check that every character fits in a byte.
      return false;
    }

    /// <summary>
    /// Returns a byte array representing the part of the process image to which this field will be mapped. Can be null.
    /// </summary>
    internal byte[]/*?*/ GetMappedData() {
      string/*?*/ str = this.Value as string;
      if (str == null) return null;
      byte[] byteString = new byte[str.Length+1];
      for (int i = 0, n = str.Length; i < n; i++)
        byteString[i] = (byte)str[i];
      return byteString;
    }

    /// <summary>
    /// Infers the type of value that this expression will evaluate to. At runtime the actual value may be an instance of subclass of the result of this method.
    /// Calling this method does not cache the computed value and does not generate any error messages. In some cases, such as references to the parameters of lambda
    /// expressions during type overload resolution, the value returned by this method may be different from one call to the next.
    /// When type inference fails, Dummy.Type is returned.
    /// </summary>
    public override ITypeDefinition InferType() {
      return PointerType.GetPointerType(this.PlatformType.SystemInt8, this.Compilation.HostEnvironment.InternFactory);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccByteStringLiteral(containingBlock, this);
    }

    public override IMetadataExpression ProjectAsIMetadataExpression()
    {
      return this;
    }

    protected override IExpression ProjectAsNonConstantIExpression() {
      return this.ProjectedExpression;
    }

    private IExpression ProjectedExpression {
      get {
        if (this.projectedExpression == null) {
          lock (this.Helper) {
            if (this.projectedExpression == null) {
              this.projectedExpression = this.GetProjectedExpression();
            }
          }
        }
        return this.projectedExpression;
      }
    }
    //^ [Once]
    IExpression/*?*/ projectedExpression;

    private IExpression GetProjectedExpression() {
      VccCompilationHelper helper = (VccCompilationHelper)this.Helper;
      string/*?*/ str = this.Value as string;
      if (str == null) return CodeDummy.Expression; //TODO: provide invariant that guarantees that str is never null.
      GlobalVariableDeclaration/*?*/ globalVar;
      if (!helper.StringTable.TryGetValue(str, out globalVar)) {
        NameDeclaration dummyName = new NameDeclaration(this.NameTable.GetNameFor("?mappedLiteral"+this.GetHashCode()), this.SourceLocation);
        VccArrayTypeExpression arrayType = new VccArrayTypeExpression(TypeExpression.For(this.PlatformType.SystemUInt8.ResolvedType), new CompileTimeConstant(str.Length+1, SourceDummy.SourceLocation), SourceDummy.SourceLocation);
        globalVar = new GlobalVariableDeclaration(FieldDeclaration.Flags.ReadOnly, TypeMemberVisibility.Assembly, arrayType, dummyName, this, this.SourceLocation);
        if (this.ContainingBlock.ContainingTypeDeclaration != null) {
          this.ContainingBlock.ContainingTypeDeclaration.AddHelperMember(globalVar);
          globalVar.SetContainingTypeDeclaration(this.ContainingBlock.ContainingTypeDeclaration, true);
          helper.StringTable.Add(str, globalVar);
        } else {
          return CodeDummy.Expression;
          //TODO: error
        }
      }
      //^ assume globalVar != null;      
      AddressableExpression fieldRef = new AddressableExpression(new BoundExpression(this, globalVar.FieldDefinition));
      AddressOf addressOf = new AddressOf(fieldRef, this.SourceLocation);
      addressOf.SetContainingExpression(this);
      Conversion conversion = new Conversion(addressOf, this.Type, this.SourceLocation);
      return conversion.ProjectAsIExpression();
    }
  }

  public class VccBitwiseAnd : BitwiseAnd
  {
    public VccBitwiseAnd(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    protected VccBitwiseAnd(BlockStatement containingBlock, VccBitwiseAnd template)
      : base(containingBlock, template)
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccBitwiseAnd(containingBlock, this);
    }

    static internal IMethodDefinition ProvideUnsignedBias(IMethodDefinition method, Expression leftOperand, Expression rightOperand, BuiltinMethods builtinMethods) 
    {
      if (TypeHelper.IsSignedPrimitiveInteger(method.Type) || method == Dummy.Method) {
        if (TypeHelper.IsUnsignedPrimitiveInteger(leftOperand.Type) && rightOperand.ValueIsPolymorphicCompileTimeConstant) {
          if (leftOperand.Type.TypeCode == PrimitiveTypeCode.UInt64)
            return builtinMethods.UInt64opUInt64;
          else if (TypeHelper.SizeOfType(rightOperand.Type.ResolvedType) <= sizeof(UInt32)) 
            return builtinMethods.UInt32opUInt32;
        } else if (TypeHelper.IsUnsignedPrimitiveInteger(rightOperand.Type) && leftOperand.ValueIsPolymorphicCompileTimeConstant) {
          if (rightOperand.Type.TypeCode == PrimitiveTypeCode.UInt64)
            return builtinMethods.UInt64opUInt64;
          else if (TypeHelper.SizeOfType(leftOperand.Type.ResolvedType) <= sizeof(UInt32)) 
            return builtinMethods.UInt32opUInt32;
        }
      }
      return method;
    }

    protected override IMethodDefinition  LookForOverloadMethod()
    {
      IMethodDefinition method = base.LookForOverloadMethod();
      return ProvideUnsignedBias(method, this.LeftOperand, this.RightOperand, this.Compilation.BuiltinMethods);
    }

    private IEnumerable<IMethodDefinition> BaseStandardOperators
    {
      get { return base.StandardOperators; }
    }

    protected override IEnumerable<IMethodDefinition> StandardOperators
    {      
      get
      {
        BuiltinMethods dummyMethods = this.Compilation.BuiltinMethods;
        IEnumerable<IMethodDefinition> baseStandardOperators = this.BaseStandardOperators;
        foreach (IMethodDefinition method in baseStandardOperators) {
          if (method != dummyMethods.BoolOpBool) yield return method;
        }
      }
    }
  }

  public class VccBitwiseOr : BitwiseOr
  {
    public VccBitwiseOr(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    protected VccBitwiseOr(BlockStatement containingBlock, VccBitwiseOr template)
      : base(containingBlock, template)
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccBitwiseOr(containingBlock, this);
    }

    protected override IMethodDefinition  LookForOverloadMethod()
    {
      IMethodDefinition method = base.LookForOverloadMethod();
      return VccBitwiseAnd.ProvideUnsignedBias(method, this.LeftOperand, this.RightOperand, this.Compilation.BuiltinMethods);
    }

    private IEnumerable<IMethodDefinition> BaseStandardOperators
    {
      get { return base.StandardOperators; }
    }

    protected override IEnumerable<IMethodDefinition> StandardOperators
    {
      get
      {
        BuiltinMethods dummyMethods = this.Compilation.BuiltinMethods;
        IEnumerable<IMethodDefinition> baseStandardOperators = this.BaseStandardOperators;
        foreach (IMethodDefinition method in baseStandardOperators)
        {
          if (method != dummyMethods.BoolOpBool) yield return method;
        }
      }
    }
  }

  public class VccExplies : Implies {
    public VccExplies(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(rightOperand, leftOperand, sourceLocation) {
    }

    protected VccExplies(BlockStatement containingBlock, VccExplies template)
      : base(containingBlock, template) {
    }

    protected override string OperationSymbolForErrorMessage {
      get {
        return "<==";
      }
    }

    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccExplies(containingBlock, this);
    }
  }


  public class VccExclusiveOr : ExclusiveOr
  {
    public VccExclusiveOr(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    protected VccExclusiveOr(BlockStatement containingBlock, VccExclusiveOr template)
      : base(containingBlock, template)
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccExclusiveOr(containingBlock, this);
    }

    protected override IMethodDefinition  LookForOverloadMethod()
    {
      IMethodDefinition method = base.LookForOverloadMethod();
      return VccBitwiseAnd.ProvideUnsignedBias(method, this.LeftOperand, this.RightOperand, this.Compilation.BuiltinMethods);
    }

    private IEnumerable<IMethodDefinition> BaseStandardOperators
    {
      get { return base.StandardOperators; }
    }

    protected override IEnumerable<IMethodDefinition> StandardOperators
    {
      get
      {
        BuiltinMethods dummyMethods = this.Compilation.BuiltinMethods;
        IEnumerable<IMethodDefinition> baseStandardOperators = this.BaseStandardOperators;
        foreach (IMethodDefinition method in baseStandardOperators)
        {
          if (method != dummyMethods.BoolOpBool) yield return method;
        }
      }
    }
  }

  public class VccConditional : Conditional {
    /// <summary>
    /// Allocates an expression that results in one of two values, depending on the value of a condition.
    /// </summary>
    /// <param name="condition">The condition that determines which subexpression to evaluate.</param>
    /// <param name="resultIfTrue">The expression to evaluate as the value of the overall expression if the condition is true.</param>
    /// <param name="resultIfFalse">The expression to evaluate as the value of the overall expression if the condition is false.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccConditional(Expression condition, Expression resultIfTrue, Expression resultIfFalse, ISourceLocation sourceLocation)
      : base(condition, resultIfTrue, resultIfFalse, sourceLocation) {
    }

    public override ITypeDefinition InferType()
    {
      ITypeDefinition t = base.InferType();
      if (t != Dummy.Type) return t;

      ITypeDefinition leftType = this.ResultIfTrue.Type;
      ITypeDefinition rightType = this.ResultIfFalse.Type;

      if (this.Helper.ImplicitConversionExists(this.ResultIfFalse, leftType) &&
          this.Helper.ImplicitConversionExists(this.ResultIfTrue, rightType) &&
          leftType is PointerType && rightType is PointerType) {
        if (((PointerType)leftType).TargetType.TypeCode == PrimitiveTypeCode.Void)
          return rightType;
        if (((PointerType)rightType).TargetType.TypeCode == PrimitiveTypeCode.Void)
          return leftType;
      }

      return Dummy.Type;
    }

    public override bool ValueIsPolymorphicCompileTimeConstant {
      get {
        return this.ResultIfTrue.ValueIsPolymorphicCompileTimeConstant && this.ResultIfFalse.ValueIsPolymorphicCompileTimeConstant;
      }
    }
  }

  public class VccLogicalOr : LogicalOr {
    /// <summary>
    /// Allocates an expression that results in true if both operands result in true. If the left operand results in false, the right operand is not evaluated.
    /// When overloaded, this expression corresponds to calls to op_False and op_BitwiseAnd.
    /// </summary>
    /// <param name="leftOperand">The left operand.</param>
    /// <param name="rightOperand">The right operand.</param>
    /// <param name="sourceLocation">The source location of the operation.</param>
    public VccLogicalOr(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccLogicalOr(BlockStatement containingBlock, VccLogicalOr template)
      : base(containingBlock, template)
    //^ requires template.ContainingBlock != containingBlock;
    //^ ensures this.containingBlock == containingBlock;
    {
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      this.WarnForSuspiciousEquality(this.LeftOperand);
      this.WarnForSuspiciousEquality(this.RightOperand);
      return base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

    private void WarnForSuspiciousEquality(Expression expression)
    {
      VccEquality equality = expression as VccEquality;
      if (equality != null) {
        if (equality.LeftOperand.Type.TypeCode == PrimitiveTypeCode.Boolean ||
            equality.RightOperand.Type.TypeCode == PrimitiveTypeCode.Boolean) {
          this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.PotentialPrecedenceErrorInLogicalExpression));
        }
      }
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccLogicalOr(containingBlock, this);
    }
  }

  public class VccLogicalAnd : LogicalAnd {
    /// <summary>
    /// Allocates an expression that results in true if both operands result in true. If the left operand results in false, the right operand is not evaluated.
    /// When overloaded, this expression corresponds to calls to op_False and op_BitwiseAnd.
    /// </summary>
    /// <param name="leftOperand">The left operand.</param>
    /// <param name="rightOperand">The right operand.</param>
    /// <param name="sourceLocation">The source location of the operation.</param>
    public VccLogicalAnd(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccLogicalAnd(BlockStatement containingBlock, VccLogicalAnd template)
      : base(containingBlock, template)
    //^ requires template.ContainingBlock != containingBlock;
    //^ ensures this.containingBlock == containingBlock;
    {
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      this.WarnForSuspiciousEquality(this.LeftOperand);
      this.WarnForSuspiciousEquality(this.RightOperand);
      return base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

    private void WarnForSuspiciousEquality(Expression expression)
    {
      VccEquality equality = expression as VccEquality;
      if (equality != null) {
        if (equality.LeftOperand.Type.TypeCode == PrimitiveTypeCode.Boolean ||
            equality.RightOperand.Type.TypeCode == PrimitiveTypeCode.Boolean) {
          this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.PotentialPrecedenceErrorInLogicalExpression));
        }
      }
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccLogicalAnd(containingBlock, this);
    }
  }

  /// <summary>
  /// An expression that results in true if both operands represent the same value or object. When overloaded, this expression corresponds to a call to op_Equality.
  /// </summary>
  public class VccEquality : Equality {

    /// <summary>
    /// Allocates an expression that results in true if both operands represent the same value or object. When overloaded, this expression corresponds to a call to op_Equality.
    /// </summary>
    public VccEquality(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccEquality(BlockStatement containingBlock, VccEquality template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Computes the compile time value of the expression. Can be null.
    /// </summary>
    protected override object/*?*/ GetValue() {
      object/*?*/ obj = base.GetValue();
      if (obj is System.Boolean) {
        return (bool)obj ? 1 : 0;
      }
      return obj;
    }

    /// <summary>
    /// Infers the type of value that this expression will evaluate to. At runtime the actual value may be an instance of subclass of the result of this method.
    /// Calling this method does not cache the computed value and does not generate any error messages. In some cases, such as references to the parameters of lambda
    /// expressions during type overload resolution, the value returned by this method may be different from one call to the next.
    /// When type inference fails, Dummy.Type is returned.
    /// </summary>
    /// <remarks>This override allows StandardOperators to use the same dummy methods as the arithmetic operations.</remarks>
    public override ITypeDefinition InferType() {
      return this.PlatformType.SystemInt32.ResolvedType;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccEquality(containingBlock, this);
    }

    /// <summary>
    /// A list of dummy methods that correspond to operations that are built into IL. The dummy methods are used, via overload resolution,
    /// to determine how the operands are to be converted before the operation is carried out.
    /// </summary>
    protected override IEnumerable<IMethodDefinition> StandardOperators {
      get {
        BuiltinMethods dummyMethods = this.Compilation.BuiltinMethods;
        yield return dummyMethods.Int32opInt32;
        yield return dummyMethods.UInt32opUInt32;
        yield return dummyMethods.Int64opInt64;
        yield return dummyMethods.UInt64opUInt64;
        yield return dummyMethods.Float32opFloat32;
        yield return dummyMethods.Float64opFloat64;
        yield return dummyMethods.DecimalOpDecimal;
        yield return dummyMethods.UIntPtrOpUIntPtr;
        yield return dummyMethods.VoidPtrOpVoidPtr;
        ITypeDefinition leftOperandType = this.LeftOperand.Type;
        ITypeDefinition rightOperandType = this.RightOperand.Type;
        if (leftOperandType.IsEnum)
          yield return dummyMethods.GetDummyEnumOpEnum(leftOperandType);
        else if (rightOperandType.IsEnum)
          yield return dummyMethods.GetDummyEnumOpEnum(rightOperandType);
        if (VccEquality.IsMathOrRecordTypeComparison(leftOperandType, rightOperandType) ||
            !(leftOperandType.IsValueType || rightOperandType.IsValueType))          
          yield return dummyMethods.ObjectOpObject;
      }
    }

    static internal bool IsMathOrRecordTypeComparison(ITypeDefinition t1, ITypeDefinition t2)
    {
      INamespaceTypeDefinition ltype = t1 as INamespaceTypeDefinition;
      INamespaceTypeDefinition rtype = t2 as INamespaceTypeDefinition;
      if (ltype != null && rtype != null &&
          TypeHelper.TypesAreEquivalent(ltype, rtype) &&
          ltype.Name.Value.StartsWith("_vcc_math_type", StringComparison.Ordinal))
        return true;

      if (ltype != null) {
        foreach (ICustomAttribute attr in ltype.Attributes) {
          if (TypeHelper.GetTypeName(attr.Type) == "Microsoft.Contracts.StringVccAttr") {
            List<IMetadataExpression> args = new List<IMetadataExpression>(attr.Arguments);
            if (args.Count == 2) {
              IMetadataConstant attrName = args[0] as IMetadataConstant;
              IMetadataConstant attrVal = args[1] as IMetadataConstant;
              if (attrName != null && attrVal != null && ((string)attrName.Value) == "record" && ((string)attrVal.Value) == "true")
                return true;
            }
          }
        }
      }

      return false;
    }

    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      if (this.LeftOperand.HasErrors() || this.RightOperand.HasErrors()) return true;
      if (VccEquality.IsMathOrRecordTypeComparison(this.LeftOperand.Type, this.RightOperand.Type))
        return false;

      return base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

  }

  public class VccIfAndOnlyIf : VccEquality {
    /// <summary>
    /// Allocates an expression that results in true if both operands represent the same value or object. When overloaded, this expression corresponds to a call to op_Equality.
    /// </summary>
    public VccIfAndOnlyIf(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccIfAndOnlyIf(BlockStatement containingBlock, VccIfAndOnlyIf template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Computes the compile time value of the expression. Can be null.
    /// </summary>
    protected override object/*?*/ GetValue()
    {
      object/*?*/ obj = base.GetValue();
      if (obj is System.Int32) {
        return ((int)obj) == 0 ? false : true;
      }
      return obj;
    }

    /// <summary>
    /// Infers the type of value that this expression will evaluate to. At runtime the actual value may be an instance of subclass of the result of this method.
    /// Calling this method does not cache the computed value and does not generate any error messages. In some cases, such as references to the parameters of lambda
    /// expressions during type overload resolution, the value returned by this method may be different from one call to the next.
    /// When type inference fails, Dummy.Type is returned.
    /// </summary>
    /// <remarks>This override allows StandardOperators to use the same dummy methods as the arithmetic operations.</remarks>
    public override ITypeDefinition InferType()
    {
      return this.PlatformType.SystemBoolean.ResolvedType;
    }

    protected override IEnumerable<IMethodDefinition> StandardOperators
    {
      get
      {
        yield return this.Compilation.BuiltinMethods.BoolOpBool;
      }
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccIfAndOnlyIf(containingBlock, this);
    }
  }

  public class VccNotEquality : NotEquality {
    /// <summary>
    /// Allocates an expression that results in true if both operands represent the same value or object. When overloaded, this expression corresponds to a call to op_Equality.
    /// </summary>
    public VccNotEquality(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccNotEquality(BlockStatement containingBlock, VccNotEquality template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Computes the compile time value of the expression. Can be null.
    /// </summary>
    protected override object/*?*/ GetValue() {
      object/*?*/ obj = base.GetValue();
      if (obj is System.Boolean) {
        return (bool)obj ? 1 : 0;
      }
      return obj;
    }

    /// <summary>
    /// Infers the type of value that this expression will evaluate to. At runtime the actual value may be an instance of subclass of the result of this method.
    /// Calling this method does not cache the computed value and does not generate any error messages. In some cases, such as references to the parameters of lambda
    /// expressions during type overload resolution, the value returned by this method may be different from one call to the next.
    /// When type inference fails, Dummy.Type is returned.
    /// </summary>
    /// <remarks>This override allows StandardOperators to use the same dummy methods as the arithmetic operations.</remarks>
    public override ITypeDefinition InferType() {
      return this.PlatformType.SystemInt32.ResolvedType;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccNotEquality(containingBlock, this);
    }

    /// <summary>
    /// A list of dummy methods that correspond to operations that are built into IL. The dummy methods are used, via overload resolution,
    /// to determine how the operands are to be converted before the operation is carried out.
    /// </summary>
    protected override IEnumerable<IMethodDefinition> StandardOperators {
      get {
        BuiltinMethods dummyMethods = this.Compilation.BuiltinMethods;
        yield return dummyMethods.Int32opInt32;
        yield return dummyMethods.UInt32opUInt32;
        yield return dummyMethods.Int64opInt64;
        yield return dummyMethods.UInt64opUInt64;
        yield return dummyMethods.Float32opFloat32;
        yield return dummyMethods.Float64opFloat64;
        yield return dummyMethods.DecimalOpDecimal;
        yield return dummyMethods.UIntPtrOpUIntPtr;
        yield return dummyMethods.VoidPtrOpVoidPtr;
        ITypeDefinition leftOperandType = this.LeftOperand.Type;
        ITypeDefinition rightOperandType = this.RightOperand.Type;
        if (leftOperandType.IsEnum)
          yield return dummyMethods.GetDummyEnumOpEnum(leftOperandType);
        else if (rightOperandType.IsEnum)
          yield return dummyMethods.GetDummyEnumOpEnum(rightOperandType);
        if (VccEquality.IsMathOrRecordTypeComparison(leftOperandType, rightOperandType) ||
            !(leftOperandType.IsValueType || rightOperandType.IsValueType))
          yield return dummyMethods.ObjectOpObject;
      }
    }


    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      if (this.LeftOperand.HasErrors() || this.RightOperand.HasErrors()) return true;

      if (VccEquality.IsMathOrRecordTypeComparison(this.LeftOperand.Type, this.RightOperand.Type))
        return false;

      return base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }
  }

  public class VccFunctionTypeExpression : TypeExpression {

    public VccFunctionTypeExpression(bool acceptsExtraArguments, CallingConvention callingConvention, TypeExpression returnType, NameDeclaration name,
      List<ParameterDeclaration> parameters, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.AcceptsExtraArguments = acceptsExtraArguments;
      this.CallingConvention = callingConvention;  
      this.ReturnType = returnType;
      this.Name = name;
      this.parameters = parameters;
    }

    internal VccFunctionTypeExpression(bool acceptsExtraArguments, CallingConvention callingConvention, TypeExpression returnType, NameDeclaration name,
      List<ParameterDeclaration> parameters, FunctionDeclarator declarator, ISourceLocation sourceLocation)
      : this(acceptsExtraArguments, callingConvention, returnType, name, parameters, sourceLocation) {
      this.declarator = declarator;
    }

    private VccFunctionTypeExpression(BlockStatement containingBlock, VccFunctionTypeExpression template)
      : base(containingBlock, template) {
      this.AcceptsExtraArguments = template.AcceptsExtraArguments;
      this.CallingConvention = template.CallingConvention;
      this.ReturnType = (TypeExpression)template.ReturnType.MakeCopyFor(containingBlock);
      this.Name = template.Name.MakeCopyFor(containingBlock.Compilation);
      this.parameters = new List<ParameterDeclaration>(template.parameters);
      this.declarator = template.declarator;
    }

    public readonly bool AcceptsExtraArguments;

    public readonly CallingConvention CallingConvention;

    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      return this.ReturnType.HasErrors();
    }

    readonly internal FunctionDeclarator/*?*/ declarator;

    protected override ITypeDefinition Resolve() {
      //^ assume false;
      return Dummy.Type;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccFunctionTypeExpression(containingBlock, this);
    }

    public readonly NameDeclaration Name;

    public IEnumerable<ParameterDeclaration> Parameters {
      get {
        if (parameters.Count == 1 && parameters[0].Type.ResolvedType.TypeCode == PrimitiveTypeCode.Void)
          parameters.Clear();
        return parameters.AsReadOnly(); 
      }
    }
    readonly internal List<ParameterDeclaration> parameters;

    public readonly TypeExpression ReturnType;

    public override void SetContainingExpression(Expression containingExpression) {
      base.SetContainingExpression(containingExpression);
      this.ReturnType.SetContainingExpression(containingExpression);
    }
  }

  /// <summary>
  /// An expression that represents an access to an array element or string character.
  /// </summary>
  public sealed class VccIndexer : Indexer {
    /// <summary>
    /// Allocates an expression that represents a call to the getter or setter of a default indexed property, or an access to an array element or string character.
    /// </summary>
    /// <param name="indexedObject">An expression that results in value whose type is expected to be an array, or string, or to define a default indexed property that matches the indices.</param>
    /// <param name="indexes">The indices to pass to the accessor.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccIndexer(Expression indexedObject, IEnumerable<Expression> indexes, ISourceLocation sourceLocation)
      : base(indexedObject, indexes, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    private VccIndexer(BlockStatement containingBlock, VccIndexer template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    protected override Indexer CreateNewIndexerForFactoring(Expression indexedObject, IEnumerable<Expression> indices, ISourceLocation sourceLocation)
    {
      return new VccIndexer(indexedObject, indices, sourceLocation);
    }

    ITypeDefinition/*?*/ FixedArrayElementType {
      get {
        ITypeDefinition indexedObjectType = this.IndexedObject.Type;
        if (/*indexedObjectType.IsSpecialName &&*/ indexedObjectType.IsStruct && indexedObjectType.SizeOf > 0) {
          IFieldDefinition/*?*/ field = TypeHelper.GetField(indexedObjectType, this.Helper.NameTable.GetNameFor("_ElementType"));
          if (field != null) return field.Type.ResolvedType;
        }
        return null;
      }
    }

    protected override void ComplainAboutCallee()
    {
      bool reportedError = false;
      IGenericTypeInstance genericType = this.IndexedObject.Type.ResolvedType as IGenericTypeInstance;
      if (genericType != null && TypeHelper.GetTypeName(genericType).StartsWith(NamespaceHelper.SystemDiagnosticsContractsCodeContractString + ".Map", StringComparison.Ordinal))
      {
        List<Expression> arguments = new List<Expression>(this.OriginalArguments);
        List<ITypeReference> expectedTypes = new List<ITypeReference>(genericType.GenericArguments);
        if (arguments.Count == 1 && expectedTypes.Count == 2)
        {
          Helper.ReportFailedImplicitConversion(arguments[0], expectedTypes[0].ResolvedType);
          reportedError = true;
        }
      }
      if (!reportedError) this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.UnknownElementSize, this.IndexedObject.SourceLocation.Source));
    }

    /// <summary>
    /// Returns a collection of methods that match the name of the method/indexer to call, or that represent the
    /// collection of constructors for the named type.
    /// </summary>
    /// <param name="allowMethodParameterInferencesToFail">If this flag is true, 
    /// generic methods should be included in the collection if their method parameter types could not be inferred from the argument types.</param>
    protected override IEnumerable<IMethodDefinition> GetCandidateMethods(bool allowMethodParameterInferencesToFail) {
      if (this.FixedArrayElementType != null) return this.GetPointerAdditionMethods(FixedArrayElementType);
      return base.GetCandidateMethods(allowMethodParameterInferencesToFail);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccIndexer(containingBlock, this);
    }

    /// <summary>
    /// Returns an object that implements IExpression and that represents this expression after language specific rules have been
    /// applied to it in order to determine its semantics. The resulting expression is a standard representation of the semantics
    /// of this expression, suitable for use by language agnostic clients and complete enough for translation of the expression
    /// into IL.
    /// </summary>
    protected override IExpression ProjectAsNonConstantIExpression() {
      if (this.FixedArrayElementType != null)
        return this.ProjectAsDereferencedConvertedPointerAddition();
      return base.ProjectAsNonConstantIExpression();
    }

    /// <summary>
    /// Returns an expression corresponding to *(ptr + index) where ptr is this.IndexedObject and index is the first element of this.ConvertedArguments.
    /// </summary>
    private IExpression ProjectAsDereferencedConvertedPointerAddition() {
      //transform to *(ptr + index)
      
      // !!! DEPENDENCY WARNING !!!
      //
      // if you ever change the way that a VccIndexer is projected, you will need to fix
      // 
      // ConvertFelt2Boogie.Visit(IAddressDereference addressDerefence)
      //
      // which relies on the exact structure of the generated projected expression
      //
      // !!! DEPENDENCY WARNING !!!

      IEnumerator<Expression> indexEnumerator = this.ConvertedArguments.GetEnumerator();
      if (!indexEnumerator.MoveNext()) return CodeDummy.Expression;
      Expression ptr = this.IndexedObject;
      if (this.FixedArrayElementType != null) {
        ptr = new VccAddressOf(new VccAddressableExpression(this.IndexedObject, true), this.IndexedObject.SourceLocation);
        ptr.SetContainingExpression(this);
        ITypeDefinition voidPointer = PointerType.GetPointerType(this.Compilation.PlatformType.SystemVoid, this.Compilation.HostEnvironment.InternFactory);
        ptr = this.Helper.ExplicitConversion(ptr, voidPointer);
        ITypeDefinition pointerType = PointerType.GetPointerType(this.FixedArrayElementType, this.Compilation.HostEnvironment.InternFactory);
        ptr = this.Helper.ExplicitConversion(ptr, pointerType);
      }
      Expression index = indexEnumerator.Current;
      if (index.Type.IsEnum) index = this.Helper.ExplicitConversion(index, index.Type.UnderlyingType.ResolvedType);
      Addition addition = new Addition(ptr, index, this.SourceLocation);
      AddressDereference aderef = new AddressDereference(addition, this.SourceLocation);
      aderef.SetContainingExpression(this);
      return aderef.ProjectAsIExpression();
    }
 
    /// <summary>
    /// Results in null or an array indexer or an indexer (property) definition.
    /// </summary>
    public override object/*?*/ ResolveAsValueContainer()
      //^ ensures result == null || result is IArrayIndexer || result is IAddressDereference || result is IPropertyDefinition;
    {
      if (this.FixedArrayElementType != null)
        return this.ProjectAsDereferencedConvertedPointerAddition() as IAddressDereference;
      return base.ResolveAsValueContainer();
    }
    
  }

  internal class VccDesignatorExpressionPair
  {
    internal readonly SimpleName Designator;
    internal readonly Expression Expression;

    internal VccDesignatorExpressionPair(SimpleName designator, Expression expression) {
      this.Designator = designator;
      this.Expression = expression;
    }

    internal VccDesignatorExpressionPair MakeCopyFor(BlockStatement containingBlock) {
      return new VccDesignatorExpressionPair((SimpleName)this.Designator.MakeCopyFor(containingBlock), this.Expression.MakeCopyFor(containingBlock));
    }
  }

  internal class VccInitializerWithDefault : VccInitializerWithDesignators
  {
    private readonly Expression defaultExpression;

    public VccInitializerWithDefault(Expression defaultExpression, VccInitializerWithDesignators initializer, ISourceLocation sourceLocation)
      : base(initializer.DesignatorsWithExpressions, sourceLocation)
    {
      this.defaultExpression = defaultExpression;
    }

    protected VccInitializerWithDefault(BlockStatement containingBlock, VccInitializerWithDefault template) 
      : base(containingBlock, template)
    {
      this.defaultExpression = template.defaultExpression.MakeCopyFor(containingBlock);
    }

    public override ITypeDefinition Type {
      get {
        return this.defaultExpression.Type;
      }
    }

    protected override bool IsOfStructuredType {
      get { return true; }
    }

    internal override VccStructuredTypeDeclaration GetStructuredTypeDecl() {
      return GetStructuredTypeDeclFor(this.Type.ResolvedType);
    }

    public override void SetContainingExpression(Expression containingExpression) {
      base.SetContainingExpression(containingExpression);
      this.defaultExpression.SetContainingExpression(this);
    }

    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccInitializerWithDefault(containingBlock, this);
    }

    protected override LocalDefinition CreateLocalTempForProjection(List<Statement> statements) {
      return Expression.CreateInitializedLocalDeclarationAndAddDeclarationsStatementToList(this.defaultExpression, statements);
    }
  }

  internal class VccInitializerWithDesignators : VccInitializerBase
  {
    protected readonly List<VccDesignatorExpressionPair> designatorsWithExpressions;

    internal List<VccDesignatorExpressionPair> DesignatorsWithExpressions {
      get { return this.designatorsWithExpressions; }
    }

    public VccInitializerWithDesignators(List<VccDesignatorExpressionPair> designatorsWithExpressions, ISourceLocation sourceLocation) 
      : base(sourceLocation)
    {
      this.designatorsWithExpressions = designatorsWithExpressions;
    }

    protected VccInitializerWithDesignators(BlockStatement containingBlock, VccInitializerWithDesignators template) 
      : base(containingBlock, template)
    {
      this.designatorsWithExpressions = new List<VccDesignatorExpressionPair>(template.designatorsWithExpressions.Count);
      foreach (var pair in template.designatorsWithExpressions)
        this.designatorsWithExpressions.Add(pair.MakeCopyFor(containingBlock));
    }

    override internal int ExpressionCount {
      get { return this.designatorsWithExpressions.Count; }
    }

    public override void SetContainingExpression(Expression containingExpression) {
      base.SetContainingExpression(containingExpression);
      foreach (var pair in this.designatorsWithExpressions) {
        pair.Designator.SetContainingExpression(this);
        pair.Expression.SetContainingExpression(this);
      }
    }

    internal override void AddInitializingElementAssignmentsTo(ICollection<Statement> statements, Expression target, VccArrayTypeExpression arrTypeExp) {
      throw new InvalidOperationException("Cannot use designator list to initialize an array");
    }

    internal override void AddInitializingFieldAssignmentsTo(ICollection<Statement> statements, Expression target, VccStructuredTypeDeclaration typeDecl) {
      foreach (var pair in this.designatorsWithExpressions) {
        QualifiedName targetDotField = new QualifiedName(target, pair.Designator, pair.Designator.SourceLocation);
        AddInitializationTo(statements, pair.Expression, targetDotField, GetTypeOfField(typeDecl, pair.Designator), this.ContainingBlock);
      }
    }

    private static TypeExpression GetTypeOfField(VccStructuredTypeDeclaration typeDecl, SimpleName fieldName) {
      foreach (FieldDefinition fd in IteratorHelper.GetFilterEnumerable<ITypeDeclarationMember, FieldDefinition>(typeDecl.TypeDeclarationMembers))
        if (fd.Name.UniqueKey == fieldName.Name.UniqueKey) return fd.Type;
      return null;
    }

    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccInitializerWithDesignators(containingBlock, this);
    }
  }

  public abstract class VccInitializerBase : Expression
  {
    protected VccInitializerBase(ISourceLocation sourceLocation)
      : base(sourceLocation) {
    }

    protected VccInitializerBase(BlockStatement containingBlock, VccInitializerBase template) 
      : base(containingBlock, template) {
    }

    private static VccStructuredTypeDeclaration/*?*/ MiniResolve(NamespaceDeclaration nsDeclaration, VccNamedTypeExpression/*?*/ typeExp) {
      if (nsDeclaration == null) return null;
      if (typeExp == null) return null;
      SimpleName/*?*/ typeName = typeExp.Expression as SimpleName;
      if (typeName == null) return null;
      int typeNameUniqueKey = typeName.Name.UniqueKey;
      foreach (VccStructuredTypeDeclaration typeDecl in
        IteratorHelper.GetFilterEnumerable<INamespaceDeclarationMember, VccStructuredTypeDeclaration>(nsDeclaration.Members)) {
        if (typeDecl.Name.UniqueKey == typeNameUniqueKey) return typeDecl;
      }
      return null;
    }

    protected static void AddInitializationTo(ICollection<Statement> statements, Expression source, Expression target, TypeExpression targetType, BlockStatement containingBlock) {
      VccInitializer initializer = source as VccInitializer;
      if (initializer != null) {
        VccArrayTypeExpression arrayType = initializer.arrayTypeExpression;
        if (arrayType == null) arrayType = targetType as VccArrayTypeExpression;
        if (arrayType != null) {
          initializer.AddInitializingElementAssignmentsTo(statements, target, arrayType);
        } else if (initializer.IsOfStructuredType) {
          VccStructuredTypeDeclaration structType = initializer.GetStructuredTypeDecl();
          if (structType != null) initializer.AddInitializingFieldAssignmentsTo(statements, target, structType);
        }
      } else {
        // It is not an initializer
        // If the expression is a string and the target is a char array, in which case we treat it as an array initializer.
        VccByteStringLiteral stringLiteral = source as VccByteStringLiteral;
        VccArrayTypeExpression arrayType = targetType as VccArrayTypeExpression;
        if (stringLiteral != null && arrayType != null) {
          string val = stringLiteral.Value as string;
          if (val != null) {
            if (arrayType.Size == null) {
              CompileTimeConstant ctc = new CompileTimeConstant(val.Length + 1, stringLiteral.SourceLocation);
              ctc.SetContainingExpression(stringLiteral);
              arrayType.ResetSizeWhenProvidedByInitializer(ctc);
            }
            int size = arrayType.SizeAsInt32;
            VccInitializer newInitializer = VccInitializer.fromStringWithPatchedZeros(val, size, stringLiteral);
            // No need to assign the array type expression field, because we know the element type is char.
            if (newInitializer != null) {
              newInitializer.AddInitializingElementAssignmentsTo(statements, target, arrayType);
            }
          }
        } else {
          // If the target is a union, we will try to treat the constant as an initializer.
          CompileTimeConstant ctc = source as CompileTimeConstant;
          VccUnionDeclaration unionType = MiniResolve(containingBlock.ContainingNamespaceDeclaration, targetType as VccNamedTypeExpression) as VccUnionDeclaration;
          if (ctc != null && unionType != null) {
            List<Expression> exprs = new List<Expression>();
            exprs.Add(ctc);
            VccInitializer newInitializer = new VccInitializer(exprs, source.SourceLocation);
            newInitializer.SetContainingBlock(containingBlock);
            newInitializer.AddInitializingFieldAssignmentsTo(statements, target, unionType);
          } else {
            // otherwise, generate an assignment.
            ExpressionStatement elementAssignment = new ExpressionStatement(new Assignment(new TargetExpression(target), source, source.SourceLocation));
            elementAssignment.SetContainingBlock(containingBlock);
            statements.Add(elementAssignment);
          }
        }
      }
    }

    protected virtual LocalDefinition CreateLocalTempForProjection(List<Statement> statements) {
      IName dummyName = this.ContainingBlock.Helper.NameTable.GetNameFor("__temp" + this.SourceLocation.StartIndex);
      NameDeclaration tempName = new NameDeclaration(dummyName, this.SourceLocation);
      LocalDeclaration temp = new LocalDeclaration(false, false, tempName, null, this.SourceLocation);
      List<LocalDeclaration> declarations = new List<LocalDeclaration>(1);
      declarations.Add(temp);
      LocalDeclarationsStatement statement = new LocalDeclarationsStatement(false, false, false, TypeExpression.For(this.structureTypeExpression.ResolvedType), declarations, this.SourceLocation);
      statements.Add(statement);
      statement.SetContainingBlock(this.ContainingBlock);
      temp.SetContainingLocalDeclarationsStatement(statement);
      return temp.LocalVariable;
    }

    /// <summary>
    /// Returns an object that implements IExpression and that represents this expression after language specific rules have been
    /// applied to it in order to determine its semantics. The resulting expression is a standard representation of the semantics
    /// of this expression, suitable for use by language agnostic clients and complete enough for translation of the expression
    /// into IL.
    /// </summary>
    protected override IExpression ProjectAsNonConstantIExpression() {
      if (cachedProjection != null) return cachedProjection;
      if (this.arrayTypeExpression == null && !this.IsOfStructuredType) return (cachedProjection = CodeDummy.Expression);
      
      // create the value in a local and initialize its fields field-by-field
      List<Statement> statements = new List<Statement>();
      LocalDefinition localTemp;
      Expression result;
      if (this.arrayTypeExpression != null) {
        CreateStackArray createStackArray = new CreateStackArray(this.arrayTypeExpression.ElementType, new CompileTimeConstant(this.ExpressionCount, this.SourceLocation), this.SourceLocation);
        createStackArray.SetContainingExpression(this);
        localTemp = Expression.CreateInitializedLocalDeclarationAndAddDeclarationsStatementToList(createStackArray, statements);
        result = new BoundExpression(this, localTemp);
        this.AddInitializingElementAssignmentsTo(statements, result, this.arrayTypeExpression);
      } else {
        localTemp = this.CreateLocalTempForProjection(statements);
        result = new BoundExpression(this, localTemp);
        VccStructuredTypeDeclaration typeDecl = this.GetStructuredTypeDecl();
        if (typeDecl != null)
          this.AddInitializingFieldAssignmentsTo(statements, result, typeDecl);
      }
      BlockStatement block = new BlockStatement(statements, this.SourceLocation);
      BlockExpression bexpr = new BlockExpression(block, result, this.SourceLocation);
      bexpr.SetContainingExpression(this);
      return this.cachedProjection = bexpr.ProjectAsIExpression();
    }

    IExpression cachedProjection = null;

    internal abstract int ExpressionCount { get; }

    internal abstract void AddInitializingElementAssignmentsTo(ICollection<Statement> statements, Expression target, VccArrayTypeExpression/*?*/ arrTypeExp);
    internal abstract void AddInitializingFieldAssignmentsTo(ICollection<Statement> statements, Expression target, VccStructuredTypeDeclaration typeDecl);

    protected virtual bool IsOfStructuredType {
      get { return this.structureTypeExpression != null; }
    }

    internal static VccStructuredTypeDeclaration GetStructuredTypeDeclFor(ITypeDefinition type) {
      if (type == null) return null;
      TypeDefinition td = type as TypeDefinition;
      if (td != null && td.TypeDeclarations != null) {
        foreach (TypeDeclaration tdecl in td.TypeDeclarations) {
          VccStructuredTypeDeclaration/*?*/  typeDecl = tdecl as VccStructuredTypeDeclaration;
          if (typeDecl != null) return typeDecl;
        }
      }
      return null;
    }

    internal virtual VccStructuredTypeDeclaration GetStructuredTypeDecl() {
      if (this.structureTypeExpression == null) return null;
      return GetStructuredTypeDeclFor(this.structureTypeExpression.ResolvedType);
    }

    /// <summary>
    /// The type of value that the expression will evaluate to, as determined at compile time.
    /// </summary>
    public override ITypeDefinition Type {
      get {
        if (this.type == null) {
          ITypeDefinition type;
          if (this.structureTypeExpression != null)
            type = this.structureTypeExpression.ResolvedType;
          else if (this.arrayTypeExpression != null) {
            type = PointerType.GetPointerType(this.arrayTypeExpression.ElementType.ResolvedType, this.Compilation.HostEnvironment.InternFactory);
          } else type = Dummy.Type;
          this.type = type;
        }
        return this.type;
      }
    }

    //^ [Once]
    private ITypeDefinition/*?*/ type;
    internal VccArrayTypeExpression/*?*/ arrayTypeExpression;
    internal VccNamedTypeExpression/*?*/ structureTypeExpression;
  }

  /// <summary>
  /// An expression that represents the initial value of an array, structure or union.
  /// </summary>
  public class VccInitializer : VccInitializerBase
  {

    public VccInitializer(List<Expression> expressions, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.expressions = expressions;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    private VccInitializer(BlockStatement containingBlock, VccInitializer template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
      this.expressions = new List<Expression>(template.expressions.Count);
      foreach (var expr in template.expressions)
        this.expressions.Add(expr.MakeCopyFor(containingBlock));
    }

    internal override void AddInitializingElementAssignmentsTo(ICollection<Statement> statements, Expression array, VccArrayTypeExpression/*?*/ arrTypeExp) {
      TypeExpression elemTypeExp = null;
      if (arrTypeExp != null) {
        elemTypeExp = arrTypeExp.ElementType;
      }
      // If we have a multiple dimensional array, and the element is not a VccInitializer,
      // then we will compute the number of constants needed for array[i], say, x, and bundle 
      // x constant together for array[i], which may be further an array, in which case, the above
      // process repeat. 
      int i = 0, n = this.expressions.Count;
      int lengthForFirstDimensionIfNotProvided = 0;
      bool elementIsVccInitializer = (n == 0) ? true : expressions[0] is VccInitializer;
      int rownum = 0; 
      // For multidimensional array,
      // i may be increased by the total number of embedded array elements per loop, rownum always by 1. 
      while (i < n){
        Expression arrayLine = array;  
        List<Expression> indices = new List<Expression>(1);
        indices.Add(new CompileTimeConstant(rownum++, true, SourceDummy.SourceLocation));
        VccIndexer element = new VccIndexer(arrayLine, indices.AsReadOnly(), SourceDummy.SourceLocation);
        // Construct the initial value for one element of the array. 
        Expression/*?*/ initialValueForOneElement = null;
        int sizeOfEmbeddedArrays = arrTypeExp.SizeOfEmbeddedArrays;
        VccArrayTypeExpression embeddedArrayType = elemTypeExp as VccArrayTypeExpression;
        //^ assert sizeOfEmbeddedArrays >=0;  
        // TODO: C doesnt allow sizeOfEmbeddedArrays to be zero, in which case we should report an error
        if (sizeOfEmbeddedArrays ==0 || elementIsVccInitializer || embeddedArrayType == null) {
          initialValueForOneElement = this.expressions[i++];
        } else {
          List<Expression> exprs = new List<Expression>(sizeOfEmbeddedArrays);
          for (int j = 0; j < sizeOfEmbeddedArrays; j++) {
            if (i < n) {
              exprs.Add(expressions[i++]);
            } else { // Patching Zeros, which in general are not really needed. 
              exprs.Add(new CompileTimeConstant(0, this.SourceLocation));
            }
          }
          initialValueForOneElement = new VccInitializer(exprs, this.SourceLocation);
        }
        //^ assert initialValueForOneElement != null;
        AddInitializationTo(statements, initialValueForOneElement, element, elemTypeExp, this.ContainingBlock);
        lengthForFirstDimensionIfNotProvided++; 
      }
      // In C, it is possible to initialize an array when its first dimension's length is not
      // specified, in which case, the initializer provides information of the length.
      if (arrTypeExp.Size != null) {
        arrTypeExp.ResetSizeWhenProvidedByInitializer(new CompileTimeConstant(lengthForFirstDimensionIfNotProvided, arrTypeExp.SourceLocation));
      }
    }

    /// <summary>
    /// Supply initialization code for a structured variable in the static initializer.
    /// </summary>
    /// <param name="statements">statements collection into which new statements are added</param>
    /// <param name="target">the name of the struct variable</param>
    /// <param name="typeDecl">the structured type's declaration</param>
    internal override void AddInitializingFieldAssignmentsTo(ICollection<Statement> statements, Expression target, VccStructuredTypeDeclaration typeDecl) {
      bool isUnion = typeDecl is VccUnionDeclaration;
      IEnumerator<Expression> exprEnumerator= this.expressions.GetEnumerator();
      foreach (FieldDefinition fd in IteratorHelper.GetFilterEnumerable<ITypeDeclarationMember, FieldDefinition>(typeDecl.TypeDeclarationMembers)) {
        SimpleName fieldName = new SimpleName(fd.Name, target.SourceLocation, false);
        QualifiedName varDotField = new QualifiedName(target, fieldName, target.SourceLocation);
        if (exprEnumerator.MoveNext())
          AddInitializationTo(statements, exprEnumerator.Current, varDotField, fd.Type, this.ContainingBlock);
        if (isUnion) return;
      }
    }

    /// <summary>
    /// Returns a byte array representing the part of the process image to which this field will be mapped. Can be null.
    /// </summary>
    internal byte[]/*?*/ GetMappedData() {
      List<byte> bytes = new List<byte>();
      foreach (Expression e in this.Expressions) {
        object/*?*/ val = e.Value;
        IConvertible/*?*/ ic = val as IConvertible;
        if (ic == null) return null;
        switch (ic.GetTypeCode()) {
          case TypeCode.Boolean:
            bytes.AddRange(BitConverter.GetBytes(ic.ToBoolean(null)));
            break;
          case TypeCode.Byte:
            bytes.Add(ic.ToByte(null));
            break;
          case TypeCode.Char:
            bytes.AddRange(BitConverter.GetBytes(ic.ToChar(null)));
            break;
          case TypeCode.Double:
            bytes.AddRange(BitConverter.GetBytes(ic.ToDouble(null)));
            break;
          case TypeCode.Int16:
            bytes.AddRange(BitConverter.GetBytes(ic.ToInt16(null)));
            break;
          case TypeCode.Int32:
            bytes.AddRange(BitConverter.GetBytes(ic.ToInt32(null)));
            break;
          case TypeCode.Int64:
            bytes.AddRange(BitConverter.GetBytes(ic.ToInt64(null)));
            break;
          case TypeCode.SByte:
            bytes.Add((byte)ic.ToSByte(null));
            break;
          case TypeCode.Single:
            bytes.AddRange(BitConverter.GetBytes(ic.ToSingle(null)));
            break;
          case TypeCode.UInt16:
            bytes.AddRange(BitConverter.GetBytes(ic.ToUInt16(null)));
            break;
          case TypeCode.UInt32:
            bytes.AddRange(BitConverter.GetBytes(ic.ToUInt32(null)));
            break;
          case TypeCode.UInt64:
            bytes.AddRange(BitConverter.GetBytes(ic.ToUInt64(null)));
            break;
        }
      }
      return bytes.ToArray();
    }

    /// <summary>
    /// The element values or fields of the array/struct/union to initialize.
    /// </summary>
    public IEnumerable<Expression> Expressions {
      get {
        for (int i = 0, n = this.expressions.Count; i < n; i++)
          yield return this.expressions[i] = this.expressions[i].MakeCopyFor(this.ContainingBlock);
      }
    }
    readonly List<Expression> expressions;

    /// <summary>
    /// Convert a string to an initializer. "12" will be turned to {'1','2'}. Zeros will 
    /// be patched if the length of the string is smaller than size. If the length of the 
    /// string is greater than size, then only the first size of chars will be converted, 
    /// unless if size is zero or less, in which case, the count number of chars will be
    /// converted.
    /// </summary>
    /// <param name="initialValue"></param>
    /// <param name="size"></param>
    /// <returns></returns>
    public static VccInitializer fromStringWithPatchedZeros(string initialValue, int size, Expression parent) {
      if (initialValue == null) return null;
      int count = initialValue.Length;
      if (size <= 0) size = count;
      char[] charArr = initialValue.ToCharArray();
      List<Expression> exprs = new List<Expression>();
      VccInitializer result = new VccInitializer(exprs, SourceDummy.SourceLocation);
      for (uint i = 0; i < size; i++) {
        if (i < count) {
          sbyte val = (sbyte)charArr[i];
          Expression ch = new CompileTimeConstant(val, parent.SourceLocation);
          ch.SetContainingExpression(parent);
          exprs.Add(ch);
        }
          // If we dont have enough element, we patch zero. It is intentional that no '\0' is added
          // if size == count.
        else {
          Expression zeroPatch = new CompileTimeConstant(0, parent.SourceLocation);
          zeroPatch.SetContainingExpression(parent);
          exprs.Add(zeroPatch);
        }
      }
      result.SetContainingExpression(parent);
      return result;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccInitializer(containingBlock, this);
    }

    override internal int ExpressionCount {
      get { return this.expressions.Count; }
    }

    /// <summary>
    /// Completes the two stage construction of this object. This allows bottom up parsers to construct an Expression before constructing the containing Expression.
    /// This method should be called once only and must be called before this object is made available to client code. The construction code itself should also take
    /// care not to call any other methods or property/event accessors on the object until after this method has been called.
    /// </summary>
    public override void SetContainingExpression(Expression containingExpression) {
      base.SetContainingExpression(containingExpression);
      for (int i = 0, n = this.expressions.Count; i < n; i++)
        this.expressions[i].SetContainingExpression(containingExpression);
    }
  }

  /// <summary>
  /// An expression that invokes a method.
  /// </summary>
  public class VccMethodCall : MethodCall {

    /// <summary>
    /// Allocates an expression that invokes a method.
    /// </summary>
    /// <param name="methodExpression">An expression that, if correct, results in a delegate or method group.</param>
    /// <param name="originalArguments">Expressions that result in the arguments to be passed to the called method.</param>
    /// <param name="sourceLocation">The source location of the call expression.</param>
    public VccMethodCall(Expression methodExpression, IEnumerable<Expression> originalArguments, ISourceLocation sourceLocation)
      : base(methodExpression, originalArguments, sourceLocation)
    {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccMethodCall(BlockStatement containingBlock, VccMethodCall template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      bool result = false;
      if (this.ResolvedMethod != Dummy.Method) {
        int i = 0;
        var formalEnum = this.ResolvedMethod.Parameters.GetEnumerator();
        var actualEnum = this.OriginalArguments.GetEnumerator();
        while (formalEnum.MoveNext()) {
          if (!actualEnum.MoveNext()) break;
          i++;
          if (formalEnum.Current.IsOut && !(actualEnum.Current is OutArgument)) {
            this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.ArgumentMustBePassedWithOutKeyword, i.ToString(System.Globalization.CultureInfo.InvariantCulture)));
            result = true;
          }
          if (!formalEnum.Current.IsOut && actualEnum.Current is OutArgument) {
            this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.ArgumentShouldNotBePassedWithOutKeyword, i.ToString(System.Globalization.CultureInfo.InvariantCulture)));
          }
        }
      }
      return result | base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

    /// <summary>
    /// Returns a list of the arguments to pass to the constructor, indexer or method, after they have been converted to match the parameters of the resolved method.
    /// </summary>
    protected override List<Expression> ConvertArguments() {
      return this.Helper.ConvertArguments(this.OriginalArguments, this.ResolvedMethod.Parameters, true);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccMethodCall(containingBlock, this);
    }

    /// <summary>
    /// Uses the this.OriginalArguments and this.GetCandidateMethods to resolve the actual method to call.
    /// </summary>
    protected override IMethodDefinition ResolveMethod() {
      IEnumerable<IMethodDefinition> candidateMethods = this.GetCandidateMethods(false);
      IMethodDefinition resolvedMethod = this.Helper.ResolveOverload(candidateMethods, this.OriginalArguments, true);
      if (resolvedMethod == Dummy.Method) {
        if (this.ComplainedAboutArguments()) return resolvedMethod;
        if (this.ComplainedAboutFailedInferences()) return resolvedMethod;
        this.ComplainAboutCallee();
      }

      return resolvedMethod;
    }

  }

  public class VccReturnValue : ReturnValue
  {
    /// <summary>
    /// Allocates an expression that refers to the return value of a method.
    /// </summary>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccReturnValue(ISourceLocation sourceLocation)
      : base(sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccReturnValue(BlockStatement containingBlock, ReturnValue template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccReturnValue(containingBlock, this);
    }

    public override ITypeDefinition InferType() {
      // check if this is used to refer to the result of an atomic operation
      // these have the original operation as their last argument, which is thus also the type
      // of 'result' in that context
      BlockStatement block = this.ContainingBlock;
      while (block != block.ContainingBlock) {
        VccAtomicOpBlock aoBlock = block as VccAtomicOpBlock;
        if (aoBlock != null) {
          return aoBlock.AtomicOp.Type;
        }
        block = block.ContainingBlock;
      }

      return base.InferType();
    }
  }

  public class VccAtomicOp : VccMethodCall
  {
    /// <summary>
    /// Allocates an expression that invokes a method.
    /// </summary>
    /// <param name="methodExpression">An expression that, if correct, results in a delegate or method group.</param>
    /// <param name="originalArguments">Expressions that result in the arguments to be passed to the called method.</param>
    /// <param name="sourceLocation">The source location of the call expression.</param>
    public VccAtomicOp(Expression methodExpression, IEnumerable<Expression> originalArguments, ISourceLocation sourceLocation)
      : base(methodExpression, originalArguments, sourceLocation)
    {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccAtomicOp(BlockStatement containingBlock, VccAtomicOp template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccAtomicOp(containingBlock, this);
    }

    public override ITypeDefinition Type {
      get {
        Expression lastArg = null;
        foreach (var arg in this.OriginalArguments)
          lastArg = arg;
        if (lastArg != null) return lastArg.Type;
        return Dummy.Type;
      }
    }
  }

  /// <summary>
  /// An expression that results in the remainder of dividing value the left operand by the value of the right operand. 
  /// When the operator is overloaded, this expression corresponds to a call to op_Modulus.
  /// </summary>
  public class VccModulus : Modulus {

    /// <summary>
    /// Allocates an expression that results in the remainder of dividing value the left operand by the value of the right operand. 
    /// When the operator is overloaded, this expression corresponds to a call to op_Modulus.
    /// </summary>
    /// <param name="leftOperand">The left operand.</param>
    /// <param name="rightOperand">The right operand.</param>
    /// <param name="sourceLocation">The source location of the operation.</param>
    public VccModulus(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccModulus(BlockStatement containingBlock, VccModulus template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Returns true if no information is lost if the integer value of this expression is converted to the target integer type.
    /// </summary>
    /// <param name="targetType"></param>
    /// <returns></returns>
    public override bool IntegerConversionIsLossless(ITypeDefinition targetType)
    {
      return this.LeftOperand.IntegerConversionIsLossless(targetType) || base.IntegerConversionIsLossless(targetType);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccModulus(containingBlock, this);
    }

    /// <summary>
    /// A list of dummy methods that correspond to operations that are built into IL. The dummy methods are used, via overload resolution,
    /// to determine how the operands are to be converted before the operation is carried out.
    /// </summary>
    protected override IEnumerable<IMethodDefinition> StandardOperators {
      get {
        BuiltinMethods dummyMethods = this.Compilation.BuiltinMethods;
        yield return dummyMethods.Int32opInt32;
        yield return dummyMethods.Int32opUInt32;
        yield return dummyMethods.UInt32opUInt32;
        yield return dummyMethods.Int64opInt64;
        yield return dummyMethods.Int64opUInt64;
        yield return dummyMethods.UInt64opUInt64;
        yield return dummyMethods.Float32opFloat32;
        yield return dummyMethods.Float64opFloat64;
        yield return dummyMethods.DecimalOpDecimal;
      }
    }

  }

  /// <summary>
  /// An expression that results in the remainder of dividing value the left operand by the value of the right operand. 
  /// The result of the expression is assigned to the left operand, which must be a target expression.
  /// When the operator is overloaded, this expression corresponds to a call to op_Modulus.
  /// </summary>
  public class VccModulusAssignment : ModulusAssignment {

    /// <summary>
    /// Allocates an expression that results in the remainder of dividing value the left operand by the value of the right operand. 
    /// The result of the expression is assigned to the left operand, which must be a target expression.
    /// When the operator is overloaded, this expression corresponds to a call to op_Modulus.
    /// </summary>
    /// <param name="leftOperand">The left operand and target of the assignment.</param>
    /// <param name="rightOperand">The right operand.</param>
    /// <param name="sourceLocation">The source location of the operation.</param>
    public VccModulusAssignment(TargetExpression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccModulusAssignment(BlockStatement containingBlock, VccModulusAssignment template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccModulusAssignment(containingBlock, this);
    }

    /// <summary>
    /// Creates a modulus expression with the given left operand and this.RightOperand.
    /// The method does not use this.LeftOperand.Expression, since it may be necessary to factor out any subexpressions so that
    /// they are evaluated only once. The given left operand expression is expected to be the expression that remains after factoring.
    /// </summary>
    /// <param name="leftOperand">An expression to combine with this.RightOperand into a binary expression.</param>
    protected override Expression CreateBinaryExpression(Expression leftOperand) {
      Expression result = new VccModulus(leftOperand, this.RightOperand, this.SourceLocation);
      result.SetContainingExpression(this);
      return result;
    }
  }

  /// <summary>
  /// An expression that refers to a type by specifying the type name.
  /// </summary>
  public class VccNamedTypeExpression : NamedTypeExpression {

    /// <summary>
    /// Allocates an expression that refers to a type by specifying the type name.
    /// </summary>
    /// <param name="expression">An expression that names a type. 
    /// Must be an instance of SimpleName, QualifiedName or AliasQualifiedName.</param>
    public VccNamedTypeExpression(Expression expression)
      : this(expression, true)
    {
    }

    /// <summary>
    /// Allocates an expression that refers to a type by specifying the type name.
    /// </summary>
    /// <param name="expression">An expression that names a type. 
    /// <param name="silentlyResolveToVoid">When the expression cannot be resolved to a type, silently resolve it to Void</param>
    /// Must be an instance of SimpleName, QualifiedName or AliasQualifiedName.</param>
    public VccNamedTypeExpression(Expression expression, bool silentlyResolveToVoid)
      : base(expression)
      //^ requires expression is SimpleName || expression is QualifiedName || expression is AliasQualifiedName;
    {
      this.silentlyResolveToVoid = silentlyResolveToVoid;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    private VccNamedTypeExpression(BlockStatement containingBlock, VccNamedTypeExpression template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
      this.silentlyResolveToVoid = template.silentlyResolveToVoid;
    }

    private readonly bool silentlyResolveToVoid;

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccNamedTypeExpression(containingBlock, this);
    }

    /// <summary>
    /// Resolves the expression as a type with the given number of generic parameters. 
    /// If expression cannot be resolved an error is reported and a dummy type is returned. 
    /// If the expression is ambiguous (resolves to more than one type) an error is reported and the first matching type is returned.
    /// </summary>
    /// <param name="numberOfTypeParameters">The number of generic parameters the resolved type must have. This number must be greater than or equal to zero.</param>
    /// <returns>The resolved type if there is one, or Dummy.Type.</returns>
    public override ITypeDefinition Resolve(int numberOfTypeParameters)
      //^^ requires numberOfTypeParameters >= 0;
      //^^ ensures result == Dummy.Type || result.GenericParameterCount == numberOfTypeParameters;
    {
      ITypeDefinition result = Dummy.Type;
      SimpleName/*?*/ simpleName = this.Expression as SimpleName;
      if (simpleName != null) {
        result = this.Resolve(simpleName.ResolveAsNamespaceOrType(), numberOfTypeParameters);
        //^ assert result == Dummy.Type || result.GenericParameterCount == numberOfTypeParameters;
        if (result == Dummy.Type) {
          if (!silentlyResolveToVoid)
            this.Helper.ReportError(new AstErrorMessage(this, Microsoft.Cci.Ast.Error.SingleTypeNameNotFound, simpleName.Name.Value));
          else
            DidSilentlyResolveToVoid = true;
          result = this.Compilation.PlatformType.SystemVoid.ResolvedType;
        }
        //^ assume result == Dummy.Type || result.GenericParameterCount == numberOfTypeParameters;
        return result;
      }
      QualifiedName/*?*/ qualifiedName = this.Expression as QualifiedName;
      if (qualifiedName != null) return this.Resolve(qualifiedName.ResolveAsNamespaceOrTypeGroup(), numberOfTypeParameters);
      AliasQualifiedName/*?*/ aliasQualName = this.Expression as AliasQualifiedName;
      if (aliasQualName != null) return this.Resolve(aliasQualName.Resolve(), numberOfTypeParameters);
      //^ assert false;
      return result;
    }

    public bool DidSilentlyResolveToVoid { get; private set; }
  }

  public class VccPointerScopedName : PointerQualifiedName
  {
    public VccPointerScopedName(Expression qualifier, SimpleName simpleName, ISourceLocation sourceLocation)
      : base(qualifier, simpleName, sourceLocation) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    private VccPointerScopedName(BlockStatement containingBlock, VccPointerScopedName template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccPointerScopedName(containingBlock, this);
    }

    protected override ITypeDefinitionMember ResolveTypeMember(ITypeDefinition qualifyingType) {
      IPointerTypeReference/*?*/ pointerQualifyingType = qualifyingType as IPointerTypeReference;
      if (pointerQualifyingType == null) return null;
      ITypeDefinition type = pointerQualifyingType.TargetType.ResolvedType;
      foreach (ITypeDefinitionMember member in type.Members) {
        INestedTypeDefinition nestedType = member as INestedTypeDefinition;
        if (nestedType != null && this.TypeIsMarkedAsGroup(nestedType.ResolvedType)) return nestedType;
        IFieldDefinition field = member as IFieldDefinition;
        if (field != null && this.TypeIsNamedMember(field.Type.ResolvedType)) return field;
      }
      return null;
    }

    bool TypeIsMarkedAsGroup(ITypeDefinition type) {
      foreach (ICustomAttribute attr in type.Attributes) {
        if (TypeHelper.GetTypeName(attr.Type) == "Microsoft.Contracts.GroupDeclAttr") {
          List<IMetadataExpression> args = new List<IMetadataExpression>(attr.Arguments);
          if (args.Count == 1) {
            IMetadataConstant groupName = args[0] as IMetadataConstant;
            if (groupName != null) return (this.NameTable.GetNameFor((string)groupName.Value) == this.SimpleName.Name);
          }
        }
      }
      return false;
    }

    bool TypeIsNamedMember(ITypeDefinition type) {
      foreach (ICustomAttribute attr in type.Attributes) {
        if (TypeHelper.GetTypeName(attr.Type) == "Microsoft.Contracts.StringVccAttr") {
          List<IMetadataExpression> args = new List<IMetadataExpression>(attr.Arguments);
          if (args.Count == 2) {
            IMetadataConstant attrStr = args[0] as IMetadataConstant;
            IMetadataConstant name = args[1] as IMetadataConstant;
            if (attr != null && name != null && (string)attrStr.Value == "member_name" && this.NameTable.GetNameFor((string)name.Value) == this.SimpleName.Name)
              return true;
          }
        }
      }
      return false;
    }

    protected override string RhsToStringForError() {
      return "::" + this.SimpleName.Name.Value;
    }

    protected override IExpression ProjectAsNonConstantIExpression()
    {
      if (this.cachedProjection == null) {
        object member = this.ResolveAsValueContainer();
        INestedTypeDefinition groupType = member as INestedTypeDefinition;
        if (groupType != null) {
          // expression refers to a group
          Cast cast = new Cast(new Parenthesis(this.Instance, this.SourceLocation), TypeExpression.For(this.Type), this.SourceLocation);
          cast.SetContainingExpression(this);
          // TODO the projection of cast looses its source location, it only retains the source location of the casted expression,
          // so we stick the proper location on the casted expression itself
          return this.cachedProjection = cast.ProjectAsIExpression();
        }
        IFieldDefinition fieldDef = member as IFieldDefinition;
        if (fieldDef != null) {
          var addrOf = new AddressOf(new AddressableExpression(new ProjectionHelper(this.Qualifier, this.SimpleName, fieldDef, this.SourceLocation)), this.SourceLocation);
          addrOf.SetContainingExpression(this);
          return this.cachedProjection = addrOf.ProjectAsIExpression();
        }
        this.cachedProjection = new DummyExpression(this.SourceLocation);
      }
      return this.cachedProjection;
    }

    IExpression cachedProjection = null;

    public override ITypeDefinition InferType() {
      object member = this.ResolveAsValueContainer();
      INestedTypeDefinition groupType = member as INestedTypeDefinition;
      if (groupType != null) return PointerType.GetPointerType(groupType, this.Compilation.HostEnvironment.InternFactory);
      IFieldDefinition field = member as IFieldDefinition;
      if (field != null) return PointerType.GetPointerType(field.Type.ResolvedType, this.Compilation.HostEnvironment.InternFactory);
      return Dummy.Type;
    }

    public override string ToString() {
      return this.Qualifier.ToString() + "::" + this.SimpleName.ToString();
    }

    private class ProjectionHelper : PointerQualifiedName
    {
      private readonly IFieldDefinition definition;

      public ProjectionHelper(Expression qualifier, SimpleName simpleName, IFieldDefinition definition, ISourceLocation sourceLocation)
        : base(qualifier, simpleName, sourceLocation) {
        this.definition = definition;
      }

      protected override ITypeDefinitionMember ResolveTypeMember(ITypeDefinition qualifyingType) {
        return definition;
      }

      public override ITypeDefinition InferType() {
        return definition.Type.ResolvedType;
      }
    }
  }


  /// <summary>
  /// An expression that denotes a pointer type.
  /// </summary>
  public class VccPointerTypeExpression : PointerTypeExpression {
    /// <summary>
    /// Allocates an expression that denotes a pointer type.
    /// </summary>
    /// <param name="elementType">The type of value that the pointer points to.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccPointerTypeExpression(TypeExpression elementType, List<TypeQualifier>/*?*/ qualifiers, ISourceLocation sourceLocation)
      : base(elementType, sourceLocation) {
      this.qualifiers = qualifiers;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    private VccPointerTypeExpression(BlockStatement containingBlock, VccPointerTypeExpression template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
      this.qualifiers = template.qualifiers;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccPointerTypeExpression(containingBlock, this);
    }

    readonly List<TypeQualifier>/*?*/ qualifiers;

    /// <summary>
    /// Returns the type denoted by the expression. If expression cannot be resolved, a dummy type is returned. If the expression is ambiguous the first matching type is returned.
    /// If the expression does not resolve to exactly one type, an error is added to the error collection of the compilation context.
    /// </summary>
    protected override ITypeDefinition Resolve() {
      VccFunctionTypeExpression/*?*/ ftexpr = this.ElementType as VccFunctionTypeExpression;
      if (ftexpr != null) return this.Resolve(ftexpr);
      if (this.qualifiers != null) {
        List<ICustomModifier> modifiers = new List<ICustomModifier>(2);
        foreach (TypeQualifier qualifier in this.qualifiers) {
          switch (qualifier.Token) {
            case Token.Const:
              modifiers.Add(new CustomModifier(true, this.PlatformType.SystemRuntimeCompilerServicesIsConst));
              break;
            case Token.Volatile:
              modifiers.Add(new CustomModifier(false, this.PlatformType.SystemRuntimeCompilerServicesIsVolatile));
              break;
            //TODO: record p.IsRestricted. (Need a new modifier for that).
          }
        }
        if (modifiers.Count != 0) 
          return ModifiedPointerType.GetPointerType(this.ElementType.ResolvedType, modifiers, this.Compilation.HostEnvironment.InternFactory);
      }

      ITypeDefinition resolvedElementType = this.ElementType.ResolvedType;
      VccNamedTypeExpression namedType = this.ElementType as VccNamedTypeExpression;
      if (namedType != null && namedType.DidSilentlyResolveToVoid) {
          Expression typePtrRef= NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.Compilation.NameTable, "TypedPtr");
          typePtrRef.SetContainingExpression(this);
          return new VccNamedTypeExpression(typePtrRef).Resolve(0);
      }

      return PointerType.GetPointerType(resolvedElementType, this.Compilation.HostEnvironment.InternFactory);
    }

    /// <summary>
    /// Returns the function pointer denoted by this expression.
    /// </summary>
    private IFunctionPointer Resolve(VccFunctionTypeExpression functionTypeExpression) {
      List<ParameterDefinition> parameters = new List<ParameterDefinition>();
      foreach (ParameterDeclaration par in functionTypeExpression.Parameters)
        parameters.Add(par.ParameterDefinition);
      IEnumerable<IParameterTypeInformation> parameterTypes = IteratorHelper.GetConversionEnumerable<ParameterDefinition, IParameterTypeInformation>(parameters);
      //List<ICustomModifier> returnValueModifiers = new List<ICustomModifier>(1);
      //returnValueModifiers.Add(new CustomModifier(true, this.PlatformType.SystemRuntimeCompilerServicesCallConvCdecl));
      //FunctionPointerType result = new FunctionPointerType(functionTypeExpression.CallingConvention, false, functionTypeExpression.ReturnType.ResolvedType, returnValueModifiers.AsReadOnly(), parameterTypes, null, this.Compilation.HostEnvironment.InternFactory);
      FunctionPointerType result = new FunctionPointerType(functionTypeExpression.CallingConvention, false, functionTypeExpression.ReturnType.ResolvedType, null, parameterTypes, null, this.Compilation.HostEnvironment.InternFactory);
      IMethodContract/*?*/ contract = this.Compilation.ContractProvider.GetMethodContractFor(this);
      if (contract != null) this.Compilation.ContractProvider.AssociateMethodWithContract(result, contract);
      return result;
    }

    public override void SetContainingExpression(Expression containingExpression) {
      base.SetContainingExpression(containingExpression);
      BlockStatement containingBlock = containingExpression.ContainingBlock;
      VccFunctionTypeExpression/*?*/ ftExpr = this.ElementType as VccFunctionTypeExpression;
      if (ftExpr != null) {
        FunctionDeclaration fdecl = new FunctionDeclaration(ftExpr.AcceptsExtraArguments, null, false, ftExpr.CallingConvention, TypeMemberVisibility.Public, ftExpr.ReturnType, ftExpr.Name, null, ftExpr.parameters, ftExpr.SourceLocation);
        fdecl.SetContainingTypeDeclaration(containingBlock.CompilationPart.GlobalDeclarationContainer);
        foreach (ParameterDeclaration parameter in ftExpr.parameters)
          parameter.SetContainingSignatureAndExpression(fdecl, containingExpression);
        containingBlock = fdecl.DummyBlock;
      }
      MethodContract/*?*/ contract = this.Compilation.ContractProvider.GetMethodContractFor(this) as MethodContract;
      if (contract != null) contract.SetContainingBlock(containingBlock);
    }
  }

  /// <summary>
  /// An expression that denotes a template type parameter.
  /// </summary>
  public class VccTemplateTypeParameterExpression : NamedTypeExpression {

    /// <summary>
    /// Allocates an expression that denotes a template type parameter.
    /// </summary>
    /// <param name="simpleName">The name of the template type parameter.</param>
    public VccTemplateTypeParameterExpression(SimpleName simpleName)
      : base (simpleName) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccTemplateTypeParameterExpression(BlockStatement containingBlock, VccTemplateTypeParameterExpression template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccTemplateTypeParameterExpression(containingBlock, this);
    }

  }

  public class VccScopedName : QualifiedName {

    public VccScopedName(Expression qualifier, SimpleName simpleName, ISourceLocation sourceLocation)
      : base(qualifier, simpleName, sourceLocation) {
    }

    protected VccScopedName(BlockStatement containingBlock, VccScopedName template)
      : base(containingBlock, template) {
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      inErrorCheck = true;
      bool result = base.CheckForErrorsAndReturnTrueIfAnyAreFound();
      inErrorCheck = false;
      return result;
    }

    private bool inErrorCheck;

    protected override ITypeDefinitionMember ResolveTypeMember(ITypeDefinition qualifyingType)
    {
      if (this.resolvedMember == null)
        this.resolvedMember = this.ResolveAgainstGroup(qualifyingType);

      if (this.resolvedMember == null) {
        this.resolvedMember = Dummy.Method;
        if (!this.inErrorCheck)
        {
          this.Helper.ReportError(new AstErrorMessage(this, Microsoft.Cci.Ast.Error.NoSuchMember,
                                                            qualifyingType.ResolvedType.ToString(),
                                                            this.SimpleName.ToString()));
          this.hasErrors = true;
        }
      }
      else if (!this.inErrorCheck)
        this.hasErrors = false;

      if (this.resolvedMember == Dummy.Method) return null;
      return this.resolvedMember;
    }
    private ITypeDefinitionMember/*?*/ resolvedMember;

    private ITypeDefinitionMember ResolveAgainstGroup(ITypeDefinition type)
    {
      foreach (ITypeDefinitionMember member in type.Members) {
        INestedTypeDefinition nestedType = member as INestedTypeDefinition;
        if (nestedType != null && this.TypeIsMarkedAsGroup(nestedType.ResolvedType)) return nestedType;
      }

      return null;
    }

    public static bool IsGroupType(ITypeDefinition type)
    {
      foreach (ICustomAttribute attr in type.Attributes) {
        if (TypeHelper.GetTypeName(attr.Type) == "Microsoft.Contracts.GroupDeclAttr")
          return true;
      }
      return false;
    }

    bool TypeIsMarkedAsGroup(ITypeDefinition type)
    {
      foreach (ICustomAttribute attr in type.Attributes) {
        if (TypeHelper.GetTypeName(attr.Type) == "Microsoft.Contracts.GroupDeclAttr") {
          List<IMetadataExpression> args = new List<IMetadataExpression>(attr.Arguments);
          if (args.Count == 1) {
            IMetadataConstant groupName = args[0] as IMetadataConstant;
            if (groupName != null) return (this.NameTable.GetNameFor((string)groupName.Value) == this.SimpleName.Name);
          }
        }
      }
      return false;
    }

    protected override IExpression ProjectAsNonConstantIExpression()
    {
      INestedTypeDefinition typeDef = this.Resolve() as INestedTypeDefinition;
      if (typeDef != null)
        return TypeExpression.For(typeDef).ProjectAsIExpression();
      else return new DummyExpression(this.SourceLocation);
    }

    public override ITypeDefinition InferType()
    {
      return Compilation.PlatformType.SystemType.ResolvedType;
    }

    public override Expression MakeCopyFor(BlockStatement containingBlock) {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccScopedName(containingBlock, this);
    }

    public override string ToString()
    {
      return this.Qualifier.ToString() + "::" + this.SimpleName.ToString();
    }
  }

  public class VccScopedTypeExpression : NamedTypeExpression {

    public VccScopedTypeExpression(VccScopedName typeExpression)
      : base(typeExpression)
    {
    }

    protected VccScopedTypeExpression(BlockStatement containingBlock, VccScopedTypeExpression template) 
      : base (containingBlock, template)
    {
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      return this.Expression.HasErrors() || base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    } 

    protected override ITypeDefinition Resolve(object resolvedExpression, int numberOfTypeParameters)
    {
      if (resolvedExpression is NestedTypeGroup) {
        INestedTypeDefinition nestedType = ((VccScopedName)this.Expression).Resolve() as INestedTypeDefinition;
        if (nestedType != null)
          return nestedType;
      }
      return base.Resolve(resolvedExpression, numberOfTypeParameters);
    }

    public override Expression MakeCopyFor(BlockStatement containingBlock)
    {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccScopedTypeExpression(containingBlock, this);
    }
  }

  public class VccSimpleName : SimpleName {
    /// <summary>
    /// Constructs an expression consisting of a simple name, for example "SimpleName".
    /// Use this constructor when constructing a new simple name. Do not give out the resulting instance to client
    /// code before completing the initialization by calling SetContainingExpression on the instance.
    /// </summary>
    /// <param name="name">The name to be wrapped as an expression.</param>
    /// <param name="sourceLocation">The source location of the SimpleName expression.</param>
    public VccSimpleName(IName name, ISourceLocation sourceLocation)
      : base(name, sourceLocation, false) {
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccSimpleName(BlockStatement containingBlock, VccSimpleName template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccSimpleName(containingBlock, this);
    }

    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      object/*?*/ container = this.Resolve();
      if (container == null) {
        this.Helper.ReportError(new AstErrorMessage(this, Microsoft.Cci.Ast.Error.NameNotInContext, this.Name.Value));
        return true;
      }
      if (container is ILocalDefinition || container is IParameterDefinition || container is IFieldDefinition || container is IPropertyDefinition || container is IMethodDefinition)
        return false;
      this.Helper.ReportError(new AstErrorMessage(this, Microsoft.Cci.Ast.Error.NameNotInContext, this.Name.Value)); //TODO: better error message
      return true;
    }

    protected override object/*?*/ ResolveUsing(BlockStatement block, bool restrictToNamespacesAndTypes)
      //^ ensures result == null || result is ITypeDefinition || result is INamespaceDefinition || result is ITypeGroup ||
      //^ (!restrictToNamespacesAndTypes && (result is ILocalDefinition || result is IParameterDefinition || result is ITypeDefinitionMember || result is INamespaceMember));
    {
      // The same logic as the base class' except that if the name resolves to a CLocalFunctionDeclaration,
      // we find a match at the top level. 
      // The function has some replicate code as the one it overrides. 
      if (!restrictToNamespacesAndTypes) {
        for (BlockStatement b = block; b.ContainingSignatureDeclaration == block.ContainingSignatureDeclaration && b.ContainingBlock != b && b.Scope is StatementScope; b = b.ContainingBlock) {
          IEnumerator<LocalDeclaration> locals = b.Scope.GetMembersNamed(this.Name, false).GetEnumerator();
          if (locals.MoveNext()) {
            LocalDeclaration localVarDecl = locals.Current;
            VccLocalFunctionDeclaration/*?*/ localFunc = localVarDecl as VccLocalFunctionDeclaration;
            if (locals.MoveNext()) {
              this.Helper.ReportError(new VccErrorMessage(locals.Current.SourceLocation, Error.LocalDuplicate, this.Name.Value));
            }
            if (localFunc != null) {
              return this.ResolveUsing(localFunc.MangledFunctionDeclaration.ContainingTypeDeclaration, restrictToNamespacesAndTypes);
            }
            return localVarDecl.LocalVariable;
          }
        }
        if (block.ContainingSignatureDeclaration != null) {
          int myKey = this.Name.UniqueKey;
          foreach (ParameterDeclaration par in block.ContainingSignatureDeclaration.Parameters) {
            int parKey =  par.Name.UniqueKey;
            if (parKey == myKey) return par.ParameterDefinition;
          }
        }
      }
      ISignatureDeclaration/*?*/ containingSignature = block.ContainingSignatureDeclaration;
      AnonymousDelegate/*?*/ anonymousDelegate = containingSignature as AnonymousDelegate;
      if (anonymousDelegate != null)
        return this.ResolveUsing(anonymousDelegate.ContainingBlock, restrictToNamespacesAndTypes);
      AnonymousMethod/*?*/ anonymousMethod = containingSignature as AnonymousMethod;
      if (anonymousMethod != null)
        return this.ResolveUsing(anonymousMethod.ContainingBlock, restrictToNamespacesAndTypes);
      if (block.ContainingSignatureDeclaration != null)
        return this.ResolveUsing(block.ContainingSignatureDeclaration, restrictToNamespacesAndTypes);
      if (block.ContainingTypeDeclaration != null)
        return this.ResolveUsing(block.ContainingTypeDeclaration, restrictToNamespacesAndTypes);
      return this.ResolveUsing(block.ContainingNamespaceDeclaration, restrictToNamespacesAndTypes);
    }

    /// <summary>
    /// Returns either null or the local variable, parameter, type parameter, type member, namespace member or type
    /// that binds to this name using the scope chain of the given method.
    /// </summary>
    /// <param name="signatureDeclaration">The signature bearing object whose scope chain is used to resolve this name.</param>
    /// <param name="restrictToNamespacesAndTypes">True if only namespaces and types should be considered when resolving this name.</param>
    protected override object/*?*/ ResolveUsing(ISignatureDeclaration signatureDeclaration, bool restrictToNamespacesAndTypes)
      //^ ensures result == null || result is ITypeDefinition || result is INamespaceDefinition || result is ITypeGroup ||
      //^ (!restrictToNamespacesAndTypes && (result is IParameterDefinition || result is ITypeDefinitionMember || result is INamespaceMember));
    {
      FunctionDeclaration/*?*/ functionDeclaration = signatureDeclaration as FunctionDeclaration;
      if (functionDeclaration != null && functionDeclaration.templateParameters != null) {
        foreach (GenericMethodParameterDeclaration templateParameter in functionDeclaration.templateParameters) {
          if (this.Name.UniqueKey == templateParameter.Name.UniqueKey)
            return templateParameter.GenericMethodParameterDefinition;
        }
      }
      return base.ResolveUsing(signatureDeclaration, restrictToNamespacesAndTypes);
    }

    /// <summary>
    /// Returns either null or the namespace member (group) that binds to this name. 
    /// This implementation of this method ignores global methods and variables, as is the case for C#. //TODO: is this really the case?
    /// </summary>
    /// <param name="namespaceDeclaration">The namespace to use to resolve this name.</param>
    /// <param name="restrictToNamespacesAndTypes">True if only namespaces and types should be considered when resolving this name.</param>
    protected override object/*?*/ ResolveUsing(NamespaceDeclaration namespaceDeclaration, bool restrictToNamespacesAndTypes)
      //^ ensures result == null || result is ITypeDefinition || result is INamespaceDefinition || result is ITypeGroup ||
      //^ (!restrictToNamespacesAndTypes && result is INamespaceMember);
    {
      // Overrides the SimpleName's corresponding method so that we can handle the scope encoding 
      // (the mangled C type names to encode scope information)
      string lookupNameString = this.Name.Value;
      // loop until we have tried every parent scope
      while (true) {
        IScope<INamespaceMember> scope = namespaceDeclaration.UnitNamespace;
        IName lookupName = this.NameTable.GetNameFor(lookupNameString);
        AliasDeclaration/*?*/ aliasDeclaration = null;
        UnitSetAliasDeclaration/*?*/ unitSetAliasDeclaration = null;
        if (!this.NamespaceDeclIsBusy(namespaceDeclaration))
          namespaceDeclaration.GetAliasNamed(lookupName, this.IgnoreCase, ref aliasDeclaration, ref unitSetAliasDeclaration);
        IEnumerable<INamespaceMember> members = scope.GetMembersNamed(lookupName, this.IgnoreCase);
        INamespaceTypeDefinition/*?*/ namespaceTypeDefinition = null;
        INamespaceDefinition/*?*/ nestedNamespaceDefinition = null;
        foreach (INamespaceMember member in members) {
          nestedNamespaceDefinition = member as INamespaceDefinition;
          if (nestedNamespaceDefinition != null) {
            //TODO: if aliasDeclaration != null give an ambiguous reference error
            return nestedNamespaceDefinition;
          }
          if (namespaceTypeDefinition == null) {
            namespaceTypeDefinition = member as INamespaceTypeDefinition;
            if (namespaceTypeDefinition != null && (aliasDeclaration == null || namespaceTypeDefinition.IsGeneric)) break;
            //carry on in case there is a generic type with this name. If not there is an ambiguity between the type and the alias.
          }
        }
        if (namespaceTypeDefinition != null) {
          //TODO: if aliasDeclaration != null give an ambiguous reference error if namespaceTypeDef is not generic
          if (this.Name.Value == lookupNameString) {
            return new NamespaceTypeGroup(this, scope, this);
          }
          VccSimpleName newSimpleName = new VccSimpleName(lookupName, this.sourceLocation);
          newSimpleName.SetContainingBlock(this.ContainingBlock);
          return new NamespaceTypeGroup(this, scope, newSimpleName);
        }
        if (this.NamespaceDeclIsBusy(namespaceDeclaration)) {
          //Have to ignore using statements.
          scope = namespaceDeclaration.UnitSetNamespace;
          members = scope.GetMembersNamed(lookupName, this.IgnoreCase);
        } else {
          if (aliasDeclaration != null) return aliasDeclaration.ResolvedNamespaceOrType;
          if (unitSetAliasDeclaration != null) {
            IUnitSetNamespace usns = unitSetAliasDeclaration.UnitSet.UnitSetNamespaceRoot;
            //^ assume usns is INamespaceDefinition; //IUnitSetNamespace : INamespaceDefinition
            return usns;
          }
          scope = namespaceDeclaration.Scope;
          members = scope.GetMembersNamed(lookupName, this.IgnoreCase); //Considers types that were imported into the namespace via using statements
        }
        foreach (INamespaceMember member in members) {
          if (nestedNamespaceDefinition == null) nestedNamespaceDefinition = member as INamespaceDefinition;
          namespaceTypeDefinition = member as INamespaceTypeDefinition;
          if (namespaceTypeDefinition != null) return new NamespaceTypeGroup(this, scope, this);
        }
        if (nestedNamespaceDefinition != null) return nestedNamespaceDefinition;
        // Test to see if we have reached the outermost scope, in which case lookupNameString
        // is not a mangled one. 
        if (!lookupNameString.Contains("^")) break;
        if (LexicalScope.LexicalScopeOf(lookupNameString) == "") {
          // outermost scope, generate an unmangled name for a last try. 
          lookupNameString = LexicalScope.UnmangledName(lookupNameString);
        } else {
          lookupNameString = LexicalScope.MangledNameWithOuterLexcialScope(lookupNameString);
        }
      }
      // The following (looking the name up in the nested namespace)
      // may not apply to the C case. Keep it here anyway. 
      NestedNamespaceDeclaration/*?*/ nestedNamespace = namespaceDeclaration as NestedNamespaceDeclaration;
      if (nestedNamespace != null) return this.ResolveUsing(nestedNamespace.ContainingNamespaceDeclaration, restrictToNamespacesAndTypes);         
      return null;
    }

    protected override object/*?*/ ResolveUsing(TypeDeclaration typeDeclaration, bool restrictToNamespacesAndTypes) {
      VccGlobalDeclarationContainerClass/*?*/ gdcc = typeDeclaration as VccGlobalDeclarationContainerClass;
      if (gdcc != null) {
        foreach (ITypeDeclarationMember member in gdcc.GlobalScope.GetMembersNamed(this.Name, false)) {
          TypedefDeclaration/*?*/ typedefDecl = member as TypedefDeclaration;
          if (typedefDecl != null) {
            if (typedefDecl.SourceLocation.EndIndex <= this.SourceLocation.StartIndex)
              return typedefDecl.Type.ResolvedType;
          }
          if (restrictToNamespacesAndTypes) continue;
          FunctionDeclaration/*?*/ functionDecl = member as FunctionDeclaration;
          if (functionDecl != null) return functionDecl.ResolvedMethod;
          FunctionDefinition/*?*/ functionDef = member as FunctionDefinition;
          if (functionDef != null) return functionDef.MethodDefinition;
        }

        if (this.Name.Value.StartsWith("__", StringComparison.Ordinal))
          return this.ResolveUsing(this.VccCompilationHelper.Runtime, false);
      }
      object/*?*/ result = base.ResolveUsing(typeDeclaration, restrictToNamespacesAndTypes);
      if (result == null && gdcc == null) {
        NestedTypeDeclaration lastSeenNestedType = null;
        foreach (ITypeDeclarationMember member in typeDeclaration.TypeDeclarationMembers) {
          NestedTypeDeclaration nestedType = member as NestedTypeDeclaration;
          if (nestedType != null) 
            lastSeenNestedType = nestedType;
          else {
            if (member is AnonymousFieldDefinition) {
              Debug.Assert(lastSeenNestedType != null);
              result = this.ResolveUsing(lastSeenNestedType, restrictToNamespacesAndTypes);
              if (result != null) return result;
            }
            lastSeenNestedType = null;
          }
        }
        result = this.ResolveUsing(this.ContainingBlock.CompilationPart.GlobalDeclarationContainer, restrictToNamespacesAndTypes);
      }
      return result;
    }

    public override bool ValueIsPolymorphicCompileTimeConstant
    {
      get
      {
        return this.Type.ResolvedType.IsEnum || base.ValueIsPolymorphicCompileTimeConstant;
      }
    }

    private VccCompilationHelper VccCompilationHelper {
      get {
        return (VccCompilationHelper)this.Helper;
      }
    }

  }

  /// <summary>
  /// An expression that computes the memory size of instances of a given type at runtime.
  /// </summary>
  public class VccSizeOf : Expression, ISizeOf {

    /// <summary>
    /// Allocates an expression that computes the memory size of instances of a given type at runtime.
    /// </summary>
    /// <param name="expression">The type to size, or the value to size.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccSizeOf(Expression expression, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.expression = expression;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccSizeOf(BlockStatement containingBlock, VccSizeOf template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
      this.expression = template.expression.MakeCopyFor(containingBlock);
    }

    /// <summary>
    /// Performs any error checks still needed and returns true if any errors were found in the statement or a constituent part of the statement.
    /// </summary>
    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      bool result = this.Expression.HasErrors();
      if (!result && this.Expression.Type.TypeCode == PrimitiveTypeCode.Void) {
        this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.SizeOfUnknown, this.Expression.SourceLocation.Source));
        result = false;
      }
      return result;
    }

    /// <summary>
    /// Calls the visitor.Visit(ISizeOf) method.
    /// </summary>
    public override void Dispatch(ICodeVisitor visitor) {
      visitor.Visit(this);
    }

    /// <summary>
    /// The type to size.
    /// </summary>
    public Expression Expression {
      get { return this.expression; }
    }
    readonly Expression expression;

    /// <summary>
    /// Computes the compile time value of the expression. Can be null.
    /// </summary>
    protected override object/*?*/ GetValue() {
      ITypeDefinition type = this.Expression.Type;
      if (type == Dummy.Type) return null;
      if (type.TypeCode == PrimitiveTypeCode.Void) return null;
      TypeExpression/*?*/ texpr = this.Expression as TypeExpression;
      if (texpr != null)
        type = texpr.ResolvedType;
      else if (type is IPointerType) {
        //Might be a fixed size array
        VccSimpleName/*?*/ sname = this.Expression as VccSimpleName;
        if (sname != null) {
          object/*?*/ container = sname.ResolveAsValueContainer();
          LocalDefinition/*?*/ loc = container as LocalDefinition;
          if (loc != null) {
            LocalDeclaration locDecl = loc.LocalDeclaration;
            VccInitializer/*?*/ initializer = locDecl.InitialValue as VccInitializer;
            if (initializer != null)
              return (uint)(TypeHelper.SizeOfType(((IPointerType)type).TargetType.ResolvedType) * IteratorHelper.EnumerableCount(initializer.Expressions));
            else
              type = locDecl.ContainingLocalDeclarationsStatement.Type;
          }
        }
      }
      uint result = (uint)TypeHelper.SizeOfType(type);
      if (result > 0) return result;
      return null;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccSizeOf(containingBlock, this);
    }

    /// <summary>
    /// Returns an object that implements IExpression and that represents this expression after language specific rules have been
    /// applied to it in order to determine its semantics. The resulting expression is a standard representation of the semantics
    /// of this expression, suitable for use by language agnostic clients and complete enough for translation of the expression
    /// into IL.
    /// </summary>
    protected override IExpression ProjectAsNonConstantIExpression() {
      return this;
    }

    /// <summary>
    /// Completes the two stage construction of this object. This allows bottom up parsers to construct an Expression before constructing the containing Expression.
    /// This method should be called once only and must be called before this object is made available to client code. The construction code itself should also take
    /// care not to call any other methods or property/event accessors on the object until after this method has been called.
    /// </summary>
    public override void SetContainingExpression(Expression containingExpression) {
      base.SetContainingExpression(containingExpression);
      this.expression.SetContainingExpression(this);
    }

    /// <summary>
    /// The type of value that the expression will evaluate to, as determined at compile time.
    /// </summary>
    public override ITypeDefinition Type {
      get { return this.PlatformType.SystemUInt32.ResolvedType; }
    }

    /// <summary>
    /// Returns true if the expression represents a compile time constant without an explicitly specified type. For example, 1 rather than 1L.
    /// Constant expressions such as 2*16 are polymorhpic if both operands are polymorhic.
    /// </summary>
    public override bool ValueIsPolymorphicCompileTimeConstant {
      get {
        return this.Value != null;
      }
    }

    #region ISizeOf Members

    public ITypeReference TypeToSize {
      get {
        ITypeDefinition type = this.Expression.Type;
        TypeExpression/*?*/ texpr = this.Expression as TypeExpression;
        if (texpr != null) type = texpr.ResolvedType;
        return type; 
      }
    }

    #endregion

    #region IExpression Members

    ITypeReference IExpression.Type {
      get { return this.Type; }
    }

    #endregion

  }

  /// <summary>
  /// An expression that validates that a pointer is not null and points to allocated memory of a size corresponding to the pointer type.
  /// </summary>
  public class VccValidatePointer : MethodCall {

    /// <summary>
    /// Allocates an expression that validates that a pointer is not null and points to allocated memory of a size corresponding to the pointer type.
    /// </summary>
    /// <param name="methodExpression">An expression that, if correct, results in a delegate or method group.</param>
    /// <param name="originalArguments">Expressions that result in the arguments to be passed to the called method.</param>
    /// <param name="sourceLocation">The source location of the call expression.</param>
    public VccValidatePointer(Expression methodExpression, IEnumerable<Expression> originalArguments, bool fromSAL, ISourceLocation sourceLocation)
      : base(methodExpression, originalArguments, sourceLocation) {
      this.fromSAL = fromSAL;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccValidatePointer(BlockStatement containingBlock, VccValidatePointer template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    /// <summary>
    /// Returns a list of the arguments to pass to the constructor, indexer or method, after they have been converted to match the parameters of the resolved method.
    /// </summary>
    protected override List<Expression> ConvertArguments() {
      IEnumerator<Expression> originalArgumentEnumerator = this.OriginalArguments.GetEnumerator();
      if (!originalArgumentEnumerator.MoveNext()) {
        //^ assume false; //The parser should never create this node with less than 1 argument or more than 2.
        return base.ConvertArguments();
      }
      Expression pointer = originalArgumentEnumerator.Current;
      IPointerType/*?*/ pointerType = pointer.Type as IPointerType;
      if (pointerType == null) {
        pointerType = this.GetPointerTypeCorrespondingToFixedArrayIfApplicable(pointer.Type);
        if (pointerType == null) return base.ConvertArguments();
        pointer = this.Helper.ExplicitConversion(pointer, pointerType);
        this.fixedSizeArray = true;
      }
      Expression size;
      if (!originalArgumentEnumerator.MoveNext()) {
        size = new CompileTimeConstant(TypeHelper.SizeOfType(pointerType.TargetType.ResolvedType), pointer.SourceLocation);
        size.SetContainingExpression(this);
      } else {
        size = originalArgumentEnumerator.Current;
      }
      IPointerType voidPointer = PointerType.GetPointerType(this.PlatformType.SystemVoid, this.Compilation.HostEnvironment.InternFactory);
      Expression convertedPointer = this.Helper.ImplicitConversionInAssignmentContext(pointer, voidPointer);
      if (convertedPointer is DummyExpression && !this.fromSAL) this.Helper.ReportFailedImplicitConversion(pointer, voidPointer);
      Expression convertedSize = this.Helper.ImplicitConversionInAssignmentContext(size, this.PlatformType.SystemUInt64.ResolvedType, true);
      if (convertedSize is DummyExpression) this.Helper.ReportFailedImplicitConversion(size, this.PlatformType.SystemUInt64.ResolvedType);
      List<Expression> result = new List<Expression>(2);
      result.Add(convertedPointer);
      result.Add(convertedSize);
      return result;
    }

    bool fixedSizeArray;
    bool fromSAL;

    private IPointerType/*?*/ GetPointerTypeCorrespondingToFixedArrayIfApplicable(ITypeDefinition type) {
      NestedTypeDefinition/*?*/ nestedType = type as NestedTypeDefinition;
      if (nestedType != null && nestedType.Name.Value.StartsWith("_FixedArrayOfSize", StringComparison.Ordinal)) {
        foreach (ITypeDefinitionMember member in nestedType.Members) {
          IFieldDefinition/*?*/ field = member as IFieldDefinition;
          if (field != null && field.Name.Value == "_ElementType")
            return PointerType.GetPointerType(field.Type.ResolvedType, this.Compilation.HostEnvironment.InternFactory);
        }
      }
      return null;
    }


    private new VccCompilationHelper Helper {
      [DebuggerNonUserCode]
      get {
        return (VccCompilationHelper)this.ContainingBlock.Helper;
      }
    }

    /// <summary>
    /// Returns __vcValid if it can be found.
    /// </summary>
    protected override IMethodDefinition ResolveMethod() {
      IMethodDefinition/*?*/ result = this.ResolveMethodExpression(this.MethodExpression) as IMethodDefinition;
      if (result == null) {
        this.ComplainAboutCallee();
        result = Dummy.Method;
      }
      return result;
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
      //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccValidatePointer(containingBlock, this);
    }

    /// <summary>
    /// Returns an object that implements IExpression and that represents this expression after language specific rules have been
    /// applied to it in order to determine its semantics. The resulting expression is a standard representation of the semantics
    /// of this expression, suitable for use by language agnostic clients and complete enough for translation of the expression
    /// into IL.
    /// </summary>
    protected override IExpression ProjectAsNonConstantIExpression() {
      IEnumerator<Expression> args = this.OriginalArguments.GetEnumerator();
      if (!(args.MoveNext() && (this.fixedSizeArray || args.Current.Type is IPointerType || args.Current.Type is IFunctionPointer))) {
        if (this.HasErrors()) {
          // when we already have reported an error for this construct we do not need to throw
          CompileTimeConstant result = new CompileTimeConstant(false, SourceDummy.SourceLocation);
          result.SetContainingExpression(this);
          return result;
        }
        throw new System.Exception();
      }
      return base.ProjectAsNonConstantIExpression();
    }

    /// <summary>
    /// The type of value that the expression will evaluate to, as determined at compile time.
    /// </summary>
    public override ITypeDefinition Type {
      get { return this.PlatformType.SystemBoolean.ResolvedType; }
    }

  }

  public class VccDivision : Division {

    public VccDivision(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    protected VccDivision(BlockStatement containingBlock, VccDivision template)
      : base(containingBlock, template)
    {
    }

    /// <summary>
    /// Returns true if no information is lost if the integer value of this expression is converted to the target integer type.
    /// </summary>
    /// <param name="targetType"></param>
    /// <returns></returns>
    public override bool IntegerConversionIsLossless(ITypeDefinition targetType)
    {
      return this.LeftOperand.IntegerConversionIsLossless(targetType) || base.IntegerConversionIsLossless(targetType);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccDivision(containingBlock, this);
    }
  }

  public class VccLeftShift : LeftShift {

    public VccLeftShift(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    protected VccLeftShift(BlockStatement containingBlock, VccLeftShift template)
      : base(containingBlock, template)
    {
    }

    public override bool CouldBeInterpretedAsUnsignedInteger {
      get {
        object/*?*/ value = this.Value;
        if (value == null) return this.LeftOperand.ValueIsPolymorphicCompileTimeConstant;
        return !this.Helper.SignBitIsSet(value);
      }
    }

    public override bool CouldBeInterpretedAsNegativeSignedInteger {
      get {
        object/*?*/ value = this.Value;
        if (value == null) return this.LeftOperand.ValueIsPolymorphicCompileTimeConstant;
        return this.Helper.SignBitIsSet(value);
      }
    }

    public override CompileTimeConstant GetAsConstant()
      //^^ requires this.Value != null;
    {
      CompileTimeConstant result = new CompileTimeConstantWhoseSignDependsOnAnotherExpression(base.GetAsConstant(), this.RightOperand);
      result.SetContainingExpression(this);
      return result;
    }

    /// <summary>
    /// Returns true if no information is lost if the integer value of this expression is converted to the target integer type.
    /// </summary>
    /// <param name="targetType"></param>
    /// <returns></returns>
    public override bool IntegerConversionIsLossless(ITypeDefinition targetType)
    {
      return this.LeftOperand.IntegerConversionIsLossless(targetType) || base.IntegerConversionIsLossless(targetType);
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      var /*?*/ right = this.ConvertedRightOperand.Value;
      if (right != null) {
        var shift = (int)right;
        var size = TypeHelper.SizeOfType(this.Type.ResolvedType) * 8;
        if (shift < 0 || shift >= size) {
          this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Vcc.Error.ShiftCountOutOfRange, (size - 1).ToString(System.Globalization.CultureInfo.InvariantCulture)));
          base.CheckForErrorsAndReturnTrueIfAnyAreFound();
          return true;
        }
      }
      return base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccLeftShift(containingBlock, this);
    }
  }

  public class VccRightShift : RightShift {

    public VccRightShift(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    protected VccRightShift(BlockStatement containingBlock, VccRightShift template)
      : base(containingBlock, template)
    {
    }

    public override CompileTimeConstant GetAsConstant()
      //^^ requires this.Value != null;
    {
      CompileTimeConstant result = new CompileTimeConstantWhoseSignDependsOnAnotherExpression(base.GetAsConstant(), this.RightOperand);
      result.SetContainingExpression(this);
      return result;
    }

    public override bool CouldBeInterpretedAsNegativeSignedInteger {
      get { return this.LeftOperand.ValueIsPolymorphicCompileTimeConstant; }
    }

    public override bool  CouldBeInterpretedAsUnsignedInteger {
      get { return this.LeftOperand.ValueIsPolymorphicCompileTimeConstant; }
    }

    /// <summary>
    /// Returns true if no information is lost if the integer value of this expression is converted to the target integer type.
    /// </summary>
    /// <param name="targetType"></param>
    /// <returns></returns>
    public override bool IntegerConversionIsLossless(ITypeDefinition targetType)
    {
      return this.LeftOperand.IntegerConversionIsLossless(targetType) || base.IntegerConversionIsLossless(targetType);
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      var /*?*/ right = this.ConvertedRightOperand.Value;
      if (right != null) {
        var shift = (int)right;
        var size = TypeHelper.SizeOfType(this.Type.ResolvedType) * 8;
        if (shift < 0 || shift >= size) {
          this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Vcc.Error.ShiftCountOutOfRange, (size - 1).ToString(System.Globalization.CultureInfo.InvariantCulture)));
          base.CheckForErrorsAndReturnTrueIfAnyAreFound();
          return true;
        }
      }
      return base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccRightShift(containingBlock, this);
    }
  }

  public class VccMultiplication : Multiplication {

    public VccMultiplication(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    protected VccMultiplication(BlockStatement containingBlock, VccMultiplication template)
      : base(containingBlock, template)
    {
    }

    /// <summary>
    /// Returns true if no information is lost if the integer value of this expression is converted to the target integer type.
    /// </summary>
    /// <param name="targetType"></param>
    /// <returns></returns>
    public override bool IntegerConversionIsLossless(ITypeDefinition targetType)
    {
      return (this.LeftOperand.IntegerConversionIsLossless(targetType) && 
              this.RightOperand.IntegerConversionIsLossless(targetType))
        || base.IntegerConversionIsLossless(targetType);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccMultiplication(containingBlock, this);
    }
  }

  public class VccSubtraction : Subtraction {

    public VccSubtraction(Expression leftOperand, Expression rightOperand, ISourceLocation sourceLocation)
      : base(leftOperand, rightOperand, sourceLocation)
    {
    }

    protected VccSubtraction(BlockStatement containingBlock, VccSubtraction template)
      : base(containingBlock, template)
    {
    }

    /// <summary>
    /// Returns true if no information is lost if the integer value of this expression is converted to the target integer type.
    /// </summary>
    /// <param name="targetType"></param>
    /// <returns></returns>
    public override bool IntegerConversionIsLossless(ITypeDefinition targetType)
    {
      return (this.LeftOperand.IntegerConversionIsLossless(targetType) && 
              this.RightOperand.IntegerConversionIsLossless(targetType))
        || base.IntegerConversionIsLossless(targetType);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccSubtraction(containingBlock, this);
    }

    protected override IMethodDefinition LookForOverloadMethod() {
      IMethodDefinition method = base.LookForOverloadMethod();
      return VccBitwiseAnd.ProvideUnsignedBias(method, this.LeftOperand, this.RightOperand, this.Compilation.BuiltinMethods);
    }
  }

  /// <summary>
  /// An expression that binds to the current object instance. Different from the framework
  /// version in typing rule.
  /// </summary>
  public class VccThisReference : ThisReference
  {
    /// <summary>
    /// Allocates an expression that binds to the current object instance.
    /// </summary>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccThisReference(ISourceLocation sourceLocation)
      : base(sourceLocation) {
    }
    
    /// <summary>
    /// Allocates an expression that binds to the current object instance.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccThisReference(BlockStatement containingBlock, ISourceLocation sourceLocation)
      : base(containingBlock, sourceLocation) {
    }

    
    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccThisReference(BlockStatement containingBlock, VccThisReference template)
      : base(containingBlock, template)
      //^ requires template.ContainingBlock != containingBlock;
      //^ ensures this.containingBlock == containingBlock;
    {
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      if (this.ContainingBlock.ContainingTypeDeclaration is VccGlobalDeclarationContainerClass)
      {
        this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.ThisNotAllowedHere));
        return true;        
      }
      else 
        return base.CheckForErrorsAndReturnTrueIfAnyAreFound();
    }

    /// <summary>
    /// The type of value that the expression will evaluate to, as determined at compile time.
    /// </summary>
    public override ITypeDefinition Type
    {
      [DebuggerNonUserCode]
      get
      {
        ITypeDefinition t = base.Type;
        return PointerType.GetPointerType(t, this.Compilation.HostEnvironment.InternFactory);
      }
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccThisReference(containingBlock, this);
    }
  }

  public abstract class Specifier : SourceItem {
    protected Specifier(ISourceLocation sourceLocation)
      : base(sourceLocation) {
    }
  }

  public abstract class CompositeTypeSpecifier : Specifier {
    protected CompositeTypeSpecifier(TypeExpression typeExpression)
      : base(typeExpression.SourceLocation) {
      this.TypeExpression = typeExpression;
    }

    internal readonly TypeExpression TypeExpression;
  }

  public class EnumSpecifier : CompositeTypeSpecifier {
    public EnumSpecifier(TypeExpression typeExpression)
      : base(typeExpression) {
    }
  }

  public class StructSpecifier : CompositeTypeSpecifier {
    public StructSpecifier(TypeExpression typeExpression)
      : base(typeExpression) {
    }
  }

  public class UnionSpecifier : CompositeTypeSpecifier {
    public UnionSpecifier(TypeExpression typeExpression)
      : base(typeExpression) {
    }
  }

  public class OutSpecifier : Specifier
  {
    public OutSpecifier(ISourceLocation sourceLocation)
      : base(sourceLocation) {
    }
  }

  public class StorageClassSpecifier : Specifier {
    public StorageClassSpecifier(Parsing.Token token, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.Token = token;
    }

    public readonly Parsing.Token Token;
  }

  public class DeclspecSpecifier : Specifier {
    public DeclspecSpecifier(IEnumerable<Expression> modifiers, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.Modifiers = modifiers;
    }

    public readonly IEnumerable<Expression> Modifiers;
  }

  public class PrimitiveTypeSpecifier : Specifier {
    public PrimitiveTypeSpecifier(Parsing.Token token, ISourceLocation sourceLocation)
      : base(sourceLocation)
      //^ requires token == Token.Void || token == Token.Char || token == Token.Short || token == Token.Int || token == Token.Int8 || token == Token.Int16 ||
      //^    token == Token.Int32 || token == Token.Int64 || token == Token.Long || token == Token.Float || token == Token.Double || 
      //^    token == Token.Signed || token == Token.Unsigned || token == Token.Bool;
    {
      this.Token = token;
    }

    public readonly Parsing.Token Token;
    //^ invariant this.Token == Token.Void || this.Token == Token.Char || this.Token == Token.Short || this.Token == Token.Int || this.Token == Token.Int8 || this.Token == Token.Int16 ||
    //^    this.Token == Token.Int32 || this.Token == Token.Int64 || this.Token == Token.Long || this.Token == Token.Float || this.Token == Token.Double || 
    //^    this.Token == Token.Signed || this.Token == Token.Unsigned ||  this.Token == Token.Bool;
  }

  public class ScopedTypeNameSpecifier : Specifier {
    public ScopedTypeNameSpecifier(VccScopedName scopedName)
      : base(scopedName.SourceLocation)
    {
      this.ScopedName = scopedName;
    }

    public static Specifier CreateForExpression(Expression nameExpression)
    {
      SimpleName simpleName = nameExpression as SimpleName;
      if (simpleName != null) return new TypedefNameSpecifier(simpleName);
      VccScopedName scopedName = nameExpression as VccScopedName;
      if (scopedName != null) return new ScopedTypeNameSpecifier(scopedName);
      throw new System.Exception();
    }

    public readonly VccScopedName ScopedName;
  }

  public class TypedefNameSpecifier : Specifier {
    public TypedefNameSpecifier(SimpleName typedefName) 
      : base(typedefName.SourceLocation) {
      this.TypedefName = typedefName;
    }

    public readonly SimpleName TypedefName;
  }

  public class TypeQualifier : Specifier {
    public TypeQualifier(Parsing.Token token, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.Token = token;
    }

    public readonly Parsing.Token Token;
  }

  public class FunctionSpecifier : Specifier {
    public FunctionSpecifier(Parsing.Token token, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.Token = token;
    }

    public readonly Parsing.Token Token;
  }


  /// <summary>
  /// An lambda expression (in other words a mathematical map).
  /// </summary>
  public class VccLambda : Quantifier
  {
    public VccLambda(List<LocalDeclarationsStatement> boundVariables, Expression condition, Expression lambdaExpr, ISourceLocation sourceLocation)
      : base(boundVariables, condition, sourceLocation)
    {
      this.lambdaExpr = lambdaExpr;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingBlock">A new value for containing block. This replaces template.ContainingBlock in the resulting copy of template.</param>
    /// <param name="template">The template to copy.</param>
    protected VccLambda(BlockStatement containingBlock, VccLambda template)
      : base(containingBlock, template)
    //^ requires template.ContainingBlock != containingBlock;
    //^ ensures this.containingBlock == containingBlock;
    {
      this.lambdaExpr = template.lambdaExpr.MakeCopyFor(base.Condition.ContainingBlock);
    }

    /// <summary>
    /// A string that names the quantifier.
    /// </summary>
    protected override string GetQuantifierName()
    {
      return "Lambda";
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      bool result = false;
      foreach (LocalDeclarationsStatement decl in this.BoundVariables)
        result |= decl.HasErrors();
      IsTrue convertedCondition = new IsTrue(base.Condition);
      result |= convertedCondition.HasErrors();
      result |= this.lambdaExpr.HasErrors();
      result |= this.HasSideEffect(true);
      return result;
    }

    public override bool HasSideEffect(bool reportError) {
      return new IsTrue(base.Condition).HasSideEffect(reportError);
    }

    /// <summary>
    /// Makes a copy of this expression, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Expression MakeCopyFor(BlockStatement containingBlock)
    //^^ ensures result.GetType() == this.GetType();
    //^^ ensures result.ContainingBlock == containingBlock;
    {
      if (containingBlock == this.ContainingBlock) return this;
      VccLambda result = new VccLambda(containingBlock, this);
      result.CopyTriggersFromTemplate(this);
      return result;
    }

    readonly Expression lambdaExpr;

    private ITypeDefinition CreateTypeForLambda() {
      Expression SysDiagContractsMap = NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.NameTable, "Map");
      var localDeclStmts = new List<LocalDeclarationsStatement>(this.BoundVariables);
      localDeclStmts.Reverse();

      TypeExpression texpr = TypeExpression.For(this.lambdaExpr.Type);

      foreach (var localDeclStmt in localDeclStmts) {
        foreach (var dummy in localDeclStmt.Declarations) // we care only about the number of variables of this type
          targetType = texpr; // keep track of the previous type because it is required for 
          texpr = new GenericTypeInstanceExpression(new NamedTypeExpression(SysDiagContractsMap),
                                                    new TypeExpression[] { localDeclStmt.TypeExpression, texpr },
                                                    SourceDummy.SourceLocation);
      }
      texpr = (TypeExpression)texpr.MakeCopyFor(this.ContainingBlock);
      return texpr.ResolvedType;
    }

    public override ITypeDefinition Type
    {
      get {
        if (this.type == null)
          type = this.CreateTypeForLambda();
        return type;
      }
    }
    ITypeDefinition type;

    protected override void AddAdditionalTypeParameters(List<TypeExpression> genericInstanceParameters)
    {
      // make sure we touch this.Type, which sets targetType
      if (this.Type != null) {
        genericInstanceParameters.Add((TypeExpression)targetType.MakeCopyFor(this.ContainingBlock));
      }
    }

    private TypeExpression targetType;

    private MethodCall cachedCondition;
    public override Expression Condition
    {
      get
      {
        if (cachedCondition != null) return cachedCondition;
        Expression inLambdaRef = NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.NameTable, "InLambda");
        Expression[] args = new Expression[] { base.Condition, this.lambdaExpr };
        cachedCondition = new MethodCall(inLambdaRef, args, SourceDummy.SourceLocation);
        cachedCondition.SetContainingExpression(this);
        return cachedCondition;
      }
    }

    /// <summary>
    /// Completes the two stage construction of this object. This allows bottom up parsers to construct an Expression before constructing the containing Expression.
    /// This method should be called once only and must be called before this object is made available to client code. The construction code itself should also take
    /// care not to call any other methods or property/event accessors on the object until after this method has been called.
    /// </summary>
    public override void SetContainingExpression(Expression containingExpression)
    {
      base.SetContainingExpression(containingExpression);

      Expression dummyExpression = new DummyExpression(this.ContainingBlock, this.SourceLocation);      
      this.lambdaExpr.SetContainingExpression(dummyExpression);
    }

  }
}