//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System.Collections.Generic;
using System.Resources;
using Microsoft.Cci.Ast;
using System.Diagnostics;
using Microsoft.Cci;
using Microsoft.Research.Vcc.Parsing;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc {

  public interface IVccStatement : IStatement
  {
    void Dispatch(IVccCodeVisitor visitor);
  }

  public interface IVccSpecStatement : IVccStatement
  {
    IStatement WrappedStatement { get; }
  }

  public interface IVccWrapStatement : IVccStatement
  {
    IExpression Object { get; }
  }

  public interface IVccUnwrapStatement : IVccStatement
  {
    IExpression Object { get; }
  }

  public interface IVccCodeVisitor : ICodeVisitor
  {
    void Visit(IVccSpecStatement specStatement);
    void Visit(IVccWrapStatement wrapStatement);
    void Visit(IVccUnwrapStatement unwrapStatement);
  }

  internal class StatementGroup : Statement {

    internal StatementGroup(List<Statement> statements)
      : base(SourceDummy.SourceLocation)
    {
      this.Statements = statements;
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      return false;
    }

    internal List<Statement> Statements;

    internal static void AddStatementOrGroupToList(Statement s, List<Statement> statements) {
      StatementGroup sg = s as StatementGroup;
      if (sg != null)
        statements.AddRange(sg.Statements);
      else
        statements.Add(s);
    }
  }

  public class VccLocalDefinition : LocalDefinition, ISpecItem
  {
    internal VccLocalDefinition(VccLocalDeclaration localDeclaration, List<Specifier> specifiers, bool isSpec)
      : base(localDeclaration) {
      this.specifiers = specifiers;
      this.isSpec = isSpec;
    }

    private readonly List<Specifier> specifiers;
    private readonly bool isSpec;

    public bool IsSpec {
      get { return this.isSpec; }
    }

    public bool IsVolatile {
      get { return VccCompilationHelper.ContainsTypeQualifier(this.specifiers, Token.Volatile); }
    }

  }

  /// <summary>
  /// A local declaration that appears as part of a statement containing a collection of local declarations, all with the same type.
  /// </summary>
  internal class VccLocalDeclaration : LocalDeclaration, ILocalDeclarationStatement, ISpecItem {

    /// <summary>
    /// Allocates local declaration that appears as part of a statement containing a collection of local declarations, all with the same type.
    /// </summary>
    /// <param name="name">The name of the local.</param>
    /// <param name="initialValue">The value, if any, to assign to the local as its initial value. May be null.</param>
    /// <param name="sourceLocation">The source location corresponding to the newly allocated expression.</param>
    public VccLocalDeclaration(NameDeclaration name, Expression/*?*/ initialValue, List<Specifier> specifiers, bool isSpec, ISourceLocation sourceLocation)
      : base(false, false, name, initialValue, sourceLocation)
    {
      this.specifiers = specifiers;
      this.isSpec = isSpec;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingLocalDeclarationsStatement">The containing statement. This should be different from the containing statement of the template declaration.</param>
    /// <param name="template">The statement to copy.</param>
    protected VccLocalDeclaration(LocalDeclarationsStatement containingLocalDeclarationsStatement, VccLocalDeclaration template)
      : base(containingLocalDeclarationsStatement, template) {
      this.specifiers = new List<Specifier>(template.specifiers);
      this.isSpec = template.isSpec;
    }

    private LanguageSpecificCompilationHelper Helper {
      get { return this.ContainingLocalDeclarationsStatement.CompilationPart.Helper; }
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      var containingSignature = this.ContainingLocalDeclarationsStatement.ContainingBlock.ContainingSignatureDeclaration;
      if (containingSignature != null) {
        foreach (var par in containingSignature.Parameters) {
          if (par.Name.UniqueKey == this.Name.UniqueKey) {
            this.Helper.ReportError(new VccErrorMessage(this.SourceLocation, Error.RedefinitionOfFormalParameter, this.Name.Value));
            return true;
          }
        }
      }
      return false;
    }

    public override void Dispatch(ICodeVisitor visitor) {
      visitor.Visit(this); // do not go to base.Dispatch because it will not do anything for const decls
      VccArrayTypeExpression/*?*/ arrayTypeExpression = this.ContainingLocalDeclarationsStatement.TypeExpression as VccArrayTypeExpression;
      if (this.InitialValue == null && arrayTypeExpression != null && arrayTypeExpression.Size != null) {
        Expression dummyExpression = new DummyExpression(this.Name.SourceLocation);
        TargetExpression targetExpression = new TargetExpression(new BoundExpression(dummyExpression, this.LocalVariable), this.Name.SourceLocation);
        Expression initialValue = new VccCreateStackArray(arrayTypeExpression.ElementType, arrayTypeExpression.Size, this.IsSpec, SourceDummy.SourceLocation);
        Assignment assignment = new Assignment(targetExpression, initialValue, this.SourceLocation);
        ExpressionStatement aStat = new ExpressionStatement(assignment);
        aStat.SetContainingBlock(this.ContainingLocalDeclarationsStatement.ContainingBlock);
        visitor.Visit(aStat);
      }
    }

    /// <summary>
    /// Makes a copy of this local declaration, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override LocalDeclaration MakeCopyFor(LocalDeclarationsStatement containingLocalDeclarationsStatement)
      //^^ ensures result.GetType() == this.GetType();
    {
      if (this.ContainingLocalDeclarationsStatement == containingLocalDeclarationsStatement) return this;
      return new VccLocalDeclaration(containingLocalDeclarationsStatement, this);
    }

    protected override LocalDefinition CreateLocalDefinition() {
      return new VccLocalDefinition(this, this.specifiers, this.isSpec);
    }

    private readonly bool isSpec;

    public bool IsSpec {
      get { return this.isSpec; }
    }

    /// <summary>
    /// The type of the local.
    /// </summary>
    public override ITypeDefinition Type {
      [DebuggerNonUserCode]
      get {
        if (this.type == null) {
          ITypeDefinition result;
          VccArrayTypeExpression/*?*/ arrayTypeExpression = this.ContainingLocalDeclarationsStatement.TypeExpression as VccArrayTypeExpression;
          if (arrayTypeExpression != null && arrayTypeExpression.Size != null) {
            if (this.IsSpec)
              result = ((VccCompilationHelper)this.Helper).MakeSpecPointer(arrayTypeExpression.ElementType.ResolvedType);
            else
              result = PointerType.GetPointerType(arrayTypeExpression.ElementType.ResolvedType, this.Helper.Compilation.HostEnvironment.InternFactory);
          } else
            result = this.ContainingLocalDeclarationsStatement.Type;
          this.type = result;
        }
        return this.type;
      }
    }
    //^ [Once]
    ITypeDefinition/*?*/ type;

    private readonly List<Specifier> specifiers;

    #region ILocalDeclarationStatement Members

    IExpression/*?*/ ILocalDeclarationStatement.InitialValue {
      get {
        if (this.InitialValue == null) return null;
        return this.ConvertedInitialValue.ProjectAsIExpression();
      }
    }

    ILocalDefinition ILocalDeclarationStatement.LocalVariable {
      get { return this.LocalVariable; }
    }

    #endregion

  }

  // A local declaration of a function. It is the same as a VccLocalDeclaration except that
  // it contains a pointer to a toplevel mangled function declaration, which is a hack to help resolve
  // this function. 
  // The mangled function declaration should be created during parsing, when a local declaration turns out
  // to be a function type. 
  internal class VccLocalFunctionDeclaration : VccLocalDeclaration {

    internal VccLocalFunctionDeclaration(NameDeclaration name, Expression/*?*/ initialValue, List<Specifier> specifiers, bool isSpec, ISourceLocation sourceLocation, FunctionDeclaration mangledFunctionDeclaration)
      : base (name, initialValue, specifiers, isSpec, sourceLocation) {
      this.mangledFunctionDeclaration = mangledFunctionDeclaration;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template, except for its containing block.
    /// </summary>
    /// <param name="containingLocalDeclarationsStatement">The containing statement. This should be different from the containing statement of the template declaration.</param>
    /// <param name="template">The statement to copy.</param>
    protected VccLocalFunctionDeclaration(LocalDeclarationsStatement containingLocalDeclarationsStatement, VccLocalFunctionDeclaration template)
      : base(containingLocalDeclarationsStatement, template) {
      this.mangledFunctionDeclaration = template.mangledFunctionDeclaration;
    }

    /// <summary>
    /// Calls the visitor.Visit(IExpressionStatement) method on an assignment statement that initializes the local. 
    /// </summary>
    public override void Dispatch(ICodeVisitor visitor) {
    }

    //^ [MustOverride]
    public override LocalDeclaration MakeCopyFor(LocalDeclarationsStatement containingLocalDeclarationsStatement) {
      if (this.ContainingLocalDeclarationsStatement == containingLocalDeclarationsStatement) return this;
      else return new VccLocalFunctionDeclaration(containingLocalDeclarationsStatement, this);
    }

    public FunctionDeclaration MangledFunctionDeclaration {
      get { return this.mangledFunctionDeclaration; }
    }
    readonly FunctionDeclaration mangledFunctionDeclaration;

  }

  /// <summary>
  /// A special kind of block introduced by the parser for atomic_op. It is used when infering the type
  /// of the VccReturnValue expression.
  /// </summary>
  public class VccAtomicOpBlock : BlockStatement
  {
    public VccAtomicOpBlock(List<Statement> statements, VccAtomicOp atomicOp, ISourceLocation sourceLocation)
      : base(statements, sourceLocation) {
      this.atomicOp = atomicOp;
    }

    protected VccAtomicOpBlock(BlockStatement containingBlock, VccAtomicOpBlock template)
      : base(containingBlock, template) {
      this.atomicOp = template.atomicOp;
    }

    private readonly VccAtomicOp atomicOp;

    public VccAtomicOp AtomicOp {
      get { return this.atomicOp; }
    }

    /// <summary>
    /// Makes a copy of this statement, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Statement MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
    {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccAtomicOpBlock(containingBlock, this);
    }
  }

  /// <summary>
  /// A special kind of block introduced by the parser for blocks with associated contracts.
  /// It is aware of these contracts and includes these contracts into the SetContainingBlock
  /// operation.
  /// </summary>
  public sealed class VccBlockWithContracts : BlockStatement
  {
    public VccBlockWithContracts(List<Statement> statements, ISourceLocation sourceLocation)
      : base(statements, sourceLocation) {
    }

    private VccBlockWithContracts(BlockStatement containingBlock, VccBlockWithContracts template)
      : base(containingBlock, template) {
    }

    private void SetContainingBlockForContracts() {
      MethodContract mc = this.Compilation.ContractProvider.GetMethodContractFor(this) as MethodContract;
      if (mc != null) mc.SetContainingBlock(this);
    }

    public override void SetContainers(BlockStatement containingBlock, ISignatureDeclaration containingSignatureDeclaration) {
      base.SetContainers(containingBlock, containingSignatureDeclaration);
      this.SetContainingBlockForContracts();
    }

    public override void SetContainers(BlockStatement containingBlock, NamespaceDeclaration containingNamespaceDeclaration) {
      base.SetContainers(containingBlock, containingNamespaceDeclaration);
      this.SetContainingBlockForContracts();
    }

    public override void SetContainers(BlockStatement containingBlock, TypeDeclaration containingTypeDeclaration) {
      base.SetContainers(containingBlock, containingTypeDeclaration);
      this.SetContainingBlockForContracts();
    }

    public override void SetContainingBlock(BlockStatement containingBlock) {
      base.SetContainingBlock(containingBlock);
      this.SetContainingBlockForContracts();
    }

    /// <summary>
    /// Makes a copy of this statement, changing the ContainingBlock to the given block.
    /// </summary>
    //^ [MustOverride]
    public override Statement MakeCopyFor(BlockStatement containingBlock)
      //^^ ensures result.GetType() == this.GetType();
    {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccBlockWithContracts(containingBlock, this);
    }
  }

  public sealed class VccWrapStatement : Statement, IVccWrapStatement
  {
    public VccWrapStatement(Expression expr, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.expression = expr;
    }

    private VccWrapStatement(BlockStatement containingBlock, VccWrapStatement template)
      : base(containingBlock, template) {
      this.expression = template.expression.MakeCopyFor(containingBlock);
    }

    public Expression Expression {
      get { return this.expression; }
    }
    private readonly Expression expression;

    IExpression IVccWrapStatement.Object {
      get { return this.expression.ProjectAsIExpression(); }
    }

    public void Dispatch(IVccCodeVisitor visitor) {
      visitor.Visit(this);
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      return this.Expression.HasErrors || this.Expression.HasSideEffect(true);
    }

    public override void SetContainingBlock(BlockStatement containingBlock) {
      base.SetContainingBlock(containingBlock);
      DummyExpression containingExpression = new DummyExpression(containingBlock, SourceDummy.SourceLocation);
      this.expression.SetContainingExpression(containingExpression);
    }

    public override Statement MakeCopyFor(BlockStatement containingBlock) {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccWrapStatement(containingBlock, this);
    }

  }

  public sealed class VccUnwrapStatement : Statement, IVccUnwrapStatement
  {
    public VccUnwrapStatement(Expression expr, ISourceLocation sourceLocation)
      : base(sourceLocation) {
      this.expression = expr;
    }

    private VccUnwrapStatement(BlockStatement containingBlock, VccUnwrapStatement template)
      : base(containingBlock, template) {
      this.expression = template.expression.MakeCopyFor(containingBlock);
    }

    public Expression Expression {
      get { return this.expression; }
    }
    private readonly Expression expression;

    IExpression IVccUnwrapStatement.Object {
      get { return this.expression.ProjectAsIExpression(); }
    }

    public void Dispatch(IVccCodeVisitor visitor) {
      visitor.Visit(this);
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      return this.Expression.HasErrors || this.Expression.HasSideEffect(true);
    }

    public override void SetContainingBlock(BlockStatement containingBlock) {
      base.SetContainingBlock(containingBlock);
      DummyExpression containingExpression = new DummyExpression(containingBlock, SourceDummy.SourceLocation);
      this.expression.SetContainingExpression(containingExpression);
    }

    public override Statement MakeCopyFor(BlockStatement containingBlock) {
      if (this.ContainingBlock == containingBlock) return this;
      return new VccUnwrapStatement(containingBlock, this);
    }
  }

  public class VccAssertStatement : AssertStatement
  {
    public VccAssertStatement(Expression condition, ISourceLocation sourceLocation)
      : base(condition, sourceLocation) {
    }

    public VccAssertStatement(BlockStatement containingBlock, VccAssertStatement template) 
      : base(containingBlock, template) {
    }

    private void CopyTriggersFromTemplate(VccAssertStatement template) {
      IEnumerable<IEnumerable<Expression>>/*?*/ triggers = template.Compilation.ContractProvider.GetTriggersFor(template);
      if (triggers != null) {
        IEnumerable<IEnumerable<Expression>> copiedTriggers = this.CopyTriggers(triggers, this.ContainingBlock);
        this.Compilation.ContractProvider.AssociateTriggersWithQuantifier(this, copiedTriggers);
      }
    }

    private IEnumerable<IEnumerable<Expression>> CopyTriggers(IEnumerable<IEnumerable<Expression>> triggers, BlockStatement containingBlock) {
      List<IEnumerable<Expression>> copiedTriggers = new List<IEnumerable<Expression>>();
      foreach (IEnumerable<Expression> trigger in triggers) {
        List<Expression> copiedTrigger = new List<Expression>();
        foreach (Expression e in trigger) {
          copiedTrigger.Add(e.MakeCopyFor(containingBlock));
        }
        copiedTriggers.Add(copiedTrigger.AsReadOnly());
      }
      return copiedTriggers.AsReadOnly();
    }

    public override void SetContainingBlock(BlockStatement containingBlock) {
      base.SetContainingBlock(containingBlock);
      IEnumerable<IEnumerable<Expression>>/*?*/ triggers = this.Compilation.ContractProvider.GetTriggersFor(this);
      if (triggers != null) {
        Expression dummyExpression = new DummyExpression(containingBlock, this.SourceLocation);
        foreach (IEnumerable<Expression> trigger in triggers)
          foreach (Expression e in trigger)
            e.SetContainingExpression(dummyExpression);
      }
    }

    public override Statement MakeCopyFor(BlockStatement containingBlock) {
      if (this.ContainingBlock == containingBlock) return this;
      var result = new VccAssertStatement(containingBlock, this);
      result.CopyTriggersFromTemplate(this);
      return result;
    }
  }

  public sealed class VccSpecStatement : Statement, IVccSpecStatement
  {
    private readonly Statement wrappedStatement;

    public VccSpecStatement(Statement statement, ISourceLocation sourceLocation)
      : base(sourceLocation) { 
      this.wrappedStatement = statement;
    }

    private VccSpecStatement(BlockStatement containingBlock, VccSpecStatement template)
      : base(containingBlock, template) {
      this.wrappedStatement = template.wrappedStatement.MakeCopyFor(containingBlock);
    }

    public Statement WrappedStatement {
      get { return this.wrappedStatement; }
    }

    IStatement IVccSpecStatement.WrappedStatement {
      get { return this.wrappedStatement; }
    }

    public override void Dispatch(ICodeVisitor visitor) {
      this.WrappedStatement.Dispatch(visitor);
    }

    public  void Dispatch(IVccCodeVisitor visitor) {
      visitor.Visit(this);
    }

    public override void Dispatch(SourceVisitor visitor) {
      this.WrappedStatement.Dispatch(visitor);
    }

    public override void SetContainingBlock(BlockStatement containingBlock) {
      base.SetContainingBlock(containingBlock);
      this.WrappedStatement.SetContainingBlock(containingBlock);
    }

    public override Statement MakeCopyFor(BlockStatement containingBlock) {
      if (containingBlock == this.ContainingBlock) return this;
      return new VccSpecStatement(containingBlock, this);
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound() {
      return this.WrappedStatement.HasErrors;
    }
  }
}