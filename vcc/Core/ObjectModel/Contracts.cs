//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System.Collections.Generic;
using System.Resources;
using Microsoft.Cci.Ast;
using Microsoft.Research.Vcc.Preprocessing;
using ITypeInvariant=Microsoft.Cci.Contracts.ITypeInvariant;
using Microsoft.Cci;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc {

  /// <summary>
  /// A collection of collections of objects that augment the signature of a type with additional information
  /// that describe invariants, model variables and functions, as well as axioms.
  /// </summary>
  // ^ [Immutable]
  public sealed class VccTypeContract : Contract, Microsoft.Cci.Contracts.ITypeContract {

    /// <summary>
    /// Allocates a collection of collections of objects that augment the signature of a type with additional information
    /// that describe invariants, model variables and functions, as well as axioms.
    /// </summary>
    /// <param name="contractFields">A possibly empty list of contract fields. Contract fields can only be used inside contracts and are not available at runtime.</param>
    /// <param name="contractFunctions">A possibly empty list of contract functions. Contract functions have no bodies and can only be used inside contracts. The meaning of a contract
    /// functions is specified by the axioms (assumed invariants) of the associated type. Contract functions are not available at runtime.</param>
    /// <param name="invariants">A possibly empty list of type invariants. Axioms are a special type of invariant.</param>
    public VccTypeContract(IEnumerable<FieldDeclaration>/*?*/ contractFields, IEnumerable<FunctionDeclaration>/*?*/ contractFunctions, IEnumerable<TypeInvariant>/*?*/ invariants) {
      this.contractFields = contractFields==null ? EmptyListOfFields:contractFields;
      this.contractFunctions = contractFunctions==null ? EmptyListOfMethods:contractFunctions;
      this.invariants = invariants==null ? EmptyListOfInvariants:invariants;
    }

    protected override bool CheckForErrorsAndReturnTrueIfAnyAreFound()
    {
      bool result = false;
      foreach (ITypeInvariant inv in this.Invariants)
        result |= inv.HasErrors();
      return result;
    }

    /// <summary>
    /// A copy constructor that allocates an instance that is the same as the given template.
    /// </summary>
    /// <param name="template">The template to copy.</param>
    private VccTypeContract(VccTypeContract template) {
      if (template.contractFields != EmptyListOfFields)
        this.contractFields = new List<FieldDeclaration>(template.contractFields);
      else
        this.contractFields = template.contractFields;
      if (template.contractFunctions != EmptyListOfMethods)
        this.contractFunctions = new List<FunctionDeclaration>(template.contractFunctions);
      else
        this.contractFunctions = template.contractFunctions;
      if (template.invariants != EmptyListOfInvariants)
        this.invariants = new List<TypeInvariant>(template.invariants);
      else
        this.invariants = template.invariants;
    }

    /// <summary>
    /// The type declaration that contains the type contract.
    /// </summary>
    TypeDeclaration/*?*/ containingType;

    /// <summary>
    /// A possibly empty list of contract fields. Contract fields can only be used inside contracts and are not available at runtime.
    /// </summary>
    public IEnumerable<IFieldDefinition> ContractFields {
      get {
        foreach (FieldDeclaration fieldDecl in this.contractFields) yield return fieldDecl.FieldDefinition;
      }
    }
    readonly IEnumerable<FieldDeclaration> contractFields;

    /// <summary>
    /// A possibly empty list of contract methods. Contract methods have no bodies and can only be used inside contracts. The meaning of a contract
    /// method is specified by the axioms (assumed invariants) of the associated type. Contract methods are not available at runtime.
    /// </summary>
    public IEnumerable<IMethodDefinition> ContractMethods {
      get {
        foreach (FunctionDeclaration fdecl in this.contractFunctions)
          if (fdecl.ResolvedMethod != null)
            yield return fdecl.ResolvedMethod;
      }
    }
    readonly IEnumerable<FunctionDeclaration> contractFunctions;

    private static readonly IEnumerable<FieldDeclaration> EmptyListOfFields = IteratorHelper.GetEmptyEnumerable<FieldDeclaration>();
    private static readonly IEnumerable<FunctionDeclaration> EmptyListOfMethods = IteratorHelper.GetEmptyEnumerable<FunctionDeclaration>();
    private static readonly IEnumerable<TypeInvariant> EmptyListOfInvariants = IteratorHelper.GetEmptyEnumerable<TypeInvariant>();

    private IEnumerable<ITypeInvariant> GetInvariantsAndAxiomsAboutConstantArraysAndStructs() {
      VccGlobalDeclarationContainerClass/*?*/ globals = this.containingType as VccGlobalDeclarationContainerClass;
      if (globals == null)
        return IteratorHelper.GetConversionEnumerable<TypeInvariant, ITypeInvariant>(this.invariants);
      List<ITypeInvariant> result = new List<ITypeInvariant>();
      foreach (TypeInvariant tinv in this.invariants) result.Add(tinv);
      List<ITypeDeclarationMember> globalMembers = (List<ITypeDeclarationMember>)globals.GlobalMembers;
      for (int i = 0; i < globalMembers.Count; i++){
        ITypeDeclarationMember member = globalMembers[i];
        GlobalVariableDeclaration/*?*/ globalVar = member as GlobalVariableDeclaration;
        if (globalVar == null) continue;

        VccOptions opts = globalVar.Compilation.Options as VccOptions;
        if (opts != null && opts.Vcc2) continue;

        if (globalVar.IsMapped) {
          VccInitializer/*?*/ initializer = globalVar.Initializer as VccInitializer;
          VccArrayTypeExpression/*?*/ arrayType = globalVar.Type as VccArrayTypeExpression;
          if (initializer != null && arrayType != null) {
            Expression array = new AddressOf(new AddressableExpression(new BoundExpression(initializer, globalVar.FieldDefinition)), initializer.SourceLocation);
            array.SetContainingExpression(initializer);
            array = globalVar.Helper.ExplicitConversion(array, PointerType.GetPointerType(arrayType.ElementType.ResolvedType, globalVar.Compilation.HostEnvironment.InternFactory));
            IName iname = initializer.ContainingBlock.Helper.NameTable.GetNameFor("i");
            int ival = 0;
            foreach (Expression initialVal in initializer.Expressions) {
              CompileTimeConstant index = new CompileTimeConstant(ival++, true, SourceDummy.SourceLocation);
              LocalDeclaration locDecl = new LocalDeclaration(false, false, new NameDeclaration(iname, SourceDummy.SourceLocation), index, SourceDummy.SourceLocation);
              List<LocalDeclaration> locDecls = new List<LocalDeclaration>(1); locDecls.Add(locDecl);
              TypeExpression intT = TypeExpression.For(globalVar.PlatformType.SystemUInt64.ResolvedType);
              LocalDeclarationsStatement loc = new LocalDeclarationsStatement(false, false, true, intT, locDecls, SourceDummy.SourceLocation);
              List<LocalDeclarationsStatement> boundVars = new List<LocalDeclarationsStatement>(1); boundVars.Add(loc);
              SimpleName indexVar = new SimpleName(iname, SourceDummy.SourceLocation, false);
              Indexer indexer = new Indexer(array, IteratorHelper.GetSingletonEnumerable<Expression>(indexVar), SourceDummy.SourceLocation);
              Equality indexVarEq = new Equality(indexVar, index, initialVal.SourceLocation);
              Equality initialValEq = new Equality(indexer, initialVal, initialVal.SourceLocation);
              Implies impl = new Implies(indexVarEq, initialValEq, initialVal.SourceLocation);
              Forall forall = new Forall(boundVars, impl, initialVal.SourceLocation);
              TypeInvariant axiom = new TypeInvariant(null, forall, true, initialVal.SourceLocation);
              axiom.SetContainingExpression(initializer);
              result.Add(axiom);
            }
          }
        }
      }
      return result.AsReadOnly();
    }

    /// <summary>
    /// A possibly empty list of type invariants. Axioms are a special type of invariant.
    /// </summary>
    public IEnumerable<ITypeInvariant> Invariants {
      get {
        if (this.invariantsAndAxiomsAboutConstantArraysAndStructs == null)
          this.invariantsAndAxiomsAboutConstantArraysAndStructs = this.GetInvariantsAndAxiomsAboutConstantArraysAndStructs();
        return this.invariantsAndAxiomsAboutConstantArraysAndStructs;
      }
    }
    readonly IEnumerable<TypeInvariant> invariants;
    IEnumerable<ITypeInvariant>/*?*/ invariantsAndAxiomsAboutConstantArraysAndStructs;

    /// <summary>
    /// Makes a copy of this contract, changing the containing block to the given block.
    /// </summary>
    public VccTypeContract MakeCopyFor(TypeDeclaration containingType) {
      if (this.containingType == containingType) return this;
      VccTypeContract result = new VccTypeContract(this);
      result.SetContainingType(containingType);
      return result;
    }

    /// <summary>
    /// Completes the two stage construction of this object. This allows bottom up parsers to construct an Expression before constructing the containing Expression.
    /// This method should be called once only and must be called before this object is made available to client code. The construction code itself should also take
    /// care not to call any other methods or property/event accessors on the object until after this method has been called.
    /// </summary>
    public void SetContainingType(TypeDeclaration containingType) {
      this.containingType = containingType;
      Expression containingExpression = new DummyExpression(containingType.DummyBlock, SourceDummy.SourceLocation);
      foreach (FieldDeclaration contractField in this.contractFields)
        contractField.SetContainingTypeDeclaration(containingType, true);
      foreach (FunctionDeclaration contractFunction in this.contractFunctions)
        contractFunction.SetContainingTypeDeclaration(containingType);
      foreach (TypeInvariant typeInvariant in this.invariants)
        typeInvariant.SetContainingExpression(containingExpression);
    }

    #region IObjectWithLocations Members

    IEnumerable<ILocation> IObjectWithLocations.Locations {
      get {
        foreach (var f in this.contractFields)
          foreach (var loc in f.Locations) yield return loc;
        foreach (var m in this.contractFunctions)
          foreach (var loc in m.Locations) yield return loc;
        foreach (var i in this.invariants)
          foreach (var loc in i.Locations) yield return loc;
      }
    }

    #endregion
  }

}