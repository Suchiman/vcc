//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using Microsoft.Cci.Contracts;
using Microsoft.Cci.Ast;
using System.Diagnostics;
using Microsoft.Cci;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc {

  /// <summary>
  /// Represents a global method in symbol table.
  /// </summary>
  public class VccGlobalMethodDefinition : GlobalMethodDefinition, ISourceItem, ISpecItem {

    /// <summary>
    /// Allocates a global method definition to correspond to a given global method declaration.
    /// </summary>
    /// <param name="functionDefinition">The global method declaration that corresponds to the definition being allocated.</param>
    internal VccGlobalMethodDefinition(FunctionDefinition functionDefinition)
      : base(functionDefinition)
    {
      this.sourceLocation = functionDefinition.SourceLocation;
    }

    /// <summary>
    /// The parameters of this method.
    /// </summary>
    public override IEnumerable<ParameterDefinition> Parameters {
      get {
        //^ assume this.GlobalMethodDeclaration is FunctionDefinition; //the constructor assures this
        FunctionDefinition functionDefinition = (FunctionDefinition)this.GlobalMethodDeclaration;
        if (functionDefinition.HasSingleVoidParameter) return IteratorHelper.GetEmptyEnumerable<ParameterDefinition>();
        return base.Parameters;
      }
    }

    public IEnumerable<FunctionDeclaration> Declarations
    {
      get
      {
        FunctionDefinition functionDefinition = (FunctionDefinition)this.GlobalMethodDeclaration;
        // copy global member list because it may change
        List<ITypeDeclarationMember> members = new List<ITypeDeclarationMember>(functionDefinition.CompilationPart.GlobalDeclarationContainer.GlobalMembers);
        foreach (ITypeDeclarationMember member in members) {
          FunctionDeclaration decl = member as FunctionDeclaration;
          if (decl != null)
          {
            if (decl.Name.Name == this.Name) yield return decl;
          }
        }
      }
    }

    public bool IsSpec {
      get {
        return ((FunctionDefinition)this.GlobalMethodDeclaration).IsSpec;
      }
    }

    #region ISourceItem Members

    private readonly ISourceLocation sourceLocation;

    public ISourceLocation SourceLocation
    {
      get { return this.sourceLocation; }
    }

    #endregion
  }
}