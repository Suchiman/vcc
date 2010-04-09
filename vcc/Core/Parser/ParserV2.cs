//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using Microsoft.Cci;
using Microsoft.Cci.Ast;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc.Parsing
{

  internal class ParserV2 : Parser
  {
    readonly IName ResultKeyword;
    readonly IName OldKeyword;

    internal ParserV2(Compilation compilation, ISourceLocation sourceLocation, List<IErrorMessage> scannerAndParserErrors) 
    : base(compilation, sourceLocation, scannerAndParserErrors) {
      this.ResultKeyword = this.compilation.NameTable.GetNameFor("\\result");
      this.OldKeyword = this.compilation.NameTable.GetNameFor("\\old");
    }

    override protected bool TryToGetBuiltInSpecTypeName(TypedefNameSpecifier typedefName, out TypeExpression result) {
      switch (typedefName.TypedefName.ToString()) {
        case "\\object":
          Expression typePtrRef = NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.nameTable, "TypedPtr");
          result = new VccNamedTypeExpression(typePtrRef);
          return true;
        case "\\integer":
          Expression bigIntRef = NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.nameTable, "BigInt");
          result = new VccNamedTypeExpression(bigIntRef);
          return true;
        default:
          result = null;
          return false;
      }
    }

    override protected void ParseTypeMemberDeclarationList(List<INamespaceDeclarationMember> namespaceMembers, List<ITypeDeclarationMember> typeMembers, TokenSet followers) {
      TokenSet expectedTokens = TS.DeclarationStart | Token.Colon;
      while (expectedTokens[this.currentToken]) {
        if (this.currentToken == Token.Specification) {
          this.ParseTypeSpecMemberDeclarationList(namespaceMembers, typeMembers, followers);
        } else {
          this.ParseNonLocalDeclaration(namespaceMembers, typeMembers, followers, false);
        }
      }
    }

    new protected void ParseTypeSpecMemberDeclarationList(List<INamespaceDeclarationMember> namespaceMembers, List<ITypeDeclarationMember> typeMembers, TokenSet followers) {
      SpecTokenSet expectedSpecTokens = new SpecTokenSet(SpecToken.Ghost, SpecToken.Invariant);
      bool savedInSpecCode = this.EnterSpecBlock();
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      while (expectedSpecTokens[this.CurrentSpecToken]) {
        switch (this.CurrentSpecToken) {
          case SpecToken.Invariant:
            this.ParseTypeInvariant(followers | Token.RightParenthesis | Token.Identifier);
            break;
          case SpecToken.Ghost:
            this.ParseNonLocalDeclaration(namespaceMembers, typeMembers, followers | Token.RightParenthesis | Token.Identifier, false);
            break;
        }
        if (this.currentToken == Token.Semicolon) {
          this.SkipOverTo(Token.Semicolon, followers);
          continue;
        }
        break;
      }
      this.SkipOverTo(Token.RightParenthesis, followers);
      this.LeaveSpecBlock(savedInSpecCode);
    }

    new protected void ParseTypeInvariant(TokenSet followers) {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      NameDeclaration nameDecl = null;
      // TODO: labeled expressions
      this.GetNextToken();
      Expression condition = this.ParseExpression(followers);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      TypeInvariant typeInvariant = new TypeInvariant(nameDecl, new CheckedExpression(condition, condition.SourceLocation), false, slb);
      this.AddTypeInvariantToCurrent(typeInvariant);
    }

    override protected void ParseFunctionOrBlockContract(FunctionOrBlockContract contract, TokenSet followers) {
      SpecTokenSet expectedSpecTokens = new SpecTokenSet(SpecToken.Ensures, SpecToken.Maintains, SpecToken.Reads, SpecToken.Requires, SpecToken.Writes);
      while (this.currentToken == Token.Specification) {
        bool savedInSpecCode = this.EnterSpecBlock();
        this.GetNextToken();
        this.Skip(Token.LeftParenthesis);
        while (expectedSpecTokens[this.CurrentSpecToken]) {
          switch (this.CurrentSpecToken) {
            case SpecToken.Requires:
              this.GetNextToken();
              var precond = this.ParseExpression(followers | Token.RightParenthesis);
              precond = this.CheckedExpressionIfRequested(precond);
              contract.AddPrecondition(new Precondition(precond, null, precond.SourceLocation));
              break;
            case SpecToken.Ensures:
              this.GetNextToken();
              this.resultIsAKeyword = true;
              var postcond = this.ParseExpression(followers | Token.RightParenthesis);
              this.resultIsAKeyword = false;
              postcond = this.CheckedExpressionIfRequested(postcond);
              contract.AddPostcondition(new Postcondition(postcond, postcond.SourceLocation));
              break;
            case SpecToken.Maintains:
              this.GetNextToken();
              var inv = this.ParseExpression(followers | Token.RightParenthesis);
              inv = this.CheckedExpressionIfRequested(inv);
              contract.AddPrecondition(new Precondition(inv, null, inv.SourceLocation));
              contract.AddPostcondition(new Postcondition(inv, inv.SourceLocation));
              break;
            case SpecToken.Writes:
              this.GetNextToken();
              var writes = this.ParseExpressionList(Token.Comma, followers | Token.RightParenthesis);
              contract.AddWrites(writes);
              break;
            case SpecToken.Reads:
              this.GetNextToken();
              var reads = this.ParseExpressionList(Token.Comma, followers | Token.RightParenthesis);
              contract.AddWrites(reads);
              break;
          }
        }
        this.SkipOverTo(Token.RightParenthesis, followers | Token.Specification);
      }
    }

    private Statement ParseSingleArgSpecStatement(TokenSet followers, Func<Expression, ISourceLocation, Statement> createStmt) {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      var expr = this.ParseExpression(followers);
      slb.UpdateToSpan(expr.SourceLocation);
      return createStmt(expr, slb);
    }

    override protected Statement ParseSpecStatements(TokenSet followers) {
      bool savedInSpecCode = this.EnterSpecBlock();
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      List<Statement> statements = new List<Statement>();
      SpecTokenSet expectedSpecTokens = new SpecTokenSet(SpecToken.Wrap, SpecToken.Unwrap, SpecToken.Ghost, SpecToken.Assert, SpecToken.Assume);
      TokenSet followersOrRightParen = followers | Token.RightParenthesis;
      while (expectedSpecTokens[this.CurrentSpecToken]) {
        switch (this.CurrentSpecToken) {
          case SpecToken.Ghost:
            SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
            this.GetNextToken();
            var stmt = this.ParseStatement(followers | Token.RightParenthesis);
            slb.UpdateToSpan(stmt.SourceLocation);
            statements.Add(new VccSpecStatement(stmt, slb));
            break;
          case SpecToken.Wrap:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new VccWrapStatement(expr, sl)));
            break;
          case SpecToken.Unwrap:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new VccUnwrapStatement(expr, sl)));
            break;
          case SpecToken.Assert:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new AssertStatement(expr, sl)));
            break;
          case SpecToken.Assume:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new AssumeStatement(expr, sl)));
            break;
        }
        if (this.currentToken == Token.Semicolon)
          this.SkipOverTo(Token.Semicolon, followersOrRightParen | Token.Identifier);
      }
      this.LeaveSpecBlock(savedInSpecCode);
      this.SkipOverTo(Token.RightParenthesis, followers);
      return new StatementGroup(statements);
    }

    protected override bool TryParseSpecialExpression(SimpleName name, TokenSet followers, out Expression expression) {
      if (name.Name.UniqueKey == this.ResultKeyword.UniqueKey) {
        expression = new ReturnValue(name.SourceLocation);
        return true;
      } else if (name.Name.UniqueKey == this.OldKeyword.UniqueKey) {
        SourceLocationBuilder slb = new SourceLocationBuilder(name.SourceLocation);
        this.Skip(Token.LeftParenthesis);
        Expression expr = this.ParseExpression(followers | Token.RightParenthesis);
        slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        expression = new OldValue(expr, slb);
        this.SkipOverTo(Token.RightParenthesis, followers);
        return true;
      }
      expression = null;
      return false;
    }

    private SpecToken CurrentSpecToken {
      get {
        if (this.currentToken == Token.Identifier) {
          switch (this.scanner.GetIdentifierString()) {
            case "assert":
              return SpecToken.Assert;
            case "assume":
              return SpecToken.Assume;
            case "ensures":
              return SpecToken.Ensures;
            case "ghost":
              return SpecToken.Ghost;
            case "invariant":
              return SpecToken.Invariant;
            case "maintains":
              return SpecToken.Maintains;
            case "unwrap":
              return SpecToken.Unwrap;
            case "reads":
              return SpecToken.Reads;
            case "requires":
              return SpecToken.Requires;
            case "wrap":
              return SpecToken.Wrap;
            case "writes":
              return SpecToken.Writes;
            default:
              return SpecToken.None;
          }
        }
        return SpecToken.None;
      }
    }

    private enum SpecToken
    {
      None,
      Assert,
      Assume,
      Ensures,
      Ghost,
      Invariant,
      Maintains,
      Reads,
      Requires,
      Unwrap,
      Wrap,
      Writes,
    }

    private struct SpecTokenSet
    {
      ulong bits;

      public SpecTokenSet(SpecToken t1, SpecToken t2) {
        this.bits = 0;
        this.bits |= (1ul << (int)t1);
        this.bits |= (1ul << (int)t2);
      }

      public SpecTokenSet(SpecToken t1, SpecToken t2, SpecToken t3) {
        this.bits = 0;
        this.bits |= (1ul << (int)t1);
        this.bits |= (1ul << (int)t2);
        this.bits |= (1ul << (int)t3);
      }

      public SpecTokenSet(SpecToken t1, SpecToken t2, SpecToken t3, SpecToken t4) {
        this.bits = 0;
        this.bits |= (1ul << (int)t1);
        this.bits |= (1ul << (int)t2);
        this.bits |= (1ul << (int)t3);
        this.bits |= (1ul << (int)t4);
      }

      public SpecTokenSet(SpecToken t1, SpecToken t2, SpecToken t3, SpecToken t4, SpecToken t5) {
        this.bits = 0;
        this.bits |= (1ul << (int)t1);
        this.bits |= (1ul << (int)t2);
        this.bits |= (1ul << (int)t3);
        this.bits |= (1ul << (int)t4);
        this.bits |= (1ul << (int)t5);
      }

      [System.Diagnostics.DebuggerNonUserCode]
      public static SpecTokenSet operator |(SpecTokenSet ts, SpecToken t) {
        SpecTokenSet result = new SpecTokenSet();
        int i = (int)t;
        result.bits = ts.bits | (1ul << i);
        return result;
      }

      [System.Diagnostics.DebuggerNonUserCode]
      public static SpecTokenSet operator |(SpecTokenSet ts1, SpecTokenSet ts2) {
        SpecTokenSet result = new SpecTokenSet();
        result.bits = ts1.bits | ts2.bits;
        return result;
      }

      internal bool this[SpecToken t] {
        get {
          int i = (int)t;
          return (this.bits & (1ul << i)) != 0;
        }
      }
    }
  }
}