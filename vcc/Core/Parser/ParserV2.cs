//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using Microsoft.Cci;
using Microsoft.Cci.Ast;

//^ using Microsoft.Contracts;

namespace Microsoft.Research.Vcc.Parsing
{

  internal class ParserV2 : Parser
  {
     internal ParserV2(Compilation compilation, ISourceLocation sourceLocation, List<IErrorMessage> scannerAndParserErrors) 
    : base(compilation, sourceLocation, scannerAndParserErrors) {
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
        case "\\objset":
          Expression objsetRef = NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.nameTable, "Objset");
          result = new VccNamedTypeExpression(objsetRef);
          return true;
        default:
          result = null;
          return false;
      }
    }

    override protected void ParseGlobalSpecDeclarationList(List<INamespaceDeclarationMember> members, List<ITypeDeclarationMember> globalMembers, TokenSet followers) {
      bool savedInSpecCode = this.SkipIntoSpecBlock();
      if (STS.SpecTypeModifiers[this.currentToken]) {
        this.ParseGlobalDeclarationWithSpecModifiers(members, globalMembers, followers, savedInSpecCode);
        return;
      }
      TokenSet followersOrDeclarationStart = followers | TS.DeclarationStart | Token.Semicolon | Token.RightParenthesis;
      while (TS.DeclarationStart[this.currentToken] || STS.Global[this.currentToken]) {
        switch (this.currentToken) {
          case Token.SpecAxiom:
            SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
            this.GetNextToken();
            var axiom = this.ParseExpression(followersOrDeclarationStart);
            slb.UpdateToSpan(axiom.SourceLocation);
            this.AddTypeInvariantToCurrent(new TypeInvariant(null, axiom, true, slb));
            break;
          case Token.SpecGhost:
            this.GetNextToken();
            this.ParseNonLocalDeclaration(members, globalMembers, followersOrDeclarationStart, true);
            break;
          case Token.SpecPure:
            List<Specifier> pureSpec = new List<Specifier>(1);
            pureSpec.Add(new SpecTokenSpecifier(Token.SpecPure, this.scanner.SourceLocationOfLastScannedToken));
            this.GetNextToken();
            this.ParseNonLocalDeclaration(members, globalMembers, followersOrDeclarationStart, true, pureSpec);
            break;
          default:
            this.ParseNonLocalDeclaration(members, globalMembers, followersOrDeclarationStart, true);
            break;
        }
        this.SkipSemicolonsInSpecBlock(TS.DeclarationStart | STS.Global | Token.RightParenthesis);
      }

      this.SkipOutOfSpecBlock(savedInSpecCode, followers);
    }


    protected override void SkipSemiColonAfterDeclarationOrStatement(TokenSet followers) {
      if (this.InSpecCode && this.currentToken == Token.RightParenthesis) {
        // do nothing
      } else this.SkipSemiColon(followers);

    }

    protected override bool ParseSpecTypeModifiers(List<Specifier> specifiers, TokenSet followers) {
      bool savedInSpecCode = this.SkipIntoSpecBlock();
      this.ParseSpecTypeModifierList(specifiers, followers);
      this.SkipOutOfSpecBlock(savedInSpecCode, followers);
      return true;
    }

    private void ParseSpecTypeModifier(List<Specifier> specifiers, TokenSet followers) {
      switch (this.currentToken) {
        case Token.SpecDynamicOwns:
        case Token.SpecClaimable:
        case Token.SpecVolatileOwns:
          specifiers.Add(new SpecTokenSpecifier(this.currentToken, this.scanner.SourceLocationOfLastScannedToken));
          this.GetNextToken();
          break;
        default:
          break;
      }
      this.SkipTo(followers);
    }

    private void ParseSpecTypeModifierList(List<Specifier> specifiers, TokenSet followers) {
      while (this.currentToken != Token.RightParenthesis) {
        this.ParseSpecTypeModifier(specifiers, followers | Token.Comma | Token.RightParenthesis);
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
    }

    private void ParseGlobalDeclarationWithSpecModifiers(List<INamespaceDeclarationMember> members, List<ITypeDeclarationMember> globalMembers, TokenSet followers, bool savedInSpecCode) {
      List<Specifier> specifiers = new List<Specifier>();
      this.ParseSpecTypeModifierList(specifiers, followers);
      this.SkipOutOfSpecBlock(savedInSpecCode, followers);
      this.ParseNonLocalDeclaration(members, globalMembers, followers, true, specifiers);
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
      while (this.currentToken == Token.Specification) {
        bool savedInSpecCode = this.SkipIntoSpecBlock();
        while (STS.TypeMember[this.currentToken]) {
          switch (this.currentToken) {
            case Token.SpecInvariant:
              this.ParseTypeInvariant(followers | Token.RightParenthesis);
              break;
            case Token.SpecGhost:
              this.GetNextToken();
              this.ParseNonLocalDeclaration(namespaceMembers, typeMembers, followers | Token.RightParenthesis, false);
              break;
          }
          this.SkipSemicolonsInSpecBlock(STS.TypeMember | Token.RightParenthesis);
        }
        this.SkipOutOfSpecBlock(savedInSpecCode, followers | Token.Specification);
      }
    }

    new protected void ParseTypeInvariant(TokenSet followers) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      NameDeclaration nameDecl = null;
      // TODO: labeled expressions
      this.GetNextToken();
      Expression condition = this.ParseExpression(followers);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      TypeInvariant typeInvariant = new TypeInvariant(nameDecl, new CheckedExpression(condition, condition.SourceLocation), false, slb);
      this.AddTypeInvariantToCurrent(typeInvariant);
    }

    protected override LoopContract ParseLoopContract(Parser.TokenSet followers) {
      List<LoopInvariant> invariants = new List<LoopInvariant>();
      List<Expression> writes = new List<Expression>();
      List<LoopVariant> variants = new List<LoopVariant>();
      while (this.currentToken == Token.Specification) {
        bool savedInSpecCode = this.SkipIntoSpecBlock();
        while (STS.LoopContract[this.currentToken]) {
          switch (this.currentToken) {
            case Token.SpecInvariant:
              SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
              this.GetNextToken();
              var inv = this.ParseExpression(followers | Token.RightParenthesis);
              slb.UpdateToSpan(inv.SourceLocation);
              invariants.Add(new LoopInvariant(inv, slb));
              break;
            case Token.SpecWrites:
              this.GetNextToken();
              this.ParseExpressionList(writes, Token.Comma, followers | Token.RightParenthesis);
              break;
            case Token.SpecDecreases:
              SourceLocationBuilder slb2 = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
              this.GetNextToken();
              var red = this.ParseExpression(followers | Token.RightParenthesis);
              slb2.UpdateToSpan(red.SourceLocation);
              variants.Add(new LoopVariant(red, slb2));
              break;
          }
          this.SkipSemicolonsInSpecBlock(STS.LoopContract | Token.RightParenthesis);
        }
        this.SkipOutOfSpecBlock(savedInSpecCode, followers | Token.Specification);
      }
      if (invariants.Count == 0 && writes.Count == 0 && variants.Count == 0) return null;
      return new LoopContract(invariants, writes, variants);
    }

    override protected void ParseFunctionOrBlockContract(FunctionOrBlockContract contract, TokenSet followers) {
      while (this.currentToken == Token.Specification) {
        bool savedInSpecCode = SkipIntoSpecBlock();
        while (STS.FunctionOrBlockContract[this.currentToken]) {
          switch (this.currentToken) {
            case Token.SpecRequires:
              this.GetNextToken();
              var precond = this.ParseExpression(followers | Token.RightParenthesis);
              precond = this.CheckedExpressionIfRequested(precond);
              contract.AddPrecondition(new Precondition(precond, null, precond.SourceLocation));
              break;
            case Token.SpecEnsures:
              this.GetNextToken();
              this.resultIsAKeyword = true;
              var postcond = this.ParseExpression(followers | Token.RightParenthesis);
              this.resultIsAKeyword = false;
              postcond = this.CheckedExpressionIfRequested(postcond);
              contract.AddPostcondition(new Postcondition(postcond, postcond.SourceLocation));
              break;
            case Token.SpecMaintains:
              this.GetNextToken();
              var inv = this.ParseExpression(followers | Token.RightParenthesis);
              inv = this.CheckedExpressionIfRequested(inv);
              contract.AddPrecondition(new Precondition(inv, null, inv.SourceLocation));
              contract.AddPostcondition(new Postcondition(inv, inv.SourceLocation));
              break;
            case Token.SpecWrites:
              this.GetNextToken();
              var writes = this.ParseExpressionList(Token.Comma, followers | Token.RightParenthesis);
              contract.AddWrites(writes);
              break;
            case Token.SpecDecreases:
              this.GetNextToken();
              var variant = this.ParseExpression(followers | Token.RightParenthesis);
              contract.AddMethodVariant(new MethodVariant(variant, variant.SourceLocation));
              break;
            case Token.SpecReads:
              this.GetNextToken();
              var reads = this.ParseExpressionList(Token.Comma, followers | Token.RightParenthesis);
              contract.AddReads(reads);
              break;
          }
          this.SkipSemicolonsInSpecBlock(STS.FunctionOrBlockContract | Token.RightParenthesis);
        }
        this.SkipOutOfSpecBlock(savedInSpecCode, followers | Token.Specification);
      }
    }

    protected override List<Parameter> ParseParameterList(Parser.TokenSet followers) {
      List<Parameter> result = new List<Parameter>();
      this.Skip(Token.LeftParenthesis);
      if (this.currentToken != Token.RightParenthesis) {
        TokenSet followersOrCommaOrRightParenthesisOrSpecification = followers | Token.Comma | Token.RightParenthesis | Token.Specification;
        while (this.currentToken != Token.RightParenthesis) {
          if (this.currentToken == Token.Specification) {
            result.Add(this.ParseSpecParameter(followers | Token.RightParenthesis));
            continue;
          }
          result.Add(this.ParseParameter(followersOrCommaOrRightParenthesisOrSpecification));
          if (this.currentToken == Token.Comma) {
            this.GetNextToken();
            continue;
          }
          if (this.currentToken == Token.Specification)
            continue;
          break;
        }
      }
      return result;
    }

    private Parameter ParseParameter(TokenSet followers, bool isOut, SourceLocationBuilder slb) {
      if (this.currentToken == Token.Range)
        return ParseVarArgsParameter(followers);

      List<Specifier> specifiers = this.ParseSpecifiers(null, null, null, followers | TS.DeclaratorStart);
      if (specifiers.Count > 0) slb.UpdateToSpan(specifiers[specifiers.Count - 1].SourceLocation);
      Declarator declarator = this.ParseDeclarator(followers);
      declarator = this.UseDeclaratorAsTypeDefNameIfThisSeemsIntended(specifiers, declarator, followers);
      slb.UpdateToSpan(declarator.SourceLocation);
      var result = new Parameter(specifiers, declarator, this.InSpecCode, isOut, slb);
      this.SkipTo(followers);
      return result;
    }

    private Parameter ParseSpecParameter(TokenSet followers) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      Parameter result;
      bool savedInSpecCode = this.SkipIntoSpecBlock();
      switch (this.currentToken) {
        case Token.SpecGhost:
          this.GetNextToken();
          result = this.ParseParameter(followers|Token.RightParenthesis, false, slb);
          break;
        case Token.SpecOut:
          this.GetNextToken();
          result = this.ParseParameter(followers|Token.RightParenthesis, true, slb);
          break;
        default:
          this.HandleError(Error.SyntaxError, "ghost or out");
          result = this.ParseParameter(followers | Token.RightParenthesis);
          break;
      }
      this.SkipOutOfSpecBlock(savedInSpecCode, followers);
      return result;
    }

    protected override List<Expression> ParseArgumentList(SourceLocationBuilder slb, Parser.TokenSet followers) {
      List<Expression> result = new List<Expression>();
      this.Skip(Token.LeftParenthesis);
      if (this.currentToken != Token.RightParenthesis) {
        while (this.currentToken != Token.RightParenthesis) {
          if (this.currentToken == Token.Specification) {
            result.Add(this.ParseSpecArgument(followers | Token.RightParenthesis));
            continue;
          }
          result.Add(this.ParseArgumentExpression(followers | Token.RightParenthesis));
          if (this.currentToken != Token.Comma) break;
          this.GetNextToken();
        }
      }
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.SkipOverTo(Token.RightParenthesis, followers);
      return result;
    }

    private Expression ParseSpecArgument(TokenSet followers) {
      Expression result;
      bool savedInSpecCode = this.SkipIntoSpecBlock();
      switch (this.currentToken) {
        case Token.SpecGhost:
          this.GetNextToken();
          result = this.ParseExpression(followers | Token.RightParenthesis);
          break;
        case Token.SpecOut:
          SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
          this.GetNextToken();
          var outArg = this.ParseExpression(followers | Token.RightParenthesis);
          slb.UpdateToSpan(outArg.SourceLocation);
          result = new VccOutArgument(new TargetExpression(outArg), slb);
          break;
        default:
          this.HandleError(Error.SyntaxError, "ghost or out");
          result = this.ParseExpression(followers | Token.RightParenthesis);
          break;
      }
      this.SkipOutOfSpecBlock(savedInSpecCode, followers);
      return result;
    }

    override protected Statement ParseSpecStatements(TokenSet followers) {
      bool savedInSpecCode = this.SkipIntoSpecBlock();
      if (this.currentToken == Token.SpecUnwrapping) {
        return this.ParseUnwrappingStatement(followers, savedInSpecCode);
      }

      if (this.currentToken == Token.SpecAtomic) {
        return this.ParseAtomic(followers, savedInSpecCode);
      }
      
      List<Statement> statements = new List<Statement>();
      TokenSet followersOrRightParen = followers | Token.RightParenthesis;
      SourceLocationBuilder slb;

      while (STS.SimpleSpecStatment[this.currentToken]) {
        switch (this.currentToken) {
          case Token.SpecGhost:
            slb = this.GetSourceLocationBuilderForLastScannedToken();
            this.GetNextToken();
            var stmt = this.ParseStatement(followers | Token.RightParenthesis);
            slb.UpdateToSpan(stmt.SourceLocation);
            LocalDeclarationsStatement localDecl = stmt as LocalDeclarationsStatement;
            if (localDecl != null)
              statements.Add(localDecl); // must not be wrapped
            else 
              statements.Add(new VccSpecStatement(stmt, slb));
            break;
          case Token.SpecWrap:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new VccWrapStatement(expr, sl)));
            break;
          case Token.SpecUnwrap:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new VccUnwrapStatement(expr, sl)));
            break;
          case Token.SpecAssert:
            statements.Add(this.ParseAssert(followersOrRightParen));
            break;
          case Token.SpecAssume:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new AssumeStatement(expr, sl)));
            break;
        }
        this.SkipSemicolonsInSpecBlock(STS.SimpleSpecStatment | Token.RightParenthesis);
      }
      this.SkipOutOfSpecBlock(savedInSpecCode, followers);
      return new StatementGroup(statements);
    }

    private Statement ParseAtomic(TokenSet followers, bool savedInSpecCode) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      var exprs = this.ParseExpressionList(Token.Comma, followers | Token.RightParenthesis);
      this.SkipOutOfSpecBlock(savedInSpecCode, TS.StatementStart | followers);
      var body = this.ParseStatement(followers);
      slb.UpdateToSpan(body.SourceLocation);
      return new VccAtomicStatement(body, exprs, slb);
    }

    private Statement ParseUnwrappingStatement(TokenSet followers, bool savedInSpecCode) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      var expr = this.ParseExpression(followers | Token.RightParenthesis);
      this.SkipOutOfSpecBlock(savedInSpecCode, TS.StatementStart | followers);
      var body = this.ParseStatement(followers);
      slb.UpdateToSpan(body.SourceLocation);
      return new VccUnwrappingStatement(body, expr, slb);
    }

    private AssertStatement ParseAssert(TokenSet followers) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      IEnumerable<IEnumerable<Expression>> triggers = null;
      if (this.currentToken == Token.LeftBrace)
        triggers = this.ParseQuantifierTriggers(followers | TS.UnaryStart);
      var condition = this.ParseExpression(followers | Token.RightParenthesis);
      slb.UpdateToSpan(condition.SourceLocation);
      var result = new VccAssertStatement(condition, slb);
      if (triggers != null) this.compilation.ContractProvider.AssociateTriggersWithQuantifier(result, triggers);
      return result;
    }

    protected override Expression ParseLabeledExpression(TokenSet followers) {
      if (this.currentToken == Token.Colon) {
        SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
        this.GetNextToken();
        var label = this.ParseNameDeclaration(true);
        Expression expr;
        if (TS.UnaryStart[this.currentToken]) {
          expr = this.ParseExpression(followers);
          slb.UpdateToSpan(expr.SourceLocation);
        } else {
          slb.UpdateToSpan(label.SourceLocation);
          expr = new DummyExpression(slb);
          this.SkipTo(followers);
        }
        return new VccLabeledExpression(expr, label, slb);
      } else return this.ParseExpression(followers);
    }

    protected override Expression ParseSpecCastExpression(Parser.TokenSet followers) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      if (this.CurrentTokenStartsTypeExpression()) {
        TypeExpression targetType = this.ParseTypeExpression(followers | Token.RightParenthesis);
        this.Skip(Token.RightParenthesis);
        var valueToCast = this.ParseUnaryExpression(followers);
        slb.UpdateToSpan(valueToCast.SourceLocation);
        return new VccCast(valueToCast, targetType, slb);
      } else if (this.currentToken == Token.Identifier && this.scanner.GetIdentifierString() == "unchecked") {
        this.GetNextToken();
        this.Skip(Token.RightParenthesis);
        var uncheckedExpr = this.ParseUnaryExpression(followers);
        slb.UpdateToSpan(uncheckedExpr.SourceLocation);
        return new UncheckedExpression(uncheckedExpr, slb);
      } else {
        this.HandleError(Error.SyntaxError, this.scanner.GetTokenSource());
        this.SkipTo(followers);
        return new DummyExpression(slb);
      }
    }

    protected override Expression ParseQuantifier(TokenSet followers) {
      Token kind = this.currentToken;
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      TokenSet followersOrLeftBraceOrSemicolonOrUnaryStart = followers | Token.LeftBrace | Token.Semicolon | TS.UnaryStart;
      List<LocalDeclarationsStatement> boundVariables = this.ParseQuantifierBoundVariables(followersOrLeftBraceOrSemicolonOrUnaryStart);
      IEnumerable<IEnumerable<Expression>> triggers = this.ParseQuantifierTriggers(followers | TS.UnaryStart | TS.PrimaryStart);
      Expression condition = this.ParseExpression(followers);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      Expression result;
      if (kind == Token.Exists)
        result = new Exists(boundVariables, condition, slb);
      else if (kind == Token.Forall)
        result = new Forall(boundVariables, condition, slb);
      else if (kind == Token.Lambda)
        result = new VccLambda(boundVariables, new CompileTimeConstant(true, SourceDummy.SourceLocation), condition, slb);
      else
        throw new InvalidOperationException();
      this.compilation.ContractProvider.AssociateTriggersWithQuantifier(result, triggers);
      return result;
    }

    private bool SkipIntoSpecBlock() {
      this.GetNextToken();
      bool savedInSpecCode = this.EnterSpecBlock();
      this.Skip(Token.LeftParenthesis, true);
      return savedInSpecCode;
    }

    private void SkipOutOfSpecBlock(bool savedInSpecBlock, TokenSet followers) {
      this.Skip(Token.RightParenthesis);
      this.LeaveSpecBlock(savedInSpecBlock);
      this.SkipTo(followers);
    }

    private void SkipSemicolonsInSpecBlock(TokenSet followers) {
      if (this.currentToken == Token.RightParenthesis) return;
      while (this.currentToken == Token.Semicolon)
        this.Skip(Token.Semicolon, true);
      this.SkipTo(followers);
    }

    private Statement ParseSingleArgSpecStatement(TokenSet followers, Func<Expression, ISourceLocation, Statement> createStmt) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      var expr = this.ParseExpression(followers);
      slb.UpdateToSpan(expr.SourceLocation);
      return createStmt(expr, slb);
    }

    private static class STS {
      public static TokenSet SimpleSpecStatment = new TokenSet() | Token.SpecWrap | Token.SpecUnwrap | Token.SpecGhost | Token.SpecAssume | Token.SpecAssert;
      public static TokenSet FunctionOrBlockContract = new TokenSet() | Token.SpecEnsures | Token.SpecMaintains | Token.SpecReads | Token.SpecRequires | Token.SpecDecreases | Token.SpecWrites;
      public static TokenSet LoopContract = new TokenSet() | Token.SpecInvariant | Token.SpecWrites | Token.SpecDecreases;
      public static TokenSet SpecParameter = new TokenSet() | Token.SpecGhost | Token.SpecOut;
      public static TokenSet TypeMember = new TokenSet() | Token.SpecGhost | Token.SpecInvariant;
      public static TokenSet Global = new TokenSet() | Token.SpecAxiom | Token.SpecGhost | Token.SpecPure;
      public static TokenSet SpecTypeModifiers = new TokenSet() | Token.SpecClaimable | Token.SpecDynamicOwns;
    }
  }
}