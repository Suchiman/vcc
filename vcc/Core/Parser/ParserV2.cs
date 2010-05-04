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
    readonly IName ExistsKeyword;
    readonly IName ForallKeyword;
    readonly IName OldKeyword;
    readonly IName ResultKeyword;
    readonly IName ThisKeyword;

    internal ParserV2(Compilation compilation, ISourceLocation sourceLocation, List<IErrorMessage> scannerAndParserErrors) 
    : base(compilation, sourceLocation, scannerAndParserErrors) {
      this.ExistsKeyword= this.compilation.NameTable.GetNameFor("\\exists");
      this.ForallKeyword = this.compilation.NameTable.GetNameFor("\\forall");
      this.OldKeyword = this.compilation.NameTable.GetNameFor("\\old");
      this.ResultKeyword = this.compilation.NameTable.GetNameFor("\\result");
      this.ThisKeyword = this.compilation.NameTable.GetNameFor("\\this");
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

    override protected void ParseGlobalSpecDeclarationList(List<INamespaceDeclarationMember> members, List<ITypeDeclarationMember> globalMembers, TokenSet followers) {
      this.GetNextToken();
      bool savedInSpecCode = this.EnterSpecBlock();
      this.Skip(Token.LeftParenthesis);

      if (STS.SpecTypeModifiers[this.CurrentSpecToken]) {
        this.ParseGlobalDeclarationWithSpecModifiers(members, globalMembers, followers, savedInSpecCode);
        return;
      }
      TokenSet followersOrDeclarationStart = followers | TS.DeclarationStart | Token.Semicolon | Token.RightParenthesis;
      while ((TS.DeclarationStart[this.currentToken] || STS.Global[this.CurrentSpecToken]) && this.currentToken != Token.EndOfFile) {
        switch (this.CurrentSpecToken) {
          case SpecToken.Axiom:
            SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
            this.GetNextToken();
            var axiom = this.ParseExpression(followersOrDeclarationStart);
            slb.UpdateToSpan(axiom.SourceLocation);
            this.AddTypeInvariantToCurrent(new TypeInvariant(null, axiom, true, slb));
            break;
          case SpecToken.Ghost:
            throw new Exception();
          case SpecToken.None:
            if (this.currentToken == Token.Specification)
              this.ParseGlobalSpecDeclarationList(members, globalMembers, followersOrDeclarationStart);
            else
              this.ParseNonLocalDeclaration(members, globalMembers, followersOrDeclarationStart, true);
            break;
          default:
            this.HandleError(Error.UnexpectedVccKeyword, this.scanner.GetIdentifierString());
            goto exitLoop;
        }
        while (this.currentToken == Token.Semicolon) this.GetNextToken();
      }
    exitLoop:
      this.SkipOverTo(Token.RightParenthesis, followers);
      this.LeaveSpecBlock(savedInSpecCode);
    }

    private void ParseGlobalDeclarationWithSpecModifiers(List<INamespaceDeclarationMember> members, List<ITypeDeclarationMember> globalMembers, TokenSet followers, bool savedInSpecCode) {
      List<Specifier> specifiers = new List<Specifier>();
      TokenSet followersOrCommaOrRightParentesisOrSpecToken = followers | Token.Comma | Token.RightParenthesis | Token.Identifier;
      while (true) {
        if (this.currentToken == Token.RightParenthesis)
          break;
        if (this.currentToken == Token.Comma) {
          this.GetNextToken();
          continue;
        }
        switch (this.CurrentSpecToken) {
          case SpecToken.DynamicOwns:
          case SpecToken.Claimable:
            specifiers.Add(new SpecTypeSpecifier(this.CurrentSpecToken, this.scanner.SourceLocationOfLastScannedToken));
            this.GetNextToken();
            this.SkipTo(followersOrCommaOrRightParentesisOrSpecToken);
            continue;
          case SpecToken.None:
            this.SkipTo(followersOrCommaOrRightParentesisOrSpecToken);
            break;
        }
      }

      this.SkipOverTo(Token.RightParenthesis, followers);
      this.LeaveSpecBlock(savedInSpecCode);
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
      bool savedInSpecCode = this.EnterSpecBlock();
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      while (STS.TypeMember[this.CurrentSpecToken]) {
        switch (this.CurrentSpecToken) {
          case SpecToken.Invariant:
            this.ParseTypeInvariant(followers | Token.RightParenthesis | Token.Identifier);
            break;
          case SpecToken.Ghost:
            this.GetNextToken();
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
        bool savedInSpecCode = this.EnterSpecBlock();
        this.GetNextToken();
        this.Skip(Token.LeftParenthesis);
        while (STS.LoopContract[this.CurrentSpecToken]) {
          switch (this.CurrentSpecToken) {
            case SpecToken.Invariant:
              SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
              this.GetNextToken();
              var inv = this.ParseExpression(followers | Token.RightParenthesis);
              slb.UpdateToSpan(inv.SourceLocation);
              invariants.Add(new LoopInvariant(inv, slb));
              break;
            case SpecToken.Writes:
              this.GetNextToken();
              this.ParseExpressionList(writes, Token.Comma, followers | Token.RightParenthesis);
              break;
            case SpecToken.Variant:
              SourceLocationBuilder slb2 = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
              this.GetNextToken();
              var red = this.ParseExpression(followers | Token.RightParenthesis);
              slb2.UpdateToSpan(red.SourceLocation);
              variants.Add(new LoopVariant(red, slb2));
              break;
          }
        }
        this.SkipOverTo(Token.RightParenthesis, followers | Token.Specification);
        this.LeaveSpecBlock(savedInSpecCode);
      }
      if (invariants.Count == 0 && writes.Count == 0 && variants.Count == 0) return null;
      return new LoopContract(invariants, writes, variants);
    }

    override protected void ParseFunctionOrBlockContract(FunctionOrBlockContract contract, TokenSet followers) {
      while (this.currentToken == Token.Specification) {
        bool savedInSpecCode = this.EnterSpecBlock();
        this.GetNextToken();
        this.Skip(Token.LeftParenthesis);
        while (STS.Contract[this.CurrentSpecToken]) {
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
            case SpecToken.Variant:
              this.GetNextToken();
              var variant = this.ParseExpression(followers | Token.RightParenthesis);
              contract.AddMethodVariant(new MethodVariant(variant, variant.SourceLocation));
              break;
            case SpecToken.Reads:
              this.GetNextToken();
              var reads = this.ParseExpressionList(Token.Comma, followers | Token.RightParenthesis);
              contract.AddReads(reads);
              break;
          }
        }
        this.SkipOverTo(Token.RightParenthesis, followers | Token.Specification);
        this.LeaveSpecBlock(savedInSpecCode);
      }
    }

    protected override List<Parameter> ParseParameterList(Parser.TokenSet followers) {
      List<Parameter> result = new List<Parameter>();
      this.Skip(Token.LeftParenthesis);
      if (this.currentToken != Token.RightParenthesis) {
        TokenSet followersOrCommaOrRightParentesisOrSpecification = followers | Token.Comma | Token.RightParenthesis | Token.Specification;
        while (true) {
          if (this.currentToken == Token.Specification) {
            result.Add(this.ParseSpecParameter(followers | Token.RightParenthesis));
            continue;
          }
          if (this.currentToken == Token.RightParenthesis)
            break;
          result.Add(this.ParseParameter(followersOrCommaOrRightParentesisOrSpecification));
          if (this.currentToken == Token.Comma) {
            this.GetNextToken();
            continue;
          }
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
      this.GetNextToken();
      bool savedInSpecCode = this.EnterSpecBlock();
      this.Skip(Token.LeftParenthesis);
      switch (this.CurrentSpecToken) {
        case SpecToken.Ghost:
          this.GetNextToken();
          result = this.ParseParameter(followers|Token.RightParenthesis, false, slb);
          break;
        case SpecToken.Out:
          this.GetNextToken();
          result = this.ParseParameter(followers|Token.RightParenthesis, true, slb);
          break;
        default:
          this.HandleError(Error.SyntaxError, "ghost or out");
          result = this.ParseParameter(followers | Token.RightParenthesis);
          break;
      }
      this.Skip(Token.RightParenthesis);
      this.LeaveSpecBlock(savedInSpecCode);
      this.SkipTo(followers);
      return result;
    }

    protected override List<Expression> ParseArgumentList(SourceLocationBuilder slb, Parser.TokenSet followers) {
      List<Expression> result = new List<Expression>();
      this.Skip(Token.LeftParenthesis);
      if (this.currentToken != Token.RightParenthesis) {
        while (true) {
          if (this.currentToken == Token.Specification) {
            result.Add(this.ParseSpecArgument(followers | Token.RightParenthesis));
            continue;
          }
          if (this.currentToken == Token.RightParenthesis)
            break;
          result.Add(this.ParseArgumentExpression(followers | Token.RightParenthesis));
          if (this.currentToken == Token.Comma) {
            this.GetNextToken();
            continue;
          }
        }
      }
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.SkipOverTo(Token.RightParenthesis, followers);
      return result;
    }

    private Expression ParseSpecArgument(TokenSet followers) {
      Expression result;
      bool savedInSpecCode = this.EnterSpecBlock();
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      switch (this.CurrentSpecToken) {
        case SpecToken.Ghost:
          this.GetNextToken();
          result = this.ParseExpression(followers | Token.RightParenthesis);
          break;
        case SpecToken.Out:
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
      this.Skip(Token.RightParenthesis);
      this.LeaveSpecBlock(savedInSpecCode);
      this.SkipTo(followers);
      return result;
    }

    override protected Statement ParseSpecStatements(TokenSet followers) {
      this.GetNextToken();
      bool savedInSpecCode = this.EnterSpecBlock();
      this.Skip(Token.LeftParenthesis);

      if (this.CurrentSpecToken == SpecToken.Unwrapping) {
        return this.ParseUnwrappingStatement(followers, savedInSpecCode);
      }

      if (this.CurrentSpecToken == SpecToken.Atomic) {
        return this.ParseAtomic(followers, savedInSpecCode);
      }
      
      List<Statement> statements = new List<Statement>();
      TokenSet followersOrRightParen = followers | Token.RightParenthesis;
      SourceLocationBuilder slb;

      while (STS.SimpleStatement[this.CurrentSpecToken]) {
        switch (this.CurrentSpecToken) {
          case SpecToken.Ghost:
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
          case SpecToken.Wrap:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new VccWrapStatement(expr, sl)));
            break;
          case SpecToken.Unwrap:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new VccUnwrapStatement(expr, sl)));
            break;
          case SpecToken.Assert:
            statements.Add(this.ParseAssert(followersOrRightParen));
            break;
          case SpecToken.Assume:
            statements.Add(this.ParseSingleArgSpecStatement(followersOrRightParen, (expr, sl) => new AssumeStatement(expr, sl)));
            break;
        }
        if (this.currentToken == Token.Semicolon)
          this.SkipOverTo(Token.Semicolon, followersOrRightParen | Token.Identifier);
      }
      this.SkipOverTo(Token.RightParenthesis, followers);
      this.LeaveSpecBlock(savedInSpecCode);
      return new StatementGroup(statements);
    }

    private Statement ParseAtomic(TokenSet followers, bool savedInSpecCode) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      var exprs = this.ParseExpressionList(Token.Comma, followers | Token.RightParenthesis);
      this.Skip(Token.RightParenthesis);
      this.LeaveSpecBlock(savedInSpecCode);
      var body = this.ParseStatement(followers);
      slb.UpdateToSpan(body.SourceLocation);
      return new VccAtomicStatement(body, exprs, slb);
    }

    private Statement ParseUnwrappingStatement(TokenSet followers, bool savedInSpecCode) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      var expr = this.ParseExpression(followers | Token.RightParenthesis);
      this.Skip(Token.RightParenthesis);
      this.LeaveSpecBlock(savedInSpecCode);
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

    new protected List<LocalDeclarationsStatement> ParseQuantifierBoundVariables(TokenSet followers) {
      List<LocalDeclarationsStatement> result = new List<LocalDeclarationsStatement>();
      TokenSet followersOrTypeStart = followers | TS.TypeStart;
      while (this.CurrentTokenStartsTypeExpression()) {
        List<Specifier> specifiers = this.ParseSpecifiers(null, null, null, followers);
        List<LocalDeclaration> declarations = new List<LocalDeclaration>(1);
        Declarator declarator = this.ParseDeclarator(followers | Token.Comma, true);
        TypeExpression type = this.GetTypeExpressionFor(specifiers, declarator);
        SourceLocationBuilder slb = new SourceLocationBuilder(type.SourceLocation);
        slb.UpdateToSpan(declarator.SourceLocation);
        declarations.Add(new LocalDeclaration(false, false, declarator.Identifier, null, slb));
        while (this.currentToken == Token.Comma) {
          this.GetNextToken();
          declarator = this.ParseDeclarator(followers | Token.Comma, true);
          slb.UpdateToSpan(declarator.SourceLocation);
          declarations.Add(new LocalDeclaration(false, false, declarator.Identifier, null, slb));
        }
        LocalDeclarationsStatement locDecls = new LocalDeclarationsStatement(false, false, false, type, declarations, slb);
        result.Add(locDecls);
      }
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

    private Expression ParseQuantifier(TokenSet followers, Token kind, SourceLocationBuilder slb) {
      TokenSet followersOrLeftBraceOrSemicolonOrUnaryStart = followers | Token.LeftBrace | Token.Semicolon | TS.UnaryStart;
      List<LocalDeclarationsStatement> boundVariables = this.ParseQuantifierBoundVariables(followersOrLeftBraceOrSemicolonOrUnaryStart);
      IEnumerable<IEnumerable<Expression>> triggers = this.ParseQuantifierTriggers(followers | Token.Semicolon | TS.UnaryStart);
      this.SkipSemiColon(followers | TS.UnaryStart);
      Expression condition = this.ParseExpression(followers);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      Expression result;
      if (kind == Token.Exists)
        result = new Exists(boundVariables, condition, slb);
      else if (kind == Token.Forall)
        result = new Forall(boundVariables, condition, slb);
      else {
        result = null;
      }
      this.compilation.ContractProvider.AssociateTriggersWithQuantifier(result, triggers);
      return result;
    }

    override protected bool TryParseSpecialExpression(SimpleName name, TokenSet followers, out Expression expression) {
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
      } else if (name.Name.UniqueKey == this.ForallKeyword.UniqueKey) {
        expression = this.ParseQuantifier(followers, Token.Forall, new SourceLocationBuilder(name.SourceLocation));
        return true;
      } else if (name.Name.UniqueKey == this.ExistsKeyword.UniqueKey) {
        expression = this.ParseQuantifier(followers, Token.Exists, new SourceLocationBuilder(name.SourceLocation));
        return true;
      } else if (name.Name.UniqueKey == this.ThisKeyword.UniqueKey) {
        expression = new VccThisReference(name.SourceLocation);
        return true;
      }
      expression = null;
      return false;
    }

    private Statement ParseSingleArgSpecStatement(TokenSet followers, Func<Expression, ISourceLocation, Statement> createStmt) {
      SourceLocationBuilder slb = this.GetSourceLocationBuilderForLastScannedToken();
      this.GetNextToken();
      var expr = this.ParseExpression(followers);
      slb.UpdateToSpan(expr.SourceLocation);
      return createStmt(expr, slb);
    }

    protected override bool ReportErrorMoreSpecificErrorFor(Token token) {
      if (this.InSpecCode && this.CurrentSpecToken != SpecToken.None) {
        this.HandleError(Error.UnexpectedVccKeyword, this.scanner.GetIdentifierString());
        return true;
      }
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
            case "atomic":
              return SpecToken.Atomic;
            case "axiom":
              return SpecToken.Axiom;
            case "claimable":
              return SpecToken.Claimable;
            case "decreases":
              return SpecToken.Variant;
            case "dynamic_owns":
              return SpecToken.DynamicOwns;
            case "ensures":
              return SpecToken.Ensures;
            case "ghost":
              return SpecToken.Ghost;
            case "invariant":
              return SpecToken.Invariant;
            case "maintains":
              return SpecToken.Maintains;
            case "out":
              return SpecToken.Out;
            case "unwrap":
              return SpecToken.Unwrap;
            case "unwrapping":
              return SpecToken.Unwrapping;
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

    public enum SpecToken
    {
      None,
      Assert,
      Assume,
      Atomic,
      Axiom,
      Claimable,
      DynamicOwns,
      Ensures,
      Ghost,
      Invariant,
      Maintains,
      Out,
      Reads,
      Variant,
      Requires,
      Unwrap,
      Unwrapping,
      Wrap,
      Writes,
    }


    private static class STS {
      public static SpecTokenSet SpecTypeModifiers = new SpecTokenSet(SpecToken.Claimable, SpecToken.DynamicOwns);
      public static SpecTokenSet Global = new SpecTokenSet(SpecToken.Axiom, SpecToken.Ghost);
      public static SpecTokenSet SimpleStatement = new SpecTokenSet(SpecToken.Wrap, SpecToken.Unwrap, SpecToken.Ghost, SpecToken.Assert, SpecToken.Assume);
      public static SpecTokenSet Contract = new SpecTokenSet(SpecToken.Ensures, SpecToken.Maintains, SpecToken.Reads, SpecToken.Requires, SpecToken.Writes);
      public static SpecTokenSet LoopContract = new SpecTokenSet(SpecToken.Invariant, SpecToken.Writes, SpecToken.Variant);
      public static SpecTokenSet TypeMember = new SpecTokenSet(SpecToken.Ghost, SpecToken.Invariant);
      public static SpecTokenSet SpecParameter = new SpecTokenSet(SpecToken.Ghost, SpecToken.Out);

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

      public SpecTokenSet(SpecToken t1, SpecToken t2, SpecToken t3, SpecToken t4, SpecToken t5, SpecToken t6) {
        this.bits = 0;
        this.bits |= (1ul << (int)t1);
        this.bits |= (1ul << (int)t2);
        this.bits |= (1ul << (int)t3);
        this.bits |= (1ul << (int)t4);
        this.bits |= (1ul << (int)t5);
        this.bits |= (1ul << (int)t6);
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