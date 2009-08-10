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

namespace Microsoft.Research.Vcc.Parsing {

  internal class Parser {
    Compilation compilation;
    List<FieldDeclaration>/*?*/ currentSpecificationFields;
    List<FunctionDeclaration>/*?*/ currentSpecificationFunctions;
    List<TypeInvariant>/*?*/ currentTypeInvariants;
    List<ITypeDeclarationMember>/*?*/ currentTypeMembers;
    Token currentToken;
    Expression/*?*/ currentTypeName;
    Dictionary<string, TypeExpression> typedefExpressions;
    Dictionary<string, TypedefDeclaration> typedefDecls;
    Dictionary<string, bool> locallyDefinedNames;
    Dictionary<Expression, bool> emptyStructuredTypes;
    INameTable nameTable;
    bool resultIsAKeyword;
    Scanner scanner;
    List<IErrorMessage> scannerAndParserErrors;
    LexicalScope/*?*/ currentLexicalScope = null;
    RootNamespaceExpression rootNs;
    AliasQualifiedName systemNs;
    
    internal Parser(Compilation compilation, ISourceLocation sourceLocation, List<IErrorMessage> scannerAndParserErrors)
    {
      this.compilation = compilation;
      this.nameTable = compilation.NameTable;
      this.scannerAndParserErrors = scannerAndParserErrors;
      this.scanner = new Scanner(scannerAndParserErrors, sourceLocation, true);
      this.typedefExpressions = new Dictionary<string, TypeExpression>();
      this.typedefDecls = new Dictionary<string, TypedefDeclaration>();
      this.locallyDefinedNames = new Dictionary<string, bool>();
      this.emptyStructuredTypes = new Dictionary<Expression, bool>();
      this.rootNs = new RootNamespaceExpression(SourceDummy.SourceLocation);
      this.systemNs = new AliasQualifiedName(rootNs, this.GetSimpleNameFor("System"), SourceDummy.SourceLocation);
    }

    private IName GetNameFor(string name)
      //^ ensures result.Value == name;
    {
      return this.nameTable.GetNameFor(name);
    }

    private void GetNextToken()
      //^ requires this.currentToken != Token.EndOfFile;
    {
      this.currentToken = this.scanner.GetNextToken();
      if (this.currentToken == Token.Pragma) {
        this.ParsePragma();
      }
    }

    private void HandleError(Error error, params string[] messageParameters) 
      // ^ modifies this.scannerAndParserErrors;
      //^ ensures this.currentToken == old(this.currentToken);
    {
      this.HandleError(this.scanner.SourceLocationOfLastScannedToken, error, messageParameters);
    }

    private void HandleError(ISourceLocation errorLocation, Error error, params string[] messageParameters)
      // ^ modifies this.scannerAndParserErrors;
      //^ ensures this.currentToken == old(this.currentToken);
    {
      //^ Token oldToken = this.currentToken;
      this.scannerAndParserErrors.Add(new VccErrorMessage(errorLocation, error, messageParameters));
      //^ assume this.currentToken == oldToken;
    }

    private void WarnIfLoopWithContractAndEmptyBody(LoopContract contract, Statement body)
    {
      if (contract != null
        && (body is EmptyStatement
          || (body is BlockStatement && (((BlockStatement)body).IsEmpty))))
        this.HandleError(body.SourceLocation, Error.PossibleMistakenNullStatement);
    }

    /// <summary>
    /// Call this method only on a freshly allocated Parser instance and call it only once.
    /// </summary>
    internal void ParseCompilationUnit(GlobalDeclarationContainerClass globalContainer, List<INamespaceDeclarationMember> members) {
      //^ assume this.currentToken != Token.EndOfFile; //assume this method is called directly after construction and then never again.
      this.GetNextToken(); //Get first token from scanner
      this.ParseNamespaceMemberDeclarations(globalContainer, members, Parser.EndOfFile);
      VccTypeContract tc = new VccTypeContract(this.currentSpecificationFields, this.currentSpecificationFunctions, this.currentTypeInvariants);
      this.compilation.ContractProvider.AssociateTypeWithContract(globalContainer, tc);
    }

    private NameDeclaration ParseNameDeclaration(bool requireIdentifier) {
      IName name;
      ISourceLocation sourceLocation = this.scanner.SourceLocationOfLastScannedToken;
      if (this.currentToken == Token.Identifier) {
        name = this.GetNameFor(this.scanner.GetIdentifierString());
        this.GetNextToken();
      } else {
        if (requireIdentifier) this.HandleError(Error.ExpectedIdentifier);
        name = this.GetNameFor(sourceLocation.SourceDocument.Name.Value+sourceLocation.StartIndex);
      }
      return new NameDeclaration(name, sourceLocation);
    }

    private Expression ParseScopedName(Expression qualifier, TokenSet followers)
    {
      Expression result = qualifier;
      while (this.currentToken == Token.ScopeResolution) {
        SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
        this.GetNextToken();
        SimpleName name = this.ParseSimpleName(followers);
        slb.UpdateToSpan(name.SourceLocation);
        result = new VccScopedName(result, name, slb.GetSourceLocation());
      }
      return result;
    }

    private Expression ParseSimpleOrScopedName(TokenSet followers)
    {
      SimpleName qualifier = this.ParseSimpleName(followers | Token.ScopeResolution);
      return ParseScopedName(qualifier, followers | Token.ScopeResolution);
    }

    private VccSimpleName ParseSimpleName(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      IName name;
      ISourceLocation sourceLocation = this.scanner.SourceLocationOfLastScannedToken;
      if (this.currentToken == Token.Identifier) {
        name = this.GetNameFor(this.scanner.GetIdentifierString());
        //^ assume this.currentToken != Token.EndOfFile;
        this.GetNextToken();
      } else {
        name = Dummy.Name;
        this.HandleError(Error.ExpectedIdentifier);
      }
      VccSimpleName result = new VccSimpleName(name, sourceLocation);
      this.SkipTo(followers);
      return result;
    }

    List<INamespaceDeclarationMember>/*?*/ namespaceDeclarationMembers;

    private void ParseNamespaceMemberDeclarations(GlobalDeclarationContainerClass globalContainer, List<INamespaceDeclarationMember> members, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      List<ITypeDeclarationMember> globalMembers = (List<ITypeDeclarationMember>)globalContainer.GlobalMembers;

      this.currentTypeMembers = globalMembers;
      this.namespaceDeclarationMembers = members;
      TokenSet followersOrDeclarationStart = followers|Parser.DeclarationStart;
      while (followersOrDeclarationStart[this.currentToken] && this.currentToken != Token.EndOfFile)
        this.ParseNonLocalDeclaration(members, globalMembers, followersOrDeclarationStart, true);
      this.SkipTo(followers);
    }

    private void ParseNonLocalDeclaration(List<INamespaceDeclarationMember>/*?*/ namespaceMembers, List<ITypeDeclarationMember> typeMembers, TokenSet followers, bool isGlobal)
      //^ requires this.currentToken != Token.EndOfFile;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      Dictionary<string, TypeExpression> savedTypedefExpressions = this.typedefExpressions;
      List<TemplateParameterDeclarator>/*?*/ templateParameters = this.ParseTemplateParameters(followers|Parser.DeclaratorStart|Token.RightParenthesis|Token.Semicolon);
      List<Specifier> specifiers = this.ParseSpecifiers(namespaceMembers, typeMembers, followers|Parser.DeclaratorStart|Token.Semicolon|Token.Colon);
      VccFunctionTypeExpression/*?*/ functionTypeExpression = null;
      TypedefNameSpecifier/*?*/ typeDefName = GetTypedefNameSpecifier(specifiers);
      if (typeDefName != null) {
        TypeExpression/*?*/ typeDefExpr;
        this.typedefExpressions.TryGetValue(typeDefName.TypedefName.Name.Value, out typeDefExpr);
        functionTypeExpression = typeDefExpr as VccFunctionTypeExpression;
      }
      bool foundNoDeclaration = true;
      TokenSet followersOrCommaOrLeftBraceOrSemicolon = followers|Token.Comma|Token.LeftBrace|Token.Semicolon;
      while (Parser.DeclaratorStart[this.currentToken]) {
        foundNoDeclaration = false;
        Declarator declarator = this.ParseDeclarator(followersOrCommaOrLeftBraceOrSemicolon);
        if (functionTypeExpression != null && functionTypeExpression.declarator != null && declarator is IdentifierDeclarator)
          declarator = new FunctionDeclarator(declarator, functionTypeExpression.declarator);
        else {
          // List<Specifier> innerSpecifiers = this.ParseSpecifiers(namespaceMembers, followers | Parser.DeclarationStart | Token.Semicolon | Token.Colon);
          declarator = this.UseDeclaratorAsTypeDefNameIfThisSeemsIntended(specifiers, declarator, followersOrCommaOrLeftBraceOrSemicolon);
        }
        PointerDeclarator/*?*/ pointerDeclarator = declarator as PointerDeclarator;
        FunctionDeclarator/*?*/ funcDeclarator = declarator as FunctionDeclarator;
        ArrayDeclarator/*?*/ arrayDeclarator = declarator as ArrayDeclarator;
        while (funcDeclarator == null && (pointerDeclarator != null || arrayDeclarator != null)) {
          if (pointerDeclarator != null) {
            funcDeclarator = pointerDeclarator.Declarator as FunctionDeclarator;
            arrayDeclarator = pointerDeclarator.Declarator as ArrayDeclarator;
            pointerDeclarator = null;
          } else if (arrayDeclarator != null) {
            funcDeclarator = arrayDeclarator.ElementType as FunctionDeclarator;
            pointerDeclarator = arrayDeclarator.ElementType as PointerDeclarator;
            arrayDeclarator = null;
          }
        }
        if (funcDeclarator != null) {
          funcDeclarator.TemplateParameters = templateParameters;
          //TODO: complain if not first declarator
          if (this.currentToken == Token.LeftBrace) {
            this.ParseFunctionDefinition(specifiers, typeMembers, declarator, funcDeclarator, followers|Token.Semicolon);
            return;
          } else
            this.ParseFunctionDeclaration(specifiers, typeMembers, declarator, funcDeclarator, followers|Token.Semicolon, isGlobal);
        } else {
          //TODO: complain if templateParameters are not null
          this.AddTypeDeclarationMember(specifiers, declarator, typeMembers);
        }
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      if (specifiers.Count > 0 && specifiers[specifiers.Count-1] is CompositeTypeSpecifier) {
        while (this.currentToken == Token.Semicolon) this.GetNextToken();
        if (this.currentTypeName != null && foundNoDeclaration) {
          StructSpecifier structSpecifier = specifiers[specifiers.Count - 1] as StructSpecifier;
          UnionSpecifier unionSpecifier = specifiers[specifiers.Count - 1] as UnionSpecifier;
          if (structSpecifier != null || unionSpecifier != null) {
            if (structSpecifier == null || !this.emptyStructuredTypes.ContainsKey(structSpecifier.TypeExpression)) {
              Declarator declarator = new AnonymousFieldDeclarator();
              this.AddTypeDeclarationMember(specifiers, declarator, typeMembers);
            }
          }
        }
        this.SkipTo(followers);
      }else
        this.SkipSemiColon(followers);
      this.typedefExpressions = savedTypedefExpressions;
    }

    private List<Statement> ParseLocalDeclaration(TokenSet followers) {
      List<Statement> result = new List<Statement>(4);      
      // Because, in C, a local declaration may introduce a type definition to the global scope
      // pass in the namespace declaration members so that these definitions can be found later. 
      List<Specifier> specifiers = this.ParseSpecifiers(this.namespaceDeclarationMembers, null, followers|Token.Semicolon);
      if ((this.currentToken == Token.LeftBrace || this.currentToken == Token.LeftParenthesis) && specifiers.Count == 1) {
        StorageClassSpecifier/*?*/ scSpecifier = specifiers[0] as StorageClassSpecifier;
        if (scSpecifier != null && scSpecifier.Token == Token.Specification) {
          if (this.currentToken == Token.LeftParenthesis) {
            List<Statement> statements = new List<Statement>();
            SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
            this.GetNextToken();
            this.ParseStatements(statements, followers | Token.RightParenthesis);
            slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
            result.Add(new BlockStatement(statements, BlockStatement.Options.Default, slb));
            this.SkipOverTo(Token.RightParenthesis, followers);
          } else {
            result.Add(this.ParseBlock(followers));
          }
          return result;
        }
      }
      bool isLocalTypeDef = false;
      foreach (Specifier sp in specifiers) {
        StorageClassSpecifier/*?*/ scs = sp as StorageClassSpecifier;
        if (scs != null) {
          if (scs.Token == Token.Typedef) isLocalTypeDef = true;
        }
      }
      while (Parser.DeclaratorStart[this.currentToken]) {
        Declarator declarator = this.ParseDeclarator(followers|Token.Comma|Token.LeftBrace|Token.Semicolon);
        declarator = this.UseDeclaratorAsTypeDefNameIfThisSeemsIntended(specifiers, declarator, followers);
        if (isLocalTypeDef) {
          if (this.currentTypeMembers != null)
            this.AddTypeDeclarationMember(specifiers, declarator, this.currentTypeMembers);
          break;
        }
        this.AddDeclarationStatement(specifiers, declarator, result);
        this.locallyDefinedNames[declarator.Identifier.Name.Value] = true;
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      this.SkipSemiColon(followers);
      return result;
    }

    private LoopContract/*?*/ ParseLoopContract(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      if (this.currentToken != Token.Invariant && this.currentToken != Token.Writes) return null;
      List<LoopInvariant> invariants = new List<LoopInvariant>();
      List<Expression> writes = new List<Expression>();
      LoopContract loopContract = new LoopContract(invariants, writes);
      while (true) {
        if (this.currentToken == Token.Invariant)
          this.ParseLoopInvariant(invariants, followers | Token.Invariant | Token.Writes);
        else if (this.currentToken == Token.Writes)
          this.ParseWrites(writes, followers | Token.Invariant | Token.Writes);
        else
          break;
      }
      this.SkipTo(followers);
      return loopContract;
    }

    private void ParseLoopInvariant(List<LoopInvariant> invariants, TokenSet followers)
    //^ requires this.currentToken == Token.Invariant;
    //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      Expression condition = this.ParseExpression(followers | Token.RightParenthesis);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.Skip(Token.RightParenthesis);
      LoopInvariant loopInvariant = new LoopInvariant(condition, slb);
      invariants.Add(loopInvariant);
    }

    private void ParseTypeInvariant(List<TypeInvariant> invariants, TokenSet followers)
      //^ requires this.currentToken == Token.Invariant;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      NameDeclaration nameDecl = null;
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      if (this.currentToken == Token.Identifier) {
        string id = this.scanner.GetIdentifierString();
        Scanner.Snapshot snap = this.scanner.MakeSnapshot();
        Token tok = scanner.GetNextToken();
        if (tok == Token.Colon) {
          slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
          nameDecl = new NameDeclaration(this.GetNameFor(id), slb);
          this.GetNextToken();
        } else {
          this.scanner.RevertToSnapshot(snap);
        }
      }
      Expression condition = this.ParseExpression(followers|Token.RightParenthesis);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.Skip(Token.RightParenthesis);
      TypeInvariant typeInvariant = new TypeInvariant(nameDecl, new CheckedExpression(condition, condition.SourceLocation), false, slb);
      invariants.Add(typeInvariant);
      if (this.currentToken == Token.Semicolon)
        this.SkipOverTo(Token.Semicolon, followers);
    }

    private void ParseFunctionDeclaration(List<Specifier> specifiers, List<ITypeDeclarationMember> typeMembers,
      Declarator declarator, FunctionDeclarator funcDeclarator, TokenSet followers, bool isGlobal)
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(funcDeclarator.SourceLocation);
      if (specifiers.Count > 0) slb.UpdateToSpan(specifiers[0].SourceLocation);
      Token storageClass = GetStorageClassToken(specifiers);
      if (funcDeclarator.Specifiers != null) {
        foreach (Specifier sp in funcDeclarator.Specifiers) {
          specifiers.Add(sp);
        }
        // specifiers = funcDeclarator.Specifiers; //TODO: combine the specifiers?
      }
      TypeMemberVisibility visibility = storageClass == Token.Static ? TypeMemberVisibility.Assembly : TypeMemberVisibility.Public;
      PointerDeclarator/*?*/ pointerToFunc = funcDeclarator.FunctionName as PointerDeclarator;
      TypeExpression returnType;
      if (pointerToFunc != null)
        returnType = this.GetTypeExpressionFor(specifiers, pointerToFunc.Declarator);
      else
        returnType = this.GetTypeExpressionFor(specifiers, funcDeclarator.FunctionName);
      if (declarator != funcDeclarator)
        returnType = this.ApplyDeclarator(declarator, returnType);
      if (pointerToFunc != null || storageClass == Token.Typedef) {
        TypeExpression functionType = this.GetTypeExpressionFor(returnType, funcDeclarator);
        if (storageClass == Token.Typedef) {
          if (pointerToFunc == null) funcDeclarator.Specifiers = specifiers;
          this.typedefExpressions[funcDeclarator.Identifier.Value] = functionType;
          TypedefDeclaration typedef = new TypedefDeclaration(functionType, funcDeclarator.Identifier, slb);  //TODO: const and volatile
          this.typedefDecls[funcDeclarator.Identifier.Value] = typedef;
          this.ParseFunctionOrBlockContract(funcDeclarator.Contract, followers);
          this.AssociateContracts(functionType, funcDeclarator);
          typeMembers.Add(typedef);
        } else {
          //^ assert pointerToFunc != null;
          // Distinguish between whether this function pointer is inside a type definition
          if (isGlobal) {
            GlobalVariableDeclaration globalVarDecl =
               new GlobalVariableDeclaration(0, visibility, functionType, pointerToFunc.Identifier, null, slb);
            typeMembers.Add(globalVarDecl);
          } else {
            FieldDeclaration field = new FieldDeclaration(null, FieldDeclaration.Flags.Unsafe, visibility, functionType, pointerToFunc.Identifier, null, slb);
            typeMembers.Add(field);
          }
        }
        return;
      }
      bool acceptsExtraArguments;
      List<ParameterDeclaration> parameters = this.ConvertToParameterDeclarations(funcDeclarator.Parameters, out acceptsExtraArguments);
      List<GenericMethodParameterDeclaration>/*?*/ templateParameters = Parser.ConvertToGenericMethodParameterDeclarations(funcDeclarator.TemplateParameters);
      CallingConvention callingConvention = GetCallingConvention(specifiers, acceptsExtraArguments);
      FunctionDeclaration fdecl = new FunctionDeclaration(acceptsExtraArguments, specifiers, storageClass == Token.Extern, callingConvention, visibility, returnType, funcDeclarator.Identifier, templateParameters, parameters, slb);
      this.AssociateContracts(fdecl, funcDeclarator);
      typeMembers.Add(fdecl);
    }
    

    private List<ParameterDeclaration> ConvertToParameterDeclarations(List<Parameter> parameters, out bool acceptsExtraArguments) {
      acceptsExtraArguments = false;
      List<ParameterDeclaration> result = new List<ParameterDeclaration>(parameters.Count);
      ushort i = 0;
      for (int j = 0, n = parameters.Count; j < n; j++) {
        Parameter p = parameters[j];
        if (p.IsVarArgs) {
          if (j != n - 1) {
            // TODO: issue an error, varargs must be the last parameter
          }
          acceptsExtraArguments = true;
          break; 
        }
        Declarator name = p.Name;
        ArrayDeclarator/*?*/ array = name as ArrayDeclarator;
        if (array != null && array.ArraySize != null && !(array.ArraySize is TypeExpression)) {
          name = new ArrayDeclarator(array.ElementType, null, array.SourceLocation);
          //TODO: add a precondition requiring the array to be of at least array.ArraySize in length
        }
        TypeExpression type = this.GetTypeExpressionFor(p.TypeSpecifiers, name);
        ParameterDeclaration pdecl = new VccParameterDeclaration(type, name.Identifier, p.TypeSpecifiers, i++, p.IsOut, p.SourceLocation);
        result.Add(pdecl);
      }
      return result;
    }

    private static List<GenericMethodParameterDeclaration>/*?*/ ConvertToGenericMethodParameterDeclarations(List<TemplateParameterDeclarator>/*?*/ parameters) {
      if (parameters == null || parameters.Count == 0) return null;
      List<GenericMethodParameterDeclaration> result = new List<GenericMethodParameterDeclaration>(parameters.Count);
      List<TypeExpression> contraints = new List<TypeExpression>(0);
      ushort index = 0;
      foreach (TemplateParameterDeclarator tp in parameters) {
        GenericMethodParameterDeclaration gmpd = new GenericMethodParameterDeclaration(null, tp.Identifier, index++, contraints, TypeParameterVariance.NonVariant, false, false, false, tp.SourceLocation);
        result.Add(gmpd);
      }
      return result;
    }

    private void ParseFunctionDefinition(List<Specifier> specifiers, List<ITypeDeclarationMember> typeMembers,
      Declarator declarator, FunctionDeclarator funcDeclarator, TokenSet followers)
      //^ requires this.currentToken == Token.LeftBrace;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(funcDeclarator.SourceLocation);
      if (specifiers.Count > 0) slb.UpdateToSpan(specifiers[0].SourceLocation);
      this.locallyDefinedNames.Clear();
      foreach (Parameter p in funcDeclarator.Parameters)
        this.locallyDefinedNames[p.Name.Identifier.Name.Value] = true;

      this.currentLexicalScope = new LexicalScope(this.currentLexicalScope, slb);
      BlockStatement body = this.ParseBody(followers | Token.Semicolon);
      this.currentLexicalScope = this.currentLexicalScope.ParentScope;

      slb.UpdateToSpan(body.SourceLocation);
      Token storageClass = GetStorageClassToken(specifiers);
      TypeMemberVisibility visibility = storageClass == Token.Static ? TypeMemberVisibility.Assembly : TypeMemberVisibility.Public;
      TypeExpression returnType;
      returnType = this.GetTypeExpressionFor(specifiers, funcDeclarator.FunctionName); //TODO: complain if function pointer type has a body
      if (declarator != funcDeclarator)
        returnType = this.ApplyDeclarator(declarator, returnType);
      bool acceptsExtraArguments;
      List<ParameterDeclaration> parameters = this.ConvertToParameterDeclarations(funcDeclarator.Parameters, out acceptsExtraArguments);
      List<GenericMethodParameterDeclaration>/*?*/ templateParameters = Parser.ConvertToGenericMethodParameterDeclarations(funcDeclarator.TemplateParameters);
      CallingConvention callingConvention = GetCallingConvention(specifiers, acceptsExtraArguments);
      if (returnType is VccFunctionTypeExpression)
        returnType = new VccPointerTypeExpression(returnType, null, returnType.SourceLocation);
      MethodDeclaration.Flags flags = 0;
      if (acceptsExtraArguments) flags |= MethodDeclaration.Flags.AcceptsExtraArguments;
      FunctionDefinition func = new FunctionDefinition(flags, specifiers, callingConvention, visibility, returnType, funcDeclarator.Identifier, templateParameters, parameters, body, slb);
      typeMembers.Add(func);
      if (storageClass == Token.Specification && IsInline(specifiers))
        this.compilation.ContractProvider.AssociateMethodWithContract(func, new MethodContract());
      else
        this.AssociateContracts(func, funcDeclarator);
      //TODO: complain if statements != null;
      if (this.currentToken == Token.Semicolon) this.GetNextToken();
      this.SkipTo(followers);
    }

    private TypeExpression ApplyDeclarator(Declarator declarator, TypeExpression returnType) {
      PointerDeclarator/*?*/ pointerDeclarator = declarator as PointerDeclarator;
      FunctionDeclarator/*?*/ funcDeclarator = declarator as FunctionDeclarator;
      ArrayDeclarator/*?*/ arrayDeclarator = declarator as ArrayDeclarator;
      while (funcDeclarator == null && (pointerDeclarator != null || arrayDeclarator != null)) {
        if (pointerDeclarator != null) {
          funcDeclarator = pointerDeclarator.Declarator as FunctionDeclarator;
          arrayDeclarator = pointerDeclarator.Declarator as ArrayDeclarator;
          if (funcDeclarator != null)
            pointerDeclarator.Declarator = new IdentifierDeclarator(funcDeclarator.FunctionName.Identifier);
          pointerDeclarator = null;
        } else if (arrayDeclarator != null) {
          funcDeclarator = arrayDeclarator.ElementType as FunctionDeclarator;
          pointerDeclarator = arrayDeclarator.ElementType as PointerDeclarator;
          if (funcDeclarator != null)
            arrayDeclarator.ElementType = new IdentifierDeclarator(funcDeclarator.FunctionName.Identifier);
          arrayDeclarator = null;
        }
      }
      return this.GetTypeExpressionFor(returnType, declarator);
    }

    private void AssociateContracts(object func, FunctionDeclarator funcDeclarator) {
      if (!funcDeclarator.HasContract) return;
      this.compilation.ContractProvider.AssociateMethodWithContract(func, funcDeclarator.Contract.ToMethodContract());
    }

    private bool IsPointerDeclarator(Declarator declarator) {
      if (declarator is PointerDeclarator) return true;
      InitializedDeclarator/*?*/ id = declarator as InitializedDeclarator;
      if (id != null) return this.IsPointerDeclarator(id.Declarator);
      return (declarator is ArrayDeclarator);
    }

    private void AddDeclarationStatement(List<Specifier> specifiers, Declarator declarator, List<Statement> statements) {
      SourceLocationBuilder slb = new SourceLocationBuilder(declarator.SourceLocation);
      if (specifiers.Count > 0) slb.UpdateToSpan(specifiers[0].SourceLocation);
      TypeExpression localType = this.GetTypeExpressionFor(specifiers, declarator);
      bool isConstant;
      bool isVolatile;
      if (this.IsPointerDeclarator(declarator)) {
        LookForConstAndVolatileForLocalPointer(specifiers, out isConstant, out isVolatile);
      }
      else 
      LookForConstAndVolatile(specifiers, out isConstant, out isVolatile);
      //Token sct = GetStorageClassToken(specifiers); //TODO: use a LocalDeclaration subclass that can record and interpret the storage class
      Expression/*?*/ initializer = null;
      InitializedDeclarator/*?*/ initializedDeclarator = declarator as InitializedDeclarator;
      if (initializedDeclarator != null) {
        initializer = initializedDeclarator.InitialValue;
        var arrayOrStructureInitializer = initializer as VccInitializerBase;
        if (arrayOrStructureInitializer != null) {
          arrayOrStructureInitializer.arrayTypeExpression = localType as VccArrayTypeExpression;
          arrayOrStructureInitializer.structureTypeExpression = localType as VccNamedTypeExpression;
        }
      }
      List<LocalDeclaration> declarations = new List<LocalDeclaration>(1);
      // if we have local function declaration, we create a global mangled function declaration
      VccFunctionTypeExpression/*?*/ cFuncTypeExp = localType as VccFunctionTypeExpression;
      if (cFuncTypeExp != null) {
        Token storageClass = GetStorageClassToken(specifiers); 
        TypeMemberVisibility visibility = storageClass == Token.Static ? TypeMemberVisibility.Assembly : TypeMemberVisibility.Public;
               
        bool isExternal = storageClass == Token.Extern;
        List<ParameterDeclaration> parameters = new List<ParameterDeclaration>();
        foreach (ParameterDeclaration pd in cFuncTypeExp.parameters) {
          parameters.Add(pd);
        }
        // create a unique mangled function declaration at the top level
        FunctionDeclaration mangledFunc = 
          new FunctionDeclaration(cFuncTypeExp.AcceptsExtraArguments, specifiers, isExternal, 
          cFuncTypeExp.CallingConvention, visibility, cFuncTypeExp.ReturnType, 
          new NameDeclaration(this.GetNameFor(cFuncTypeExp.Name.Value + cFuncTypeExp.GetHashCode()), cFuncTypeExp.SourceLocation),
          null, parameters, slb);
        
        // this.currentTypeMembers shouldnt be null.
        if (this.currentTypeMembers != null) {
          this.currentTypeMembers.Add(mangledFunc);
        }
        declarations.Add(new VccLocalFunctionDeclaration(declarator.Identifier, initializer, specifiers, slb, mangledFunc));
      } else {
        declarations.Add(new VccLocalDeclaration(declarator.Identifier, initializer, specifiers, slb));
      }


      //TODO: There may also be constant pointers - this would need to be dealt with differently
      if (TypeExpressionHasPointerType(localType) != null)
        isConstant = false;

      statements.Add(new LocalDeclarationsStatement(isConstant, false, false, localType, declarations, slb));
    }

    private void AddTypeDeclarationMember(List<Specifier> specifiers, Declarator declarator, List<ITypeDeclarationMember> typeMembers) {
      SourceLocationBuilder slb = new SourceLocationBuilder(declarator.SourceLocation);
      if (specifiers.Count > 0) slb.UpdateToSpan(specifiers[0].SourceLocation);
      TypeExpression memberType = this.GetTypeExpressionFor(specifiers, declarator);
      bool isConstant;
      bool isVolatile;
      LookForConstAndVolatile(specifiers, out isConstant, out isVolatile);
      FieldDeclaration.Flags flags = 0;
      if (isConstant) flags |= FieldDeclaration.Flags.ReadOnly;
      if (isVolatile) flags |= FieldDeclaration.Flags.Volatile;
      // C's const will be treated as readonly. When we are inside a type, isConst is never true because you 
      // cannot initialize it.
      Token sct = GetStorageClassToken(specifiers);
      if (sct == Token.Typedef) {
        this.typedefExpressions[declarator.Identifier.Value] = memberType;
        var typedefDecl = new TypedefDeclaration(memberType, declarator.Identifier, specifiers, slb);
        this.typedefDecls[declarator.Identifier.Value] = typedefDecl;
        typeMembers.Add(typedefDecl);
      } else if (sct == Token.Specification) {
        Expression/*?*/ initializer = null;
        InitializedDeclarator/*?*/ initializedDeclarator = declarator as InitializedDeclarator;
        if (initializedDeclarator != null) initializer = initializedDeclarator.InitialValue;
        if (this.currentTypeName != null) {
          FieldDefinition specField = new FieldDefinition(specifiers, flags, memberType, declarator.Identifier, initializer, slb);
          if (this.currentSpecificationFields == null) this.currentSpecificationFields = new List<FieldDeclaration>();
          this.currentSpecificationFields.Add(specField);
        } else {
          if (initializer != null && IsAxiom(specifiers)) {
            if (this.currentTypeInvariants == null) this.currentTypeInvariants = new List<TypeInvariant>();
            this.currentTypeInvariants.Add(new TypeInvariant(declarator.Identifier, new CheckedExpression(initializer, initializer.SourceLocation), true, slb));
          } else {
            if (this.currentSpecificationFields == null) this.currentSpecificationFields = new List<FieldDeclaration>();
            FieldDeclaration glob = new GlobalVariableDeclaration(flags, TypeMemberVisibility.Public, memberType, declarator.Identifier, initializer, slb);
            this.currentSpecificationFields.Add(glob);
          }
        }
      } else {
        //TODO: complain if sct is Auto or Register.
        //TODO: complain if memberType is function
        Expression/*?*/ initializer = null;
        InitializedDeclarator/*?*/ initializedDeclarator = declarator as InitializedDeclarator;
        if (initializedDeclarator != null)
          initializer = initializedDeclarator.InitialValue;
        else if (sct == Token.Extern || sct == Token.Static) {
          TypeMemberVisibility visibility = sct == Token.Static ? TypeMemberVisibility.Other : TypeMemberVisibility.Public;
          typeMembers.Add(new GlobalVariableDeclaration(flags, visibility, memberType, declarator.Identifier, null, slb));
          return;
        }
        if (this.currentTypeName != null) {
          if (sct == Token.Static) flags |= FieldDeclaration.Flags.Static;
          AnonymousFieldDeclarator/*?*/ anonymousFieldDeclarator = declarator as AnonymousFieldDeclarator;
          if (anonymousFieldDeclarator != null) {
            typeMembers.Add(new AnonymousFieldDefinition(flags, memberType, anonymousFieldDeclarator.Identifier));
          } else {
            BitfieldDeclarator/*?*/ bitFieldDeclarator = declarator as BitfieldDeclarator;
            if (bitFieldDeclarator != null)
              typeMembers.Add(new BitFieldDefinition(specifiers, bitFieldDeclarator.FieldSize, flags, memberType, declarator.Identifier, initializer, slb));
            else
              typeMembers.Add(new FieldDefinition(specifiers, flags, memberType, declarator.Identifier, initializer, slb));
          }
        } else {
          flags |= FieldDeclaration.Flags.Static; //global fields are always static
          //at the global level, the static modifier means visible only inside the current compilation unit (file)
          TypeMemberVisibility visibility = sct == Token.Static ? TypeMemberVisibility.Other : TypeMemberVisibility.Public;
          typeMembers.Add(new GlobalVariableDeclaration(flags, visibility, memberType, declarator.Identifier, initializer, slb));
        }
      }
    }

    private static CallingConvention GetCallingConvention(List<Specifier>/*?*/ specifiers, bool acceptsExtraArguments) {
      if (acceptsExtraArguments) return CallingConvention.ExtraArguments;
      if (specifiers != null) {
        foreach (Specifier specifier in specifiers) {
          FunctionSpecifier/*?*/ fs = specifier as FunctionSpecifier;
          if (fs != null) {
            switch (fs.Token) {
              case Token.Cdecl: return CallingConvention.C;
              case Token.Stdcall: return CallingConvention.Standard;
              case Token.Fastcall: return CallingConvention.FastCall;
            }
          }
        }
      }
      return CallingConvention.Default;
    }

    private static Token GetStorageClassToken(List<Specifier> specifiers) {
      Token result = Token.None;
      foreach (Specifier specifier in specifiers) {
        StorageClassSpecifier/*?*/ scs = specifier as StorageClassSpecifier;
        if (scs != null) {
          //TODO: give error if result != Token.None;
          result = scs.Token;
        }
      }
      return result;
    }

    private static TypedefNameSpecifier/*?*/ GetTypedefNameSpecifier(List<Specifier> specifiers) {
      TypedefNameSpecifier/*?*/ result = null;
      foreach (Specifier specifier in specifiers) {
        result = specifier as TypedefNameSpecifier;
        if (result != null) return result;
      }
      return result;
    }

    private static bool IsAxiom(List<Specifier> specifiers) {
      foreach (Specifier specifier in specifiers) {
        PrimitiveTypeSpecifier/*?*/ pts = specifier as PrimitiveTypeSpecifier;
        if (pts != null && pts.Token == Token.Axiom) return true;
      }
      return false;
    }

    private static bool IsInline(List<Specifier> specifiers) {
      foreach (Specifier specifier in specifiers) {
        FunctionSpecifier/*?*/ fs = specifier as FunctionSpecifier;
        if (fs != null && fs.Token == Token.Inline) return true;
      }
      return false;
    }

    /// <summary>
    /// For a local variable of pointer type, see if it is const or volatile. 
    /// If volatile appears in the specifiers, then it is volatile;
    /// If const appears after the last type specifier (primitive, structured, or typedef), 
    /// then the local pointer is "constant".
    /// TODO: to fully support the const specifier, we need a more expressive type system in the framework. 
    /// </summary>
    /// <param name="specifiers"></param>
    /// <param name="isConstant"></param>
    /// <param name="isVolatile"></param>
    private static void LookForConstAndVolatileForLocalPointer(List<Specifier> specifiers, out bool isConstant, out bool isVolatile) {
      isConstant = false;
      isVolatile = false;
      foreach (Specifier specifier in specifiers) {
        if (specifier is CompositeTypeSpecifier || specifier is PrimitiveTypeSpecifier ||
          specifier is FunctionSpecifier || specifier is TypedefNameSpecifier) {
          if (isConstant) isConstant = false;
          continue;
        }
        TypeQualifier/*?*/ tqual = specifier as TypeQualifier;
        if (tqual == null) continue;
        if (tqual.Token == Token.Const) isConstant = true;
        else if (tqual.Token == Token.Volatile) isVolatile = true;
      }
    }

    private void LookForConstAndVolatile(List<Specifier> specifiers, out bool isConstant, out bool isVolatile) {
      isConstant = false;
      isVolatile = false;
      foreach (Specifier specifier in specifiers) {
        TypeQualifier/*?*/ tqual = specifier as TypeQualifier;
        if (tqual != null) {
          if (tqual.Token == Token.Const) isConstant = true;
          else if (tqual.Token == Token.Volatile) isVolatile = true;
          continue;
        }
        TypedefNameSpecifier tdn = specifier as TypedefNameSpecifier;
        if (tdn != null) {
          TypedefDeclaration typedefDecl;
          if (this.typedefDecls.TryGetValue(tdn.TypedefName.Name.Value, out typedefDecl)) {
            if (typedefDecl.IsConst) isConstant = true;
            if (typedefDecl.IsVolatile) isVolatile = true;

            if (this.TypeExpressionHasPointerType(typedefDecl.Type) != null) {
              return; //specifiers after a typedef belong to the field
            }

          }
        }
      }
    }

    private TypeExpression GetTypeExpressionFor(IEnumerable<Specifier> specifiers, Declarator declarator)
    {
      return this.GetTypeExpressionFor(specifiers, declarator, null);
    }

    private TypeExpression GetTypeExpressionFor(IEnumerable<Specifier> specifiers, Declarator declarator, Expression/*?*/ initializer) {
      InitializedDeclarator/*?*/ initialized = declarator as InitializedDeclarator;
      if (initialized != null)
        return this.GetTypeExpressionFor(specifiers, initialized.Declarator, initialized.InitialValue);
      else {
        return this.GetTypeExpressionFor(this.GetTypeExpressionFor(specifiers, declarator as IdentifierDeclarator), declarator, initializer);
      }
    }

    private TypeExpression GetTypeExpressionFor(TypeExpression elementType, Declarator declarator) 
    {
      return this.GetTypeExpressionFor(elementType, declarator, null);
    }

    private TypeExpression GetTypeExpressionFor(TypeExpression elementType, Declarator declarator, Expression/*?*/ initializer) {
      SourceLocationBuilder slb = new SourceLocationBuilder(elementType.SourceLocation);
      ArrayDeclarator/*?*/ array = declarator as ArrayDeclarator;
      if (array != null) {
        slb.UpdateToSpan(array.SourceLocation);
        Declarator nestedDeclarator = array.ElementType;
        PointerDeclarator/*?*/ pointerDeclarator = nestedDeclarator as PointerDeclarator;
        if (pointerDeclarator != null) nestedDeclarator = pointerDeclarator.Declarator;
        Expression/*?*/ arraySize = array.ArraySize;
        if (arraySize == null) {
          var vcInitializer = initializer as VccInitializerBase;         
          if (initializer != null)
            arraySize = new CompileTimeConstant(vcInitializer.ExpressionCount, array.SourceLocation);
        }
        if (arraySize is TypeExpression) {
          Expression mapRef = NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.nameTable, "Map");
          elementType = new GenericTypeInstanceExpression(new NamedTypeExpression(mapRef), new TypeExpression[] { (TypeExpression)arraySize, elementType }, slb);
        } else
          elementType = new VccArrayTypeExpression(elementType, arraySize, slb);
        TypeExpression result = this.GetTypeExpressionFor(elementType, nestedDeclarator);
        result = AddIndirectionsToType(result, pointerDeclarator, slb);
        return result;
      }
      FunctionDeclarator/*?*/ function = declarator as FunctionDeclarator;
      if (function != null) {
        slb.UpdateToSpan(declarator.SourceLocation);
        Declarator nestedDeclarator = function.FunctionName;
        PointerDeclarator/*?*/ pointerDeclarator = nestedDeclarator as PointerDeclarator;
        if (pointerDeclarator != null) nestedDeclarator = pointerDeclarator.Declarator;
        ArrayDeclarator/*?*/ arrayDeclarator = nestedDeclarator as ArrayDeclarator;
        if (arrayDeclarator != null) nestedDeclarator = arrayDeclarator.ElementType;
        TypeExpression returnType = this.GetTypeExpressionFor(elementType, nestedDeclarator);
        bool acceptsExtraArguments;
        List<ParameterDeclaration> parameters = this.ConvertToParameterDeclarations(function.Parameters, out acceptsExtraArguments);
        CallingConvention callingConvention = GetCallingConvention(function.Specifiers, acceptsExtraArguments);
        TypeExpression result = new VccFunctionTypeExpression(acceptsExtraArguments, callingConvention, returnType, 
          function.FunctionName.Identifier, parameters, function, slb);
        result = AddIndirectionsToType(result, pointerDeclarator, slb);
        if (arrayDeclarator != null) {
          SourceLocationBuilder aslb = new SourceLocationBuilder(arrayDeclarator.SourceLocation);
          aslb.UpdateToSpan(slb);
          result = new VccArrayTypeExpression(result, arrayDeclarator.ArraySize, aslb);
        }
        return result;
      }
      PointerDeclarator/*?*/ pointer = declarator as PointerDeclarator;
      elementType = AddIndirectionsToType(elementType, pointer, slb);
      if (pointer != null)
        return this.GetTypeExpressionFor(elementType, pointer.Declarator);
      return elementType;
    }

    private static TypeExpression AddIndirectionsToType(TypeExpression type, PointerDeclarator pointerDeclarator, SourceLocationBuilder slb) {
      TypeExpression result = type;
      if (pointerDeclarator != null) {
        foreach (Pointer p in pointerDeclarator.Pointers) {
          SourceLocationBuilder pslb = new SourceLocationBuilder(p.SourceLocation);
          pslb.UpdateToSpan(slb);
          result = new VccPointerTypeExpression(result, p.Qualifiers, pslb);
        }
      }
      return result;
    }

    private TypeExpression GetTypeExpressionFor(IEnumerable<Specifier> specifiers, IdentifierDeclarator/*?*/ declarator) {
      TypeExpression/*?*/ result = this.TryToGetTypeExpressionFor(specifiers);
      if (result != null) return result;
      ISourceLocation/*?*/ errorLocation = null;
      if (declarator != null)
        errorLocation = declarator.SourceLocation;
      else {
        foreach (Specifier specifier in specifiers) errorLocation = specifier.SourceLocation;
      }
      if (errorLocation != null)
        this.HandleError(errorLocation, Error.UnexpectedToken, errorLocation.Source);
      return TypeExpression.For(Dummy.Type);
    }

    private TypeExpression/*?*/ TryToGetTypeExpressionFor(IEnumerable<Specifier> specifiers) {
      TypeExpression/*?*/ result = null;
      PrimitiveTypeSpecifier/*?*/ sign = null;
      PrimitiveTypeSpecifier/*?*/ length = null;
      PrimitiveTypeSpecifier/*?*/ primitiveType = null;
      List<TypeQualifier> typeQualifiers = null;
      foreach (Specifier specifier in specifiers) {
        CompositeTypeSpecifier/*?*/ cts = specifier as CompositeTypeSpecifier;
        if (cts != null) {
          //TODO: if (result != null || sign != null || length != null || primitiveType != null) Error;
          result = cts.TypeExpression;
        }
        TypeQualifier/*?*/ tq = specifier as TypeQualifier;
        if (tq != null && result != null) {
          TypeExpression/*?*/ elementType = this.TypeExpressionHasPointerType(result);
          if (elementType != null) {
            if (typeQualifiers == null) {
              typeQualifiers = new List<TypeQualifier>(2);
              result = new VccPointerTypeExpression(elementType, typeQualifiers, result.SourceLocation);
            }
            typeQualifiers.Add(tq);
          }
        }
        TypedefNameSpecifier/*?*/ tdns = specifier as TypedefNameSpecifier;
        if (tdns != null) {
          //TODO: if (result != null || sign != null || length != null || primitiveType != null) Error;
          TypeExpression typeDefExpression;
          if (tdns.TypedefName.ToString() == "_vcc_obj_t") {
            Expression typePtrRef = NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.nameTable, "TypedPtr");
            result = new VccNamedTypeExpression(typePtrRef);
          } else if (tdns.TypedefName.ToString() == "_vcc_integer_t") {
            Expression bigIntRef = NamespaceHelper.CreateInSystemDiagnosticsContractsCodeContractExpr(this.nameTable, "BigInt");
            result = new VccNamedTypeExpression(bigIntRef);
          } else {
            if (this.typedefExpressions.TryGetValue(tdns.TypedefName.ToString(), out typeDefExpression)) {
              NamedTypeExpression namedTypeExpression = typeDefExpression as NamedTypeExpression;
              if (namedTypeExpression != null) {
                QualifiedName qName = namedTypeExpression.Expression as QualifiedName;
                if (qName != null && qName.Qualifier is AliasQualifiedName && qName.SimpleName.Name == this.nameTable.Void) {
                  primitiveType = new PrimitiveTypeSpecifier(Token.Void, tdns.SourceLocation);
                  continue;
                }
              }
            }
            result = new VccNamedTypeExpression(tdns.TypedefName, false);
          }
        }
        ScopedTypeNameSpecifier/*?*/ stns = specifier as ScopedTypeNameSpecifier;
        if (stns != null) {
          result = new VccScopedTypeExpression(stns.ScopedName);
        }
        PrimitiveTypeSpecifier/*?*/ pts = specifier as PrimitiveTypeSpecifier;
        if (pts != null) {
          switch (pts.Token) {
            case Token.Signed:
            case Token.Unsigned:
              //TODO: error if sign != null || result != null;
              sign = pts;
              break;
            case Token.Short:
              //TODO: error if length != null || result != null;;
              length = pts;
              break;
            case Token.Long:
              //TODO: error if result != null;
              if (length == null)
                length = pts;
              else {
                if (length.Token == Token.Long) {
                  SourceLocationBuilder slb = new SourceLocationBuilder(length.SourceLocation);
                  slb.UpdateToSpan(pts.SourceLocation);
                  length = new PrimitiveTypeSpecifier(Token.Int64, slb);
                } else {
                  //TODO: error
                }
              }
              break;
            case Token.Axiom:
            case Token.Char:
            case Token.Int:
            case Token.Float:
            case Token.Double:
            case Token.Void:
            case Token.Bool:
            case Token.Int8:
            case Token.Int16:
            case Token.Int32:
            case Token.Int64:
              //TODO: error is primitiveType != null || result != null;
              primitiveType = pts;
              break;
            default:
              //^ assert false;
              break;
          }
        }
      }
      if (result != null) return result;
      if (primitiveType != null) {
        SourceLocationBuilder slb = new SourceLocationBuilder(primitiveType.SourceLocation);
        if (length != null) {
          slb.UpdateToSpan(length.SourceLocation);
          if (length.Token == Token.Short) {
            //TODO: error if primitive type != int
          } else {
            //TODO: error if primitive type != int and != double and != float;
            //only warn if primitive type is float
          }
        }
        if (sign != null) slb.UpdateToSpan(sign.SourceLocation);
        if (sign == null || sign.Token == Token.Signed) {
          switch (primitiveType.Token) {
            case Token.Char:
              return this.GetTypeExpressionFor(TypeCode.SByte, slb);
            case Token.Float:
              //TODO: error if signed != null;
              if (length != null && length.Token == Token.Long) {
                //TODO: warning
                return this.GetTypeExpressionFor(TypeCode.Double, slb);
              } else
                return this.GetTypeExpressionFor(TypeCode.Single, slb);
            case Token.Double:
              //TODO: error if signed != null;
              return this.GetTypeExpressionFor(TypeCode.Double, slb);
            case Token.Void:
              //TODO: error if signed != null;
              return this.GetTypeExpressionFor(TypeCode.Empty, slb);
            case Token.Bool:
              //TODO: error if signed != null;
              return this.GetTypeExpressionFor(TypeCode.Boolean, slb);
            case Token.Int8:
              return this.GetTypeExpressionFor(TypeCode.SByte, slb);
            case Token.Int16:
              return this.GetTypeExpressionFor(TypeCode.Int16, slb);
            case Token.Int32:
              return this.GetTypeExpressionFor(TypeCode.Int32, slb);
            case Token.Int64:
              return this.GetTypeExpressionFor(TypeCode.Int64, slb);
          }
        } else {
          //unsigned
          switch (primitiveType.Token) {
            case Token.Char:
              return this.GetTypeExpressionFor(TypeCode.Byte, slb);
            case Token.Float:
              //TODO: error
              if (length != null && length.Token == Token.Long) {
                //TODO: warning
                return this.GetTypeExpressionFor(TypeCode.Double, slb);
              } else
                return this.GetTypeExpressionFor(TypeCode.Single, slb);
            case Token.Double:
              //TODO: error
              return this.GetTypeExpressionFor(TypeCode.Double, slb);
            case Token.Void:
              //TODO: error
              return this.GetTypeExpressionFor(TypeCode.Empty, slb);
            case Token.Bool:
              //TODO: error
              return this.GetTypeExpressionFor(TypeCode.Boolean, slb);
            case Token.Int8:
              return this.GetTypeExpressionFor(TypeCode.Byte, slb);
            case Token.Int16:
              return this.GetTypeExpressionFor(TypeCode.UInt16, slb);
            case Token.Int32:
              return this.GetTypeExpressionFor(TypeCode.UInt32, slb);
            case Token.Int64:
              return this.GetTypeExpressionFor(TypeCode.UInt64, slb);
          }
        }
      }
      //get here if primitive type is int or has not been specified
      if (sign != null) {
        if (sign.Token == Token.Unsigned) {
          if (length == null) return this.GetTypeExpressionFor(TypeCode.UInt32, sign.SourceLocation);
          SourceLocationBuilder slb = new SourceLocationBuilder(sign.SourceLocation);
          slb.UpdateToSpan(length.SourceLocation);
          if (primitiveType != null) slb.UpdateToSpan(primitiveType.SourceLocation);
          switch (length.Token) {
            case Token.Short: return this.GetTypeExpressionFor(TypeCode.UInt16, slb);
            case Token.Long: return this.GetTypeExpressionFor(TypeCode.UInt32, slb);
            case Token.Int64: return this.GetTypeExpressionFor(TypeCode.UInt64, slb);
          }
        }else if (sign.Token == Token.Signed) {
          if (length == null) return this.GetTypeExpressionFor(TypeCode.Int32, sign.SourceLocation);
          SourceLocationBuilder slb = new SourceLocationBuilder(sign.SourceLocation);
          slb.UpdateToSpan(length.SourceLocation);
          if (primitiveType != null) slb.UpdateToSpan(primitiveType.SourceLocation);
          switch (length.Token) {
            case Token.Short: return this.GetTypeExpressionFor(TypeCode.Int16, slb);
            case Token.Long: return this.GetTypeExpressionFor(TypeCode.Int32, slb);
            case Token.Int64: return this.GetTypeExpressionFor(TypeCode.Int64, slb);
          }
        }
      }
      if (length != null) {
        SourceLocationBuilder slb = new SourceLocationBuilder(length.SourceLocation);
        if (primitiveType != null) slb.UpdateToSpan(primitiveType.SourceLocation);
        switch (length.Token) {
          case Token.Short: return this.GetTypeExpressionFor(TypeCode.Int16, slb);
          case Token.Long: return this.GetTypeExpressionFor(TypeCode.Int32, slb);
          case Token.Int64: return this.GetTypeExpressionFor(TypeCode.Int64, slb);
        }
      }
      if (primitiveType != null) {
        //^ assume primitiveType.Token == Token.Int;
        return this.GetTypeExpressionFor(TypeCode.Int32, primitiveType.SourceLocation);
      }
      return null;
    }

    private Declarator ParseDeclarator(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      //^ ensures result is IdentifierDeclarator || result is BitfieldDeclarator || result is ArrayDeclarator || result is FunctionDeclarator ||
      //^   result is PointerDeclarator || result is AbstractMapDeclarator || result is InitializedDeclarator;
    {
      return this.ParseDeclarator(followers, false);
    }


    private Declarator ParseDeclarator(TokenSet followers, bool requireIdentifier)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      //^ ensures result is IdentifierDeclarator || result is BitfieldDeclarator || result is ArrayDeclarator || result is FunctionDeclarator ||
      //^   result is PointerDeclarator || result is AbstractMapDeclarator || result is InitializedDeclarator;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      List<Pointer> pointers = this.ParsePointers();
      Declarator result;
      List<Specifier>/*?*/ specifiers = null;
      if (this.currentToken == Token.LeftParenthesis){
        this.GetNextToken();
        if (!Parser.DeclarationStart[this.currentToken])
          specifiers = this.ParseSpecifiers(new List<INamespaceDeclarationMember>(), null, followers|Parser.DeclaratorStart|Token.RightParenthesis|Token.Semicolon); 
        result = this.ParseDeclarator(followers|Token.RightParenthesis, requireIdentifier);
        this.Skip(Token.RightParenthesis);
      } else if (this.currentToken == Token.Colon) {
        result = this.ParseBitfieldDeclarator(null, followers|Token.LeftBracket|Token.LeftParenthesis);
      } else {
        result = new IdentifierDeclarator(this.ParseNameDeclaration(requireIdentifier));
        if (this.currentToken == Token.Colon)
          result = this.ParseBitfieldDeclarator(result, followers|Token.LeftBracket|Token.LeftParenthesis);
      }
      while (this.currentToken == Token.LeftBracket || this.currentToken == Token.LeftParenthesis) {
        if (this.currentToken == Token.LeftBracket)
          result = this.ParseArrayDeclarator(result, followers|Token.LeftBracket|Token.LeftParenthesis|Token.Assign);
        else
          result = this.ParseFunctionDeclarator(result, followers|Token.LeftBracket|Token.LeftParenthesis|Token.Assign);
      }
      if (pointers.Count > 0) {
        slb.UpdateToSpan(result.SourceLocation);
        result = new PointerDeclarator(pointers, result, slb);
      }
      if (this.currentToken == Token.Assign)
        result = this.ParseInitializedDeclarator(result, followers);
      else if (specifiers != null && specifiers.Count > 0 && result is FunctionDeclarator)
        ((FunctionDeclarator)result).Specifiers = specifiers;
      else
        this.SkipTo(followers);
      return result;
    }

    private List<TemplateParameterDeclarator>/*?*/ ParseTemplateParameters(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      if (this.currentToken != Token.Template) return null;
      this.typedefExpressions = new Dictionary<string, TypeExpression>(this.typedefExpressions);
      this.GetNextToken();
      this.Skip(Token.LessThan);
      List<TemplateParameterDeclarator> result = new List<TemplateParameterDeclarator>();
      while (true) {
        SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
        this.Skip(Token.Typename);
        NameDeclaration parName = this.ParseNameDeclaration(false);
        slb.UpdateToSpan(parName.SourceLocation);
        result.Add(new TemplateParameterDeclarator(parName, slb));
        SimpleName simpleName = new SimpleName(parName.Name, slb, false);
        this.typedefExpressions[parName.Value] = new VccTemplateTypeParameterExpression(simpleName);
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      result.TrimExcess();
      this.SkipOverTo(Token.GreaterThan, followers);
      return result;
    }

    private InitializedDeclarator ParseInitializedDeclarator(Declarator declarator, TokenSet followers)       
      //^ requires this.currentToken == Token.Assign;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(declarator.SourceLocation);
      this.GetNextToken();
      Expression initialValue = this.ParseInitializer(followers);
      InitializedDeclarator result = new InitializedDeclarator(declarator, initialValue, slb);
      slb.UpdateToSpan(result.InitialValue.SourceLocation);
      this.SkipTo(followers);
      return result;
    }

    private VccDesignatorExpressionPair ParseDesignatorExpressionPair(TokenSet followers) {
      this.Skip(Token.Dot);
      SimpleName designator = this.ParseSimpleName(followers | Token.Assign);
      this.Skip(Token.Assign);
      Expression expr = this.ParseExpression(followers);
      return new VccDesignatorExpressionPair(designator, expr);
    }

    private Expression ParseInitializerWithDesignators(TokenSet followers, SourceLocationBuilder slb)
      //^ requires this.currentToken == Token.Dot
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {

      List<VccDesignatorExpressionPair> pairs = new List<VccDesignatorExpressionPair>();
      TokenSet followersOrCommaOrRightBrace = followers|Token.Comma|Token.RightBrace;
      pairs.Add(this.ParseDesignatorExpressionPair(followersOrCommaOrRightBrace));
      while (this.currentToken == Token.Comma) {
        this.GetNextToken();
        pairs.Add(this.ParseDesignatorExpressionPair(followersOrCommaOrRightBrace));
      }
      
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      var result = new VccInitializerWithDesignators(pairs, slb);
      this.SkipOverTo(Token.RightBrace, followers);
      return result;
    }

    private Expression ParseInitializer(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      if (this.currentToken != Token.LeftBrace) return this.ParseExpression(followers);
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      TokenSet followersOrCommaOrRightBrace = followers|Token.Comma|Token.RightBrace;
      if (this.currentToken == Token.Dot) return ParseInitializerWithDesignators(followers, slb);

      List<Expression> expressions = new List<Expression>();
      if (this.currentToken != Token.RightBrace) {
        Expression expression = this.ParseInitializer(followersOrCommaOrRightBrace);
        expressions.Add(expression);
        while (this.currentToken == Token.Comma) {
          this.GetNextToken();
          if (this.currentToken != Token.RightBrace) {
            expression = this.ParseInitializer(followersOrCommaOrRightBrace);
            expressions.Add(expression);
          }
        }
      }
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      VccInitializer result = new VccInitializer(expressions, slb);
      this.SkipOverTo(Token.RightBrace, followers);
      return result;
    }

    private BitfieldDeclarator ParseBitfieldDeclarator(Declarator/*?*/ fieldDeclarator, TokenSet followers) 
      //^ requires this.currentToken == Token.Colon;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      if (fieldDeclarator == null)
        fieldDeclarator = new IdentifierDeclarator(new NameDeclaration(this.ParseNameDeclaration(false), this.scanner.SourceLocationOfLastScannedToken));
      SourceLocationBuilder slb = new SourceLocationBuilder(fieldDeclarator.SourceLocation);
      this.GetNextToken();
      BitfieldDeclarator result = new BitfieldDeclarator(fieldDeclarator, this.ParseExpression(followers), slb);
      slb.UpdateToSpan(result.FieldSize.SourceLocation);
      this.SkipTo(followers);
      return result;
    }

    private List<Pointer> ParsePointers() {
      List<Pointer> result = new List<Pointer>(2);
      while (this.currentToken == Token.Multiply) {
        result.Add(this.ParsePointer());
      }
      result.TrimExcess();
      return result;
    }

    private Pointer ParsePointer()
      //^ requires this.currentToken == Token.Multiply;
    {
      SourceLocationBuilder sloc = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      bool isSpec = this.currentToken == Token.BitwiseXor;
      this.GetNextToken();
      List<TypeQualifier>/*?*/ qualifiers = this.ParseTypeQualifiers();
      return new Pointer(qualifiers, sloc);
    }

    private ArrayDeclarator ParseArrayDeclarator(Declarator elementTypeAndName, TokenSet followers)
      //^ requires this.currentToken == Token.LeftBracket;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(elementTypeAndName.SourceLocation);
      this.Skip(Token.LeftBracket);
      Expression/*?*/ arraySize = null;
      if (this.currentToken != Token.RightBracket)
        if (this.CurrentTokenStartsTypeExpression())
          arraySize = this.ParseTypeExpression(followers|Token.RightBracket);
        else
          arraySize = this.ParseExpression(followers|Token.RightBracket);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.SkipOverTo(Token.RightBracket, followers);
      ArrayDeclarator result = new ArrayDeclarator(elementTypeAndName, arraySize, slb);
      return result;
    }

    private FunctionDeclarator ParseFunctionDeclarator(Declarator functionName, TokenSet followers)
      //^ requires this.currentToken == Token.LeftParenthesis;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(functionName.SourceLocation);
      List<Parameter> parameters = new List<Parameter>();
      this.Skip(Token.LeftParenthesis);
      if (this.currentToken != Token.RightParenthesis) {
        while (true) {
          parameters.Add(this.ParseParameter(followers|Token.Comma|Token.RightParenthesis));
          if (this.currentToken != Token.Comma && this.currentToken != Token.Specification) break;
          if (this.currentToken == Token.Comma) this.GetNextToken(); // do not skip Specification, it is added as a type modifier
        }
      }
      // If the declarator is a pointer to a function declarator, exchange the
      // parameters:
      // int (*func(void))(int) is a function that takes void and return an int->int. 
      parameters = this.OutermostFuncDeclaratorAdjustIfNecessary(functionName, parameters);

      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      FunctionDeclarator result = new FunctionDeclarator(functionName, parameters, slb);
      this.Skip(Token.RightParenthesis);
      this.ParseFunctionOrBlockContract(result.Contract, followers);
      return result;
    }

    /// <summary>
    /// When a function that may return a function type, the order of the parameters needs to
    /// be reversed. Given a possible chain of declarators, in which the chain of parameter lists
    /// is in the right order, put the new outer parameter list into the inner most, and shift the
    /// chain of parameter lists outside.
    /// For example:
    /// inner (1) outer (2) -> inner (2) outer (1)
    /// innermost (1) inner (2) outer (3) 0> innermost (3) inner (1) outer (2)
    /// </summary>
    /// <param name="functionName">the function name declarator of the function declarator of concern; the function
    /// name declarator could represent a chain of function types; each link of the chain may be a pointer, or an array.</param>
    /// <param name="currentParameters">the outermost parameter list from parsing</param>
    /// <returns>the new outermost parameter list</returns>
    private List<Parameter> OutermostFuncDeclaratorAdjustIfNecessary(Declarator functionName, List<Parameter> currentParameters) {
      PointerDeclarator/*?*/ pointerDeclarator = functionName as PointerDeclarator;
      ArrayDeclarator/*?*/ arrayDeclarator = functionName as ArrayDeclarator;
      FunctionDeclarator/*?*/ functionDeclarator = functionName as FunctionDeclarator; 
      if (pointerDeclarator != null)
        functionDeclarator = pointerDeclarator.Declarator as FunctionDeclarator;
      if (arrayDeclarator != null)
        functionDeclarator = arrayDeclarator.ElementType as FunctionDeclarator;
      if (functionDeclarator != null) {
        List<Parameter> temp = functionDeclarator.Parameters;
        List<Parameter> nextToOuttermost = OutermostFuncDeclaratorAdjustIfNecessary(functionDeclarator.FunctionName, currentParameters);
        functionDeclarator.ResetParameters(nextToOuttermost);
        return temp;
      } else {
        return currentParameters;
      }
    }

    private void ParseFunctionOrBlockContract(FunctionOrBlockContract contract, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      TokenSet followersOrContractStart = followers|Parser.ContractStart;
      while (Parser.ContractStart[this.currentToken]){
        switch (this.currentToken) {
          case Token.Allocates: this.ParseAllocates(contract, followersOrContractStart); break;
          case Token.Ensures: this.ParseEnsures(contract, followersOrContractStart); break;
          case Token.Frees: this.ParseFrees(contract, followersOrContractStart); break;
          case Token.Reads: this.ParseReads(contract, followersOrContractStart); break;
          case Token.Requires: this.ParseRequires(contract, followersOrContractStart); break;
          case Token.Writes: this.ParseWrites(contract, followersOrContractStart); break;
        }
      }
      this.SkipTo(followers);
    }

    private void ParseAllocates(FunctionOrBlockContract contract, TokenSet followers)
      //^ requires this.currentToken == Token.Allocates;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      this.resultIsAKeyword = true;
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      while (true) {
        Expression expr = this.ParseExpressionWithCheckedDefault(followers|Token.Comma|Token.RightParenthesis);
        contract.AddAllocates(expr);
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      this.SkipOverTo(Token.RightParenthesis, followers);
      this.resultIsAKeyword = false;
    }

    private void ParseEnsures(FunctionOrBlockContract contract, TokenSet followers)
      //^ requires this.currentToken == Token.Ensures;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      this.resultIsAKeyword = true;
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      Expression condition = this.ParseExpressionWithCheckedDefault(followers|Token.RightParenthesis);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.Skip(Token.RightParenthesis);
      Postcondition postCondition = new Postcondition(condition, slb);
      contract.AddPostcondition(postCondition);
      this.resultIsAKeyword = false;
    }

    private void ParseFrees(FunctionOrBlockContract contract, TokenSet followers)
      //^ requires this.currentToken == Token.Frees;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      this.resultIsAKeyword = true;
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      while (true) {
        Expression expr = this.ParseExpressionWithCheckedDefault(followers|Token.Comma|Token.RightParenthesis);
        contract.AddFrees(expr);
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      this.SkipOverTo(Token.RightParenthesis, followers);
      this.resultIsAKeyword = false;
    }

    private void ParseReads(FunctionOrBlockContract contract, TokenSet followers)
      //^ requires this.currentToken == Token.Reads;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      while (true) {
        Expression expr = this.ParseExpressionWithCheckedDefault(followers|Token.Comma|Token.RightParenthesis);
        contract.AddReads(expr);
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      this.SkipOverTo(Token.RightParenthesis, followers);
    }

    private void ParseRequires(FunctionOrBlockContract contract, TokenSet followers)
      //^ requires this.currentToken == Token.Requires;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      Expression condition = this.ParseExpressionWithCheckedDefault(followers|Token.RightParenthesis);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.Skip(Token.RightParenthesis);
      Precondition preCondition = new Precondition(condition, null, slb);
      contract.AddPrecondition(preCondition);
    }

    private void ParseWrites(List<Expression> writes, TokenSet followers)
      //^ requires this.currentToken == Token.Writes;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      while (true) {
        Expression expr = this.ParseExpressionWithCheckedDefault(followers|Token.Comma|Token.RightParenthesis);
        writes.Add(expr);
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      this.SkipOverTo(Token.RightParenthesis, followers);
    }

    private void ParseWrites(FunctionOrBlockContract contract, TokenSet followers)
    //^ requires this.currentToken == Token.Writes;
    //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      List<Expression> writes = new List<Expression>();
      this.ParseWrites(writes, followers);
      foreach (var write in writes)
        contract.AddWrites(write);
    }

    private TypeExpression/*?*/ TypeExpressionHasPointerType(TypeExpression typeExpr)
    {
      return TypeExpressionHasPointerType(typeExpr, new List<TypeExpression>());
    }


    private TypeExpression/*?*/ TypeExpressionHasPointerType(TypeExpression typeExpr, List<TypeExpression> visitedTypes)
    {
      PointerTypeExpression pte = typeExpr as PointerTypeExpression;
      if (pte != null) return pte.ElementType;

      ArrayTypeExpression ate = typeExpr as ArrayTypeExpression;
      if (ate != null) return ate.ElementType;

      if (typeExpr is VccTemplateTypeParameterExpression)
        return null;

      if (visitedTypes.Contains(typeExpr)) // cycle in type definitions (or "typedef struct S { ... } S;")
        return null;

      NamedTypeExpression namedTypeExpr = typeExpr as NamedTypeExpression;
      if (namedTypeExpr != null) {
        QualifiedName qName = namedTypeExpr.Expression as QualifiedName;
        if (qName != null) {
          return null;
        }
        SimpleName simpleName = namedTypeExpr.Expression as SimpleName;
        if (simpleName != null) {
          TypeExpression referencedType;
          if (this.typedefExpressions.TryGetValue(simpleName.ToString(), out referencedType)) {
            visitedTypes.Add(typeExpr);
            return TypeExpressionHasPointerType(referencedType, visitedTypes);
          }
          else
            return null ;
        }
        return null;
      }
      GenericTypeInstanceExpression genericTypeInstance = typeExpr as GenericTypeInstanceExpression;
      if (genericTypeInstance != null)
        return TypeExpressionHasPointerType(genericTypeInstance.GenericType);
      NonNullTypeExpression nonNullType = typeExpr as NonNullTypeExpression;
      if (nonNullType != null)
        return TypeExpressionHasPointerType(nonNullType);
      return null;
    }

    private Parameter ParseVarArgsParameter(TokenSet followers) {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      IName name = this.GetNameFor("__arglist");
      Declarator dec = new IdentifierDeclarator(new NameDeclaration(name, slb));
      slb.UpdateToSpan(dec.SourceLocation);
      Parameter result = new Parameter(new List<Specifier>(0), dec, slb, true);
      this.SkipTo(followers);
      return result;
    }

    private Parameter ParseParameter(TokenSet followers) {
      if (this.currentToken == Token.Range) 
        return ParseVarArgsParameter(followers);

      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      List<Specifier> specifiers = this.ParseSpecifiers(null, null, followers | Parser.DeclaratorStart, this.currentToken == Token.Specification);
      if (specifiers.Count > 0) slb.UpdateToSpan(specifiers[specifiers.Count-1].SourceLocation);
      Declarator declarator = this.ParseDeclarator(followers);
      declarator = this.UseDeclaratorAsTypeDefNameIfThisSeemsIntended(specifiers, declarator, followers);
      slb.UpdateToSpan(declarator.SourceLocation);
      var result = new Parameter(specifiers, declarator, slb);
      this.SkipTo(followers);
      return result;
    }

    private Declarator UseDeclaratorAsTypeDefNameIfThisSeemsIntended(List<Specifier> specifiers, Declarator declarator, TokenSet followers) {
      if (this.currentToken == Token.Identifier && declarator is IdentifierDeclarator && (specifiers.Count == 0 || this.TryToGetTypeExpressionFor(specifiers) == null)) {
        specifiers.Add(new TypedefNameSpecifier(new SimpleName(declarator.Identifier.Name, declarator.Identifier.SourceLocation, false)));
        declarator = this.ParseDeclarator(followers);
      }
      return declarator;
    }

    private List<Specifier> ParseSpecifiers(List<INamespaceDeclarationMember>/*?*/ namespaceMembers, List<ITypeDeclarationMember>/*?*/ typeMembers, TokenSet followers) {
      return this.ParseSpecifiers(namespaceMembers, typeMembers, followers, false);
    }

    private List<Specifier> ParseSpecifiers(List<INamespaceDeclarationMember>/*?*/ namespaceMembers, List<ITypeDeclarationMember>/*?*/ typeMembers, TokenSet followers, bool outIsAKeyword) {
      List<Specifier> result = new List<Specifier>();
      bool typeDefNameIsAllowed = true;
      bool typeDefNameMustReferencePrimitive = false;
      TokenSet followersOrSpecifierStart = followers|Parser.SpecifierStart;
      for (; ; ) {
        switch (this.currentToken) {
          case Token.Auto:
          case Token.Register:
          case Token.Static:
          case Token.Extern:
          case Token.Specification:
          case Token.Typedef:
            result.Add(new StorageClassSpecifier(this.currentToken, this.scanner.SourceLocationOfLastScannedToken));
            this.GetNextToken();
            break;
          case Token.Declspec:
            result.Add(this.ParseDeclspec(followersOrSpecifierStart));
            break;
          case Token.Void:
          case Token.Char:
          case Token.Int:
          case Token.Int8:
          case Token.Int16:
          case Token.Int32:
          case Token.Int64:
          case Token.Float:
          case Token.Bool:
          //case Token.Complex:
          case Token.W64:
          case Token.Axiom:
            typeDefNameIsAllowed = false;
            goto case Token.Double
              ;
          case Token.Double:
          case Token.Long:
          case Token.Short:
          case Token.Signed:
          case Token.Unsigned:
            typeDefNameMustReferencePrimitive = true;
            result.Add(new PrimitiveTypeSpecifier(this.currentToken, this.scanner.SourceLocationOfLastScannedToken));
            this.GetNextToken();
            break;
          case Token.Struct:
            typeDefNameIsAllowed = false;
            LookAndWarnForMisplacedDeclspec(result);
            result.Add(new StructSpecifier(this.ParseStructuredDeclarationOrDefinition(namespaceMembers, typeMembers, followersOrSpecifierStart, true)));
            goto default;
          case Token.Union:
            typeDefNameIsAllowed = false;
            LookAndWarnForMisplacedDeclspec(result);
            result.Add(new UnionSpecifier(this.ParseStructuredDeclarationOrDefinition(namespaceMembers, typeMembers, followersOrSpecifierStart, false)));
            goto default;
          case Token.Enum:
            typeDefNameIsAllowed = false;
            LookAndWarnForMisplacedDeclspec(result);
            result.Add(new EnumSpecifier(this.ParseEnumDeclarationOrDefinition(namespaceMembers, followersOrSpecifierStart)));
            goto default;
          case Token.Based:
          case Token.Const:
          case Token.Restrict:
          case Token.Volatile:
          case Token.Unaligned:
            result.Add(new TypeQualifier(this.currentToken, this.scanner.SourceLocationOfLastScannedToken));
            this.GetNextToken();
            break;
          //case Token.Asm:
          case Token.Cdecl:
          case Token.Fastcall:
          case Token.Inline:
          case Token.Stdcall:
            result.Add(new FunctionSpecifier(this.currentToken, this.scanner.SourceLocationOfLastScannedToken));
            this.GetNextToken();
            break;
          case Token.Identifier:
            if (outIsAKeyword && this.scanner.GetIdentifierString() == "out") {
              result.Add(new OutSpecifier(this.scanner.SourceLocationOfLastScannedToken));
              this.GetNextToken();
              break;
            }
            if (!typeDefNameIsAllowed) goto default;
            TypeExpression/*?*/ referencedType;
            if (!this.typedefExpressions.TryGetValue(this.scanner.GetIdentifierString(), out referencedType)) goto default;
            if (typeDefNameMustReferencePrimitive) {
              NamedTypeExpression/*?*/ nte = referencedType as NamedTypeExpression;
              if (nte == null) goto default;
              if (!(nte.Expression is AliasQualifiedName)) goto default;
            }
            typeDefNameIsAllowed = false;
            result.Add(ScopedTypeNameSpecifier.CreateForExpression(this.ParseSimpleOrScopedName(followers|Parser.SpecifierThatCombinesWithTypedefName)));
            break;
          default:
            this.SkipTo(followers);
            return result;
        }
      }
    }

    private void LookAndWarnForMisplacedDeclspec(IList<Specifier> specifiers) {
      foreach (var specifier in specifiers) {
        DeclspecSpecifier declspec = specifier as DeclspecSpecifier;
        if (declspec != null) {
          var modEnum = declspec.Modifiers.GetEnumerator();
          if (modEnum.MoveNext()) {
            if (modEnum.Current.ToString() == "Microsoft.Contracts.StringVccAttr" && modEnum.MoveNext()) {
              VccByteStringLiteral str = modEnum.Current as VccByteStringLiteral;
              if (str != null && (str.Value.ToString() == "dynamic_owns" || str.Value.ToString() == "volatile_owns"))
                this.HandleError(declspec.SourceLocation, Error.VccAttributeOnTypeDef);
            }
          }
        }
      }
    }

    private DeclspecSpecifier ParseDeclspec(TokenSet followers)
      //^ requires this.currentToken == Token.Declspec;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder sctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      TokenSet followersOrCommaOrRightParenthesis = followers|Token.Comma|Token.RightParenthesis;
      this.GetNextToken();
      List<Expression> modifiers = new List<Expression>();
      this.Skip(Token.LeftParenthesis);
      if (this.currentToken != Token.RightParenthesis) {
        Expression modifier = this.ParseExpression(followersOrCommaOrRightParenthesis);
        modifiers.Add(modifier);
        while (this.currentToken == Token.Comma) {
          this.GetNextToken();
          modifier = this.ParseExpression(followersOrCommaOrRightParenthesis);
          modifiers.Add(modifier);
        }
      }
      sctx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      DeclspecSpecifier result = new DeclspecSpecifier(modifiers, sctx);
      this.SkipOverTo(Token.RightParenthesis, followers);
      return result;
    }

    /// <summary>
    /// In C, there could be structured type declaration inside a method, a scope or another structured declaration,
    /// while in CCI, these have to live in the top level namespace. To avoid name clash, we have to mangle
    /// the names. A mangled name is the original name + a hash that represents the scope 
    /// 
    /// </summary>
    /// <param name="unmangeledName">the unmangled name from the source</param>
    /// <returns>The mangeled name if we are inside a scope, or unmangledName if not.</returns>
    private NameDeclaration MangledStructuredName(NameDeclaration unmangeledName) {
      string seg1 = unmangeledName.Name.Value;
      string seg2 = this.currentLexicalScope == null ? "" : this.currentLexicalScope.FullMangledName;
      string newname;
      if (String.IsNullOrEmpty(seg2)) newname = seg1;
      else newname = seg1 + "^" + seg2;
      return new NameDeclaration(this.nameTable.GetNameFor(newname), unmangeledName.SourceLocation);
    }

    private TypeExpression ParseStructuredDeclarationOrDefinition(List<INamespaceDeclarationMember>/*?*/ namespaceMembers, List<ITypeDeclarationMember> typeMembers, TokenSet followers, bool isStruct) 
      //^ requires this.currentToken == Token.Struct;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      List<FieldDeclaration>/*?*/ savedSpecificationFields = this.currentSpecificationFields;
      List<FunctionDeclaration>/*?*/ savedSpecificationMethods = this.currentSpecificationFunctions;
      List<TypeInvariant>/*?*/ savedTypeInvariants = this.currentTypeInvariants;
      List<DeclspecSpecifier> extendedAttributes = new List<DeclspecSpecifier>();
      this.currentSpecificationFields = null;
      this.currentSpecificationFunctions = null;
      this.currentTypeInvariants = null;
      SourceLocationBuilder sctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      while (this.currentToken == Token.Declspec) {
        extendedAttributes.Add(this.ParseDeclspec(followers | Token.LeftBrace));
      }
      bool noName = this.currentToken != Token.Identifier;
      NameDeclaration name = this.ParseNameDeclaration(false);
      NameDeclaration mangledName = this.MangledStructuredName(name);
      NamedTypeExpression/*?*/ texpr = null;
      List<ITypeDeclarationMember> newTypeMembers = new List<ITypeDeclarationMember>();
      if (this.currentToken == Token.LeftBrace) {
        object type;
        if (this.currentTypeName != null) {
          SimpleName nestedName = new VccSimpleName(name, name.SourceLocation);
          texpr = new VccNamedTypeExpression(new QualifiedName(this.currentTypeName, nestedName, name.SourceLocation));
          ITypeDeclarationMember nestedType;
          if (isStruct)
            nestedType = new VccNestedStructDeclaration(name, newTypeMembers, extendedAttributes, sctx);
          else
            nestedType = new VccNestedUnionDeclaration(name, newTypeMembers, extendedAttributes, sctx);
          if (typeMembers != null)
            typeMembers.Add(nestedType);
          type = nestedType;
          if (namespaceMembers != null)
            namespaceMembers.Insert(0, new AliasDeclaration(mangledName, texpr.Expression, name.SourceLocation));
        } else {
          texpr = new VccNamedTypeExpression(new VccSimpleName(mangledName, name.SourceLocation));
          INamespaceDeclarationMember namespaceType;
          if (isStruct)
            namespaceType = new VccStructDeclaration(mangledName, newTypeMembers, extendedAttributes, sctx);
          else
            namespaceType = new VccUnionDeclaration(mangledName, newTypeMembers, extendedAttributes, sctx);
          if (namespaceMembers != null)
            namespaceMembers.Add(namespaceType);
          type = namespaceType;
        }
        //TODO: else give error
        this.ParseRestOfTypeDeclaration(sctx, namespaceMembers, texpr.Expression, newTypeMembers, followers);
        // filter out unexpected constructs that cannot have they CompilationPart setup properly, they may
        // have been generated as artifacts of recovery from parse errors
        // see Microsoft.Cci.Ast.TypeDeclaration.SetMemberContainingTypeDeclaration for the supported classes
        newTypeMembers.RemoveAll(delegate(ITypeDeclarationMember member) { return !(member is TypeDeclarationMember || member is NestedTypeDeclaration); });
        if (this.currentSpecificationFields != null || this.currentTypeInvariants != null) {
          VccTypeContract tc = new VccTypeContract(this.currentSpecificationFields, null, this.currentTypeInvariants);
          this.compilation.ContractProvider.AssociateTypeWithContract(type, tc);
        }
      } else if (noName) {
        texpr = new VccNamedTypeExpression(new VccSimpleName(name, name.SourceLocation));
        this.HandleError(name.SourceLocation, Error.ExpectedIdentifier);
        this.SkipTo(followers);
      }
      if (texpr == null) {
        VccSimpleName simpleName = new VccSimpleName(mangledName, name.SourceLocation);
        if (this.currentToken == Token.ScopeResolution)
          texpr = new VccScopedTypeExpression((VccScopedName)this.ParseScopedName(simpleName, followers | Token.ScopeResolution));
        else 
          texpr = new VccNamedTypeExpression(simpleName);
      }

      if (newTypeMembers.Count == 0)
        this.emptyStructuredTypes[texpr] = true;

      this.currentSpecificationFields = savedSpecificationFields;
      this.currentSpecificationFunctions = savedSpecificationMethods;
      this.currentTypeInvariants = savedTypeInvariants;
      return texpr;
    }

    private void ParseRestOfTypeDeclaration(SourceLocationBuilder sctx, List<INamespaceDeclarationMember>/*?*/ namespaceMembers, Expression typeName, List<ITypeDeclarationMember> typeMembers, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      Expression/*?*/ savedCurrentTypeName = this.currentTypeName;
      this.currentTypeName = typeName;
      List<ITypeDeclarationMember>/*?*/ savedCurrentTypeMembers = this.currentTypeMembers;
      this.currentTypeMembers = typeMembers;
      this.Skip(Token.LeftBrace);
      while (Parser.DeclarationStart[this.currentToken] || this.currentToken == Token.Colon || this.currentToken == Token.Invariant) {
        if (this.currentToken == Token.Invariant) {
          if (this.currentTypeInvariants == null)
            this.currentTypeInvariants = new List<TypeInvariant>();
          this.ParseTypeInvariant(this.currentTypeInvariants, followers | Token.RightBrace | Token.Invariant);
        } else {
          this.ParseNonLocalDeclaration(namespaceMembers, typeMembers, followers | Token.RightBrace | Token.Invariant, false);
        }
      }
      ISourceLocation tokLoc = this.scanner.SourceLocationOfLastScannedToken;
      //^ assume tokLoc.SourceDocument == sctx.SourceDocument;
      sctx.UpdateToSpan(tokLoc);
      typeMembers.TrimExcess();
      this.SkipOverTo(Token.RightBrace, followers);
      this.currentTypeName = savedCurrentTypeName;
      this.currentTypeMembers = savedCurrentTypeMembers;
    }

    private List<TypeQualifier>/*?*/ ParseTypeQualifiers() {
      List<TypeQualifier>/*?*/ result = null;
      for (; ; ) {
        switch (this.currentToken) {
          case Token.Const:
          case Token.Restrict:
          case Token.Volatile:
          case Token.Cdecl:
          case Token.Unaligned:
            if (result == null) result = new List<TypeQualifier>(1);
            result.Add(new TypeQualifier(this.currentToken, this.scanner.SourceLocationOfLastScannedToken));
            this.GetNextToken();
            break;
          default:
            return result;
        }
      }
    }

    private TypeExpression ParseEnumDeclarationOrDefinition(List<INamespaceDeclarationMember>/*?*/ namespaceMembers, TokenSet followers)
      //^ requires this.currentToken == Token.Enum;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder sctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      NameDeclaration name = this.ParseNameDeclaration(false);
      NameDeclaration newname = this.MangledStructuredName(name);
      VccNamedTypeExpression texpr = new VccNamedTypeExpression(new VccSimpleName(newname, name.SourceLocation));
      if (this.currentToken == Token.LeftBrace) {
        List<ITypeDeclarationMember> members = new List<ITypeDeclarationMember>();
        NamespaceEnumDeclaration enumDeclaration = new VccEnumDeclaration(newname, this.GetTypeExpressionFor(TypeCode.UInt32, name.SourceLocation), members, sctx);
        if (namespaceMembers != null)
          namespaceMembers.Add(enumDeclaration);
        //TODO: else give error
        this.Skip(Token.LeftBrace);
        while (this.currentToken == Token.Identifier) {
          FieldDeclaration enumField = this.ParseEnumMember(texpr, members, followers|Token.Comma|Token.RightBrace);
          //TODO: deal with enums declared inside a method body.
          if (this.currentTypeMembers != null) this.currentTypeMembers.Add(enumField); //promote the enumeration member to a constant in the current namespace.
          if (this.currentToken == Token.RightBrace) break;
          this.Skip(Token.Comma);
          if (this.currentToken == Token.RightBrace) break;
        }
        sctx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        this.Skip(Token.RightBrace);
      }
      this.SkipTo(followers);
      return texpr;
    }

    private FieldDeclaration ParseEnumMember(VccNamedTypeExpression typeExpression, List<ITypeDeclarationMember>/*?*/ members, TokenSet followers)
      //^ requires this.currentToken == Token.Identifier;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder sctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      List<SourceCustomAttribute>/*?*/ attributes = null;
      NameDeclaration name = this.ParseNameDeclaration(true);
      Expression/*?*/ initializer = null;
      if (this.currentToken == Token.Assign) {
        this.GetNextToken();
        initializer = this.ParseExpression(followers);
      }
      EnumMember member = new EnumMember(attributes, typeExpression, name, initializer, sctx);
      if (members != null) members.Add(member);
      QualifiedName globalInitializer = new QualifiedName(typeExpression.Expression, new VccSimpleName(name, name.SourceLocation), name.SourceLocation);
      FieldDeclaration.Flags flags = FieldDeclaration.Flags.Constant|FieldDeclaration.Flags.Static|FieldDeclaration.Flags.Unsafe; //TODO: why unsafe?
      FieldDeclaration result = new FieldDeclaration(null, flags, TypeMemberVisibility.Assembly, typeExpression, name, globalInitializer, sctx);
      this.SkipTo(followers);
      return result;
    }

    private TypeExpression GetTypeExpressionFor(TypeCode typeCode, ISourceLocation sourceLocation)
      //^ requires typeCode == TypeCode.Boolean || typeCode == TypeCode.Byte || typeCode == TypeCode.Double || typeCode == TypeCode.Int16 ||
      //^   typeCode == TypeCode.Int32 || typeCode == TypeCode.Int64 || typeCode == TypeCode.SByte || typeCode == TypeCode.Single ||
      //^   typeCode == TypeCode.UInt16 || typeCode == TypeCode.UInt32 || typeCode == TypeCode.UInt64 || typeCode == TypeCode.Empty; 
    {
      return new NamedTypeExpression(this.RootQualifiedNameFor(typeCode, sourceLocation));
    }


    private BlockStatement ParseBody(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder bodyCtx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      List<Statement> statements = new List<Statement>();
      BlockStatement block = new BlockStatement(statements, 
        this.compilation.Options.CheckedArithmetic ? BlockStatement.Options.UseCheckedArithmetic : BlockStatement.Options.UseUncheckedArithmetic, bodyCtx);
      this.ParseBody(statements, bodyCtx, followers);
      //TODO: throw the body away and replace it with a stub that will reparse when needed.
      return block;
    }

    private void ParseBody(List<Statement> statements, SourceLocationBuilder bodyCtx, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      bodyCtx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      if (this.currentToken == Token.LeftBrace){
        this.GetNextToken();
        this.ParseStatements(statements, followers|Token.RightBrace);
        statements.Add(new EmptyStatement(true, this.scanner.SourceLocationOfLastScannedToken));
        bodyCtx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        this.Skip(Token.RightBrace);
      }
      this.SkipTo(followers);
    }

    private BlockStatement ParseBlock(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      return this.ParseBlock(BlockStatement.Options.Default, slb, followers);
    }

    private BlockStatement ParseBlock(BlockStatement.Options options, SourceLocationBuilder slb, TokenSet followers)
      //^ requires options == BlockStatement.Options.Default || options == BlockStatement.Options.AllowUnsafeCode || 
      //^  options == BlockStatement.Options.UseCheckedArithmetic || options == BlockStatement.Options.UseUncheckedArithmetic;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      FunctionOrBlockContract contract = new FunctionOrBlockContract();
      if (this.currentToken == Token.Block) {
        this.Skip(Token.Block);
        this.ParseFunctionOrBlockContract(contract, followers);
      }
      this.Skip(Token.LeftBrace);
      List<Statement> statements = new List<Statement>();
      this.ParseStatements(statements, followers|Token.RightBrace);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      BlockStatement result = contract.HasContract ? new VccBlockWithContracts(statements, options, slb) : new BlockStatement(statements, options, slb);
      if (contract.HasContract) {
        this.compilation.ContractProvider.AssociateMethodWithContract(result, contract.ToMethodContract());
      }
      this.SkipOverTo(Token.RightBrace, followers);
      return result;
    }

    private void ParseStatements(List<Statement> statements, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      TokenSet statementFollowers = followers|Parser.StatementStart;
      if (!statementFollowers[this.currentToken])
        this.SkipTo(statementFollowers, Error.InvalidExprTerm, this.scanner.GetTokenSource());
      while (Parser.StatementStart[this.currentToken]) {
        Statement s = this.ParseStatement(statementFollowers);
        StatementGroup.AddStatementOrGroupToList(s, statements);
      }
      this.SkipTo(followers);
    }

    private Statement ParseStatement(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      Statement result;

      switch (this.currentToken) {
        case Token.LeftBrace:
        case Token.Block:
          //this.lexicalScopeSuffix = this.PushScopeString(this.lexicalScopeSuffix, this.GetNewScopeId());
          SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
          this.currentLexicalScope = new LexicalScope(this.currentLexicalScope, slb);
          result = this.ParseBlock(followers);
          this.currentLexicalScope = this.currentLexicalScope.ParentScope;
          return result;
        case Token.Semicolon: return this.ParseEmptyStatement(followers);
        case Token.If: return this.ParseIf(followers);
        case Token.Switch: return this.ParseSwitch(followers);
        case Token.While: return this.ParseWhile(followers);
        case Token.Do: return this.ParseDoWhile(followers);
        case Token.For: return this.ParseFor(followers);
        case Token.Assert: return this.ParseAssert(followers);
        case Token.Assume: return this.ParseAssume(followers);
        case Token.Break: return this.ParseBreak(followers);
        case Token.Continue: return this.ParseContinue(followers);
        case Token.Goto: return this.ParseGoto(followers);
        case Token.Return: return this.ParseReturn(followers);
        default:
          return this.ParseExpressionStatementOrDeclaration(false, true, followers);
      }
    }

    private Statement ParseExpressionStatementOrDeclaration(bool acceptComma, bool acceptLabel, TokenSet followers)
      //^ requires acceptComma ==> followers[Token.Comma];
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      //^ ensures result is ExpressionStatement || result is LocalDeclarationsStatement || (acceptLabel && result is LabeledStatement);
    {
      if (this.CurrentTokenStartsDeclaration()) {
        List<Statement> statements = this.ParseLocalDeclaration(followers);
        if (statements.Count == 1)
          return statements[0];
        else
          return new StatementGroup(statements);
      }
      TokenSet followersOrCommaOrColonOrSemicolon = followers|Token.Comma|Token.Colon|Token.Semicolon;
      Expression e = this.ParseExpression(!acceptComma, followersOrCommaOrColonOrSemicolon);
      SourceLocationBuilder slb = new SourceLocationBuilder(e.SourceLocation);
      ExpressionStatement eStat = new ExpressionStatement(e, slb);
      VccSimpleName/*?*/ id = null;
      if (this.currentToken == Token.Colon && acceptLabel && (id = e as VccSimpleName) != null)
        return this.ParseLabeledStatement(id, followers);
      if (!acceptComma || this.currentToken != Token.Comma) {
        if (this.currentToken == Token.Semicolon) {
          slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
          //^ assume this.currentToken == Token.Semicolon;
          this.GetNextToken();
          this.SkipTo(followers);
        } else
          this.SkipSemiColon(followers);
      }
      //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      return eStat;
    }

    private LabeledStatement ParseLabeledStatement(VccSimpleName label, TokenSet followers) 
      //^ requires this.currentToken == Token.Colon;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(label.SourceLocation);
      this.GetNextToken();
      LoopContract/*?*/ contract = this.ParseLoopContract(followers);
      Statement statement;
      if (Parser.StatementStart[this.currentToken]) {
        statement = this.ParseStatement(followers);
      } else {
        statement = new EmptyStatement(false, this.scanner.SourceLocationOfLastScannedToken);
        this.SkipTo(followers, Error.ExpectedSemicolon);
      }
      //^ assert followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      slb.UpdateToSpan(statement.SourceLocation);
      LabeledStatement result = new LabeledStatement(new NameDeclaration(label.Name, label.SourceLocation), statement, slb);
      if (contract != null)
        this.compilation.ContractProvider.AssociateLoopWithContract(result, contract);
      //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      return result;
    }

    private Statement ParseReturn(TokenSet followers)       
      //^ requires this.currentToken == Token.Return;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      Expression/*?*/ expr = null;
      if (this.currentToken != Token.Semicolon) {
        expr = this.ParseExpression(true, followers|Token.Semicolon);
        slb.UpdateToSpan(expr.SourceLocation);
      }
      Statement result = new ReturnStatement(expr, slb);
      this.SkipSemiColon(followers);
      return result;
    }

    private Statement ParseAssert(TokenSet followers)
      //^ requires this.currentToken == Token.Assert;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      Expression/*?*/ expr = this.ParseExpression(true, followers|Token.RightParenthesis|Token.Semicolon);
      slb.UpdateToSpan(expr.SourceLocation);
      Statement result = new AssertStatement(expr, slb);
      this.SkipOverTo(Token.RightParenthesis, followers|Token.Semicolon);
      this.SkipSemiColon(followers);
      return result;
    }

    private Statement ParseAssume(TokenSet followers)
      //^ requires this.currentToken == Token.Assume;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      Expression/*?*/ expr = this.ParseExpression(true, followers|Token.RightParenthesis|Token.Semicolon);
      slb.UpdateToSpan(expr.SourceLocation);
      Statement result = new AssumeStatement(expr, slb);
      this.SkipOverTo(Token.RightParenthesis, followers|Token.Semicolon);
      this.SkipSemiColon(followers);
      return result;
    }

    private Statement ParseGoto(TokenSet followers)
      //^ requires this.currentToken == Token.Goto;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      Statement result;
      switch (this.currentToken) {
        case Token.Case:
          this.GetNextToken();
          Expression caseLabel = this.ParseExpression(followers|Token.Semicolon);
          slb.UpdateToSpan(caseLabel.SourceLocation);
          result = new GotoSwitchCaseStatement(caseLabel, slb);
          break;
        case Token.Default:
          slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
          result = new GotoSwitchCaseStatement(null, slb);
          this.GetNextToken();
          break;
        default:
          result = new GotoStatement(this.ParseSimpleName(followers), slb);
          break;
      }
      this.SkipSemiColon(followers);
      return result;
    }

    private Statement ParseContinue(TokenSet followers)
      //^ requires this.currentToken == Token.Continue;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      ISourceLocation sourceLocation = this.scanner.SourceLocationOfLastScannedToken;
      this.GetNextToken();
      Statement result = new ContinueStatement(sourceLocation);
      this.SkipSemiColon(followers);
      return result;
    }

    private Statement ParseBreak(TokenSet followers)       
      //^ requires this.currentToken == Token.Break;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      ISourceLocation sourceLocation = this.scanner.SourceLocationOfLastScannedToken;
      this.GetNextToken();
      Statement result = new BreakStatement(sourceLocation);
      this.SkipSemiColon(followers);
      return result;
    }

    private Statement ParseFor(TokenSet followers)
      //^ requires this.currentToken == Token.For;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      TokenSet followersOrRightParenthesisOrSemicolon = followers|Parser.RightParenthesisOrSemicolon|Token.Invariant|Token.Writes;
      List<Statement> initStatements = this.ParseForInitializer(followersOrRightParenthesisOrSemicolon);
      Expression condition = this.ParseForCondition(followersOrRightParenthesisOrSemicolon);
      List<Statement> incrementStatements = this.ParseForIncrementer(followers|Token.RightParenthesis|Token.Invariant|Token.Writes);
      this.Skip(Token.RightParenthesis);
      LoopContract/*?*/ contract = this.ParseLoopContract(followers);
      Statement body = this.ParseStatement(followers);
      slb.UpdateToSpan(body.SourceLocation);
      this.WarnIfLoopWithContractAndEmptyBody(contract, body);
      ForStatement result = new ForStatement(initStatements, condition, incrementStatements, body, slb);
      if (contract != null)
        this.compilation.ContractProvider.AssociateLoopWithContract(result, contract);
      //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      return result;
    }

    private List<Statement> ParseForInitializer(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.Semicolon || this.currentToken == Token.RightParenthesis || this.currentToken == Token.EndOfFile;
    {
      List<Statement> statements = new List<Statement>(1);
      if (this.currentToken == Token.Semicolon) {
        this.GetNextToken();
        statements.TrimExcess();
        //^ assume this.currentToken == Token.Semicolon;
        return statements;
      }
      if (this.currentToken == Token.RightParenthesis) {
        this.Skip(Token.Semicolon);
        statements.TrimExcess();
        //^ assume this.currentToken == Token.RightParenthesis;
        return statements;
      }
      TokenSet followerOrComma = followers|Token.Comma;
      for (; ; ) {
        //^ assume followerOrComma[Token.Comma];
        Statement s = this.ParseExpressionStatementOrDeclaration(true, false, followerOrComma);
        statements.Add(s);
        if (s is LocalDeclarationsStatement) {
          if (statements.Count > 1)
            this.HandleError(s.SourceLocation, Error.ExpectedExpression);
        } else {
          ExpressionStatement es = (ExpressionStatement)s;
          Expression e = es.Expression;
          if (!(e is Assignment || e is BinaryOperationAssignment || e is MethodCall || e is UnaryOperationAssignment || e is CreateObjectInstance))
            this.HandleError(e.SourceLocation, Error.IllegalStatement);
        }
        //^ assume followers[this.currentToken] || this.currentToken == Token.Comma || this.currentToken == Token.EndOfFile;
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      //^ assert followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      statements.TrimExcess();
      //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      return statements;
    }

    private List<Statement> ParseForIncrementer(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.RightParenthesis || this.currentToken == Token.EndOfFile;
    {
      List<Statement> statements = new List<Statement>(1);
      if (this.currentToken == Token.RightParenthesis) {
        statements.TrimExcess();
        //^ assume this.currentToken == Token.RightParenthesis;
        return statements;
      }
      TokenSet followerOrComma = followers|Token.Comma;
      for (; ; ) {
        Expression e = this.ParseExpression(followerOrComma);
        if (!(e is Assignment || e is BinaryOperationAssignment || e is MethodCall || e is UnaryOperationAssignment || e is CreateObjectInstance))
          this.HandleError(e.SourceLocation, Error.IllegalStatement);
        statements.Add(new ExpressionStatement(e));
        //^ assume followers[this.currentToken] || this.currentToken == Token.Comma || this.currentToken == Token.EndOfFile;
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      //^ assert followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      statements.TrimExcess();
      //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      return statements;
    }

    private Expression ParseForCondition(TokenSet follower)
    {
      Expression result;
      if (this.currentToken != Token.Semicolon)
        result = this.ParseExpression(follower);
      else {
        SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
        result = new CompileTimeConstant(true, slb.GetSourceLocation());
      }
      this.Skip(Token.Semicolon);
      return result;
    }

    private Statement ParseDoWhile(TokenSet followers)
      //^ requires this.currentToken == Token.Do;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      LoopContract/*?*/ contract = this.ParseLoopContract(followers);
      Statement body = this.ParseStatement(followers|Token.While);
      if (body is EmptyStatement)
        this.HandleError(body.SourceLocation, Error.PossibleMistakenNullStatement);
      this.Skip(Token.While);
      Expression condition = this.ParseParenthesizedExpression(false, followers|Token.Semicolon);
      DoWhileStatement result = new DoWhileStatement(body, condition, slb);
      if (contract != null)
        this.compilation.ContractProvider.AssociateLoopWithContract(result, contract);

      this.SkipSemiColon(followers);
      return result;
    }

    private Statement ParseWhile(TokenSet followers) 
      //^ requires this.currentToken == Token.While;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      Expression condition = this.ParseParenthesizedExpression(false, followers|Token.Invariant|Token.Semicolon|Token.Writes);
      LoopContract/*?*/ contract = this.ParseLoopContract(followers);
      Statement body = this.ParseStatement(followers);
      slb.UpdateToSpan(body.SourceLocation);
      WarnIfLoopWithContractAndEmptyBody(contract, body);
      WhileDoStatement result = new WhileDoStatement(condition, body, slb);
      if (contract != null)
        this.compilation.ContractProvider.AssociateLoopWithContract(result, contract);
      this.SkipTo(followers);
      return result;
    }

    private Statement ParseSwitch(TokenSet followers)
      //^ requires this.currentToken == Token.Switch;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      Expression expression = this.ParseParenthesizedExpression(false, followers|Token.LeftBrace);
      List<SwitchCase> cases = new List<SwitchCase>();
      this.Skip(Token.LeftBrace);
      TokenSet followersOrCaseOrColonOrDefaultOrRightBrace = followers|Parser.CaseOrColonOrDefaultOrRightBrace;
      TokenSet followersOrCaseOrDefaultOrRightBrace = followers|Parser.CaseOrDefaultOrRightBrace;
      for (; ; ) {
        SourceLocationBuilder scCtx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
        Expression/*?*/ scExpression = null;
        switch (this.currentToken) {
          case Token.Case:
            this.GetNextToken();
            if (this.currentToken == Token.Colon)
              this.HandleError(Error.ConstantExpected);
            else {
              scExpression = this.ParseExpression(followersOrCaseOrColonOrDefaultOrRightBrace);
              scCtx.UpdateToSpan(scExpression.SourceLocation);
            }
            break;
          case Token.Default: //Parse these as many times as they occur. Checker will report the error.
            this.GetNextToken();
            break;
          default:
            if (Parser.StatementStart[this.currentToken]) {
              this.HandleError(Error.StmtNotInCase);
              this.ParseStatement(followersOrCaseOrColonOrDefaultOrRightBrace);
              continue;
            }
            goto done;
        }
        this.Skip(Token.Colon);
        IEnumerable<Statement> scBody;
        if (Parser.StatementStart[this.currentToken])
          scBody = this.ParseSwitchCaseStatementBlock(scCtx, followersOrCaseOrDefaultOrRightBrace);
        else
          scBody = IteratorHelper.GetEmptyEnumerable<Statement>();
        cases.Add(new SwitchCase(scExpression, scBody, scCtx));
      }
    done:
      if (cases.Count == 0) {
        this.HandleError(Error.EmptySwitch);
      } else {
        // add SwitchCaseBottom to last case if it happened to have no statements.
        SwitchCase lastCase = cases[cases.Count-1];
        if (lastCase != null && !lastCase.Body.GetEnumerator().MoveNext()) {
          List<Statement> body = new List<Statement>(1);
          body.Add(new EmptyStatement(true, lastCase.SourceLocation));
          cases[cases.Count-1] = new SwitchCase(lastCase.IsDefault ? null : lastCase.Expression, body.AsReadOnly(), lastCase.SourceLocation);
        }
      }

      cases.TrimExcess();
      SwitchStatement result = new SwitchStatement(expression, cases, slb);
      this.SkipOverTo(Token.RightBrace, followers);
      return result;
    }

    private IEnumerable<Statement> ParseSwitchCaseStatementBlock(SourceLocationBuilder switchCaseContext, TokenSet followers)
      //^ requires Parser.StatementStart[this.currentToken];
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      List<Statement> statements = new List<Statement>();
      while (Parser.StatementStart[this.currentToken]) {
        if (this.currentToken == Token.Default) break;
        Statement s = this.ParseStatement(followers);
        StatementGroup.AddStatementOrGroupToList(s, statements);
      }
      if (statements.Count > 0) {
        ISourceLocation sctx = statements[statements.Count-1].SourceLocation;
        switchCaseContext.UpdateToSpan(sctx);
        statements.Add(new EmptyStatement(true, sctx));
      }
      statements.TrimExcess();
      IEnumerable<Statement> result = statements.AsReadOnly();
      //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      return result;
    }

    private Statement ParseIf(TokenSet followers)       
      //^ requires this.currentToken == Token.If;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      Expression ifCondition = this.ParseParenthesizedExpression(false, followers|Parser.StatementStart);
      Statement ifTrue = this.ParseStatement(followers|Token.Else);
      if (ifTrue is EmptyStatement)
        this.HandleError(ifTrue.SourceLocation, Error.PossibleMistakenNullStatement);
      Statement ifFalse;
      if (this.currentToken == Token.Else) {
        this.GetNextToken();
        ifFalse = this.ParseStatement(followers);
        if (ifFalse is EmptyStatement)
          this.HandleError(ifFalse.SourceLocation, Error.PossibleMistakenNullStatement);
      } else {
        ifFalse = new EmptyStatement(false, ifTrue.SourceLocation);
      }
      slb.UpdateToSpan(ifFalse.SourceLocation);
      Statement result = new ConditionalStatement(ifCondition, ifTrue, ifFalse, slb);
      this.SkipTo(followers);
      return result;
    }

    private Statement ParseEmptyStatement(TokenSet followers)
      //^ requires this.currentToken == Token.Semicolon;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      EmptyStatement result = new EmptyStatement(false, this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.SkipTo(followers);
      return result;
    }

    private Expression ParseExpression(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      return this.ParseExpression(false, followers);
    }

    private Expression ParseExpressionWithCheckedDefault(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      Expression e = this.ParseExpression(false, followers);
      if (this.compilation.Options.CheckedArithmetic)
        e = new CheckedExpression(e, e.SourceLocation);
      return e;
    }

    private Expression ParseExpression(bool allowCommaExpressions, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      TokenSet followersOrInfixOperators = followers|Parser.InfixOperators;
      Expression operand1 = this.ParseUnaryExpression(followersOrInfixOperators);
      for (; ; ) {
        if (!Parser.InfixOperators[this.currentToken] || (this.currentToken == Token.Comma && !allowCommaExpressions)) {
          this.SkipTo(followers);
          return operand1;
        }
        if (this.currentToken == Token.Conditional)
          operand1 = this.ParseConditional(operand1, followers|Token.Comma);
        else if (this.currentToken == Token.Comma) {
          this.GetNextToken();
          Expression operand2 = this.ParseUnaryExpression(followers|Parser.InfixOperators);
          if (Parser.InfixOperators[this.currentToken])
            operand2 = this.ParseAssignmentExpression(operand2, followers);
          operand1 = this.AllocateBinaryExpression(operand1, operand2, Token.Comma);
        } else {
          Expression assignmentExpr = this.ParseAssignmentExpression(operand1, followers|Token.Comma);
          if (assignmentExpr == operand1) return assignmentExpr; //no progress made, exit.
          operand1 = assignmentExpr;
        }
      }
    }

    private Expression ParseAssignmentExpression(Expression operand1, TokenSet followers) 
      //^ requires Parser.InfixOperators[this.currentToken];
      //^ requires this.currentToken != Token.Conditional && this.currentToken != Token.Comma;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      switch (this.currentToken) {
        case Token.AddAssign:
        case Token.Assign:
        case Token.BitwiseAndAssign:
        case Token.BitwiseOrAssign:
        case Token.BitwiseXorAssign:
        case Token.DivideAssign:
        case Token.LeftShiftAssign:
        case Token.MultiplyAssign:
        case Token.RemainderAssign:
        case Token.RightShiftAssign:
        case Token.SubtractAssign:
          SourceLocationBuilder slb = new SourceLocationBuilder(operand1.SourceLocation);
          Token operatorToken = this.currentToken;
          this.GetNextToken();
          TargetExpression target = new TargetExpression(operand1);
          Expression operand2 = this.ParseExpression(followers);
          slb.UpdateToSpan(operand2.SourceLocation);
          //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
          switch (operatorToken) {
            case Token.AddAssign: return new VccAdditionAssignment(target, operand2, slb);
            case Token.BitwiseAndAssign: return new BitwiseAndAssignment(target, operand2, slb);
            case Token.BitwiseOrAssign: return new BitwiseOrAssignment(target, operand2, slb);
            case Token.BitwiseXorAssign: return new ExclusiveOrAssignment(target, operand2, slb);
            case Token.DivideAssign: return new DivisionAssignment(target, operand2, slb);
            case Token.LeftShiftAssign: return new LeftShiftAssignment(target, this.ConvertToInt32(operand2), slb);
            case Token.MultiplyAssign: return new MultiplicationAssignment(target, operand2, slb);
            case Token.RemainderAssign: return new VccModulusAssignment(target, operand2, slb);
            case Token.RightShiftAssign: return new RightShiftAssignment(target, this.ConvertToInt32(operand2), slb);
            case Token.SubtractAssign: return new SubtractionAssignment(target, operand2, slb);
            default: return new VccAssignment(target, operand2, slb);
          }
        default:
          operand1 = this.ParseBinaryExpression(operand1, followers|Token.Conditional);
          if (this.currentToken == Token.Conditional)
            return this.ParseConditional(operand1, followers);
          //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
          return operand1;
      }
    }

    private Expression ParseBinaryExpression(Expression operand1, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      TokenSet unaryFollowers = followers|Parser.InfixOperators;
      Expression expression;
      switch (this.currentToken) {
        case Token.Plus:
        case Token.BitwiseAnd:
        case Token.BitwiseOr:
        case Token.BitwiseXor:
        case Token.Divide:
        case Token.Equal:
        case Token.Explies:
        case Token.GreaterThan:
        case Token.GreaterThanOrEqual:
        case Token.Implies:
        case Token.IfAndOnlyIf:
        case Token.LeftShift:
        case Token.LessThan:
        case Token.LessThanOrEqual:
        case Token.LogicalAnd:
        case Token.LogicalOr:
        case Token.Multiply:
        case Token.NotEqual:
        case Token.Remainder:
        case Token.RightShift:
        case Token.Subtract:
          Token operator1 = this.currentToken;
          this.GetNextToken();
          Expression operand2;
          operand2 = this.ParseUnaryExpression(unaryFollowers);
          switch (this.currentToken) {
            case Token.Plus:
            case Token.BitwiseAnd:
            case Token.BitwiseOr:
            case Token.BitwiseXor:
            case Token.Divide:
            case Token.Equal:
            case Token.Explies:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.Implies:
            case Token.IfAndOnlyIf:
            case Token.LeftShift:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.LogicalAnd:
            case Token.LogicalOr:
            case Token.Multiply:
            case Token.NotEqual:
            case Token.Remainder:
            case Token.RightShift:
            case Token.Subtract:
              expression = this.ParseComplexExpression(Token.None, operand1, operator1, operand2, unaryFollowers);
              break;
            default:
              expression = this.AllocateBinaryExpression(operand1, operand2, operator1);
              break;
          }
          break;
        default:
          expression = operand1;
          break;
      }
      this.SkipTo(followers);
      return expression;
    }

    private Expression AllocateBinaryExpression(Expression operand1, Expression operand2, Token operatorToken) {
      SourceLocationBuilder slb = new SourceLocationBuilder(operand1.SourceLocation);
      slb.UpdateToSpan(operand2.SourceLocation);
      switch (operatorToken) {
        case Token.Plus: return new VccAddition(operand1, operand2, slb);
        case Token.BitwiseAnd: return new VccBitwiseAnd(operand1, operand2, slb);
        case Token.BitwiseOr: return new VccBitwiseOr(operand1, operand2, slb);
        case Token.BitwiseXor: return new VccExclusiveOr(operand1, operand2, slb);
        case Token.Comma: return new Comma(operand1, operand2, slb);
        case Token.Explies: return new VccExplies(operand1, operand2, slb);
        case Token.IfAndOnlyIf: return new VccIfAndOnlyIf(operand1, operand2, slb);
        case Token.Equal: return new VccEquality(operand1, operand2, slb);
        case Token.GreaterThan: return new GreaterThan(operand1, operand2, slb);
        case Token.GreaterThanOrEqual: return new GreaterThanOrEqual(operand1, operand2, slb);
        case Token.Implies: return new Implies(operand1, operand2, slb);
        case Token.LeftShift: return new VccLeftShift(this.ProvideSignBiasIfNeeded(operand1, operand2), this.ConvertToInt32(operand2), slb);
        case Token.LessThan: return new LessThan(operand1, operand2, slb);
        case Token.LessThanOrEqual: return new LessThanOrEqual(operand1, operand2, slb);
        case Token.LogicalAnd: return new VccLogicalAnd(operand1, operand2, slb);
        case Token.LogicalOr: return new VccLogicalOr(operand1, operand2, slb);
        case Token.Multiply: return new VccMultiplication(operand1, operand2, slb);
        case Token.NotEqual: return new VccNotEquality(operand1, operand2, slb);
        case Token.Remainder: return new VccModulus(operand1, operand2, slb);
        case Token.RightShift: return new VccRightShift(this.ProvideSignBiasIfNeeded(operand1, operand2), this.ConvertToInt32(operand2), slb);
        case Token.Subtract: return new VccSubtraction(operand1, operand2, slb);
        case Token.Divide:
          VccInitializerWithDesignators initializer = operand2 as VccInitializerWithDesignators;
          if (initializer != null)
            return new VccInitializerWithDefault(operand1, initializer, slb);
          return new VccDivision(operand1, operand2, slb);
        default:
          //^ assume false;
          goto case Token.Plus;
      }
    }

    private Expression ProvideSignBiasIfNeeded(Expression operand1, Expression operand2) {
      CompileTimeConstant/*?*/ cc = operand1 as CompileTimeConstant;
      if (cc != null && cc.ValueIsPolymorphicCompileTimeConstant) {
        //If operand2 is unsigned, we want cc to prefer binding to unsigned types during overload resolution.
        //For example, 1 << 2u, whould result in (unsigned)4, not (signed)4.
        return new CompileTimeConstantWhoseSignDependsOnAnotherExpression(cc, operand2);
      }
      Parenthesis/*?*/ paren = operand1 as Parenthesis;
      if (paren != null)
        operand1 = new Parenthesis(this.ProvideSignBiasIfNeeded(paren.ParenthesizedExpression, operand2), paren.SourceLocation);
      return operand1;
    }

    private Expression ConvertToInt32(Expression expression) {
      return new UncheckedExpression(new Cast(expression, this.GetTypeExpressionFor(TypeCode.Int32, expression.SourceLocation), expression.SourceLocation), expression.SourceLocation);      
    }

    private Expression ParseComplexExpression(Token operator0, Expression operand1, Token operator1, Expression operand2, TokenSet followers)
      //^ requires this.currentToken != Token.EndOfFile;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
    restart:
      //^ assume this.currentToken != Token.EndOfFile; //OK because of precondition and state at point where control comes back here
      Token operator2 = this.currentToken;
      this.GetNextToken();
      Expression operand3 = this.ParseUnaryExpression(followers);
      if (Parser.LowerPriority(operator1, operator2)) {
        switch (this.currentToken) {
          case Token.Plus:
          case Token.BitwiseAnd:
          case Token.BitwiseOr:
          case Token.BitwiseXor:
          case Token.Divide:
          case Token.Equal:
          case Token.Explies:
          case Token.GreaterThan:
          case Token.GreaterThanOrEqual:
          case Token.Implies:
          case Token.IfAndOnlyIf:
          case Token.LeftShift:
          case Token.LessThan:
          case Token.LessThanOrEqual:
          case Token.LogicalAnd:
          case Token.LogicalOr:
          case Token.Multiply:
          case Token.NotEqual:
          case Token.Remainder:
          case Token.RightShift:
          case Token.Subtract:
            if (Parser.LowerPriority(operator2, this.currentToken)) {
              //Can't reduce just operand2 op2 operand3 because there is an op3 with priority over op2
              //^ assume this.currentToken != Token.EndOfFile; //follows from the switch logic
              operand2 = this.ParseComplexExpression(operator1, operand2, operator2, operand3, followers); //reduce complex expression
              //Now either at the end of the entire expression, or at an operator that is at the same or lower priority than op1
              //Either way, operand2 op2 operand3 op3 ... has been reduced to just operand2 and the code below will
              //either restart this procedure to parse the remaining expression or reduce operand1 op1 operand2 and return to the caller
            } else
              goto default;
            break;
          default:
            //Reduce operand2 op2 operand3. There either is no further binary operator, or it does not take priority over op2.
            operand2 = this.AllocateBinaryExpression(operand2, operand3, operator2);
            //The code following this will reduce operand1 op1 operand2 and return to the caller
            break;
        }
      } else {
        operand1 = this.AllocateBinaryExpression(operand1, operand2, operator1);
        operand2 = operand3;
        operator1 = operator2;
      }
      //At this point either operand1 op1 operand2 has been reduced, or operand2 op2 operand3 .... has been reduced, so back to just two operands
      switch (this.currentToken) {
        case Token.Plus:
        case Token.BitwiseAnd:
        case Token.BitwiseOr:
        case Token.BitwiseXor:
        case Token.Divide:
        case Token.Equal:
        case Token.Explies:
        case Token.GreaterThan:
        case Token.GreaterThanOrEqual:
        case Token.Implies:
        case Token.IfAndOnlyIf:
        case Token.LeftShift:
        case Token.LessThan:
        case Token.LessThanOrEqual:
        case Token.LogicalAnd:
        case Token.LogicalOr:
        case Token.Multiply:
        case Token.NotEqual:
        case Token.Remainder:
        case Token.RightShift:
        case Token.Subtract:
          if (operator0 == Token.None || Parser.LowerPriority(operator0, this.currentToken))
            //The caller is not prepared to deal with the current token, go back to the start of this routine and consume some more tokens
            goto restart;
          else
            goto default; //Let the caller deal with the current token
        default:
          //reduce operand1 op1 operand2 and return to caller
          return this.AllocateBinaryExpression(operand1, operand2, operator1);
      }
    }

    /// <summary>
    /// returns true if opnd1 operator1 opnd2 operator2 opnd3 implicitly brackets as opnd1 operator1 (opnd2 operator2 opnd3)
    /// </summary>
    private static bool LowerPriority(Token operator1, Token operator2) {
      switch (operator1) {
        case Token.Divide:
        case Token.Multiply:
        case Token.Remainder:
          switch (operator2) {
            default:
              return false;
          }
        case Token.Plus:
        case Token.Subtract:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
              return true;
            default:
              return false;
          }
        case Token.LeftShift:
        case Token.RightShift:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
              return true;
            default:
              return false;
          }
        case Token.GreaterThan:
        case Token.GreaterThanOrEqual:
        case Token.LessThan:
        case Token.LessThanOrEqual:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
              return true;
            default:
              return false;
          }
        case Token.Equal:
        case Token.NotEqual:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
              return true;
            default:
              return false;
          }
        case Token.BitwiseAnd:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.Equal:
            case Token.NotEqual:
              return true;
            default:
              return false;
          }
        case Token.BitwiseXor:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.Equal:
            case Token.NotEqual:
            case Token.BitwiseAnd:
              return true;
            default:
              return false;
          }
        case Token.BitwiseOr:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.Equal:
            case Token.NotEqual:
            case Token.BitwiseAnd:
            case Token.BitwiseXor:
              return true;
            default:
              return false;
          }
        case Token.LogicalAnd:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.Equal:
            case Token.NotEqual:
            case Token.BitwiseAnd:
            case Token.BitwiseXor:
            case Token.BitwiseOr:
              return true;
            default:
              return false;
          }
        case Token.LogicalOr:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.Equal:
            case Token.NotEqual:
            case Token.BitwiseAnd:
            case Token.BitwiseXor:
            case Token.BitwiseOr:
            case Token.LogicalAnd:
              return true;
            default:
              return false;
          }
        case Token.Implies:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.Equal:
            case Token.NotEqual:
            case Token.BitwiseAnd:
            case Token.BitwiseXor:
            case Token.BitwiseOr:
            case Token.LogicalAnd:
            case Token.LogicalOr:
            case Token.Explies:
            case Token.Implies: // ==> is right associative
              return true;
            default:
              return false;
          }

        case Token.Explies:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.Equal:
            case Token.NotEqual:
            case Token.BitwiseAnd:
            case Token.BitwiseXor:
            case Token.BitwiseOr:
            case Token.LogicalAnd:
            case Token.LogicalOr:
              return true;
            default:
              // Explies is left-associative
              return false;
          }

         case Token.IfAndOnlyIf:
          switch (operator2) {
            case Token.Divide:
            case Token.Multiply:
            case Token.Remainder:
            case Token.Plus:
            case Token.Subtract:
            case Token.LeftShift:
            case Token.RightShift:
            case Token.GreaterThan:
            case Token.GreaterThanOrEqual:
            case Token.LessThan:
            case Token.LessThanOrEqual:
            case Token.Equal:
            case Token.NotEqual:
            case Token.BitwiseAnd:
            case Token.BitwiseXor:
            case Token.BitwiseOr:
            case Token.LogicalAnd:
            case Token.LogicalOr:
            case Token.Implies:
            case Token.Explies:
              return true;
            default:
              return false;
          }
      }
      //^ assume false;
      return false;
    }

    private Expression ParseConditional(Expression condition, TokenSet followers) 
      //^ requires this.currentToken == Token.Conditional;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      this.GetNextToken();
      SourceLocationBuilder slb = new SourceLocationBuilder(condition.SourceLocation);
      Expression resultIfTrue = this.ParseExpression(true, followers|Token.Colon);
      Expression resultIfFalse;
      if (this.currentToken == Token.Colon) {
        this.GetNextToken();
        resultIfFalse = this.ParseExpression(false, followers);
      } else {
        this.Skip(Token.Colon); //gives appropriate error message
        if (!followers[this.currentToken])
          //Assume that only the : is missing. Go ahead as if it were specified.
          resultIfFalse = this.ParseExpression(false, followers);
        else
          resultIfFalse = this.ParseDummyExpression();
      }
      slb.UpdateToSpan(resultIfFalse.SourceLocation);
      Expression result = new VccConditional(condition, resultIfTrue, resultIfFalse, slb);
      this.SkipTo(followers);
      return result;
    }

    private Expression ParseDummyExpression() {
      ISourceLocation currentLocation = this.scanner.SourceLocationOfLastScannedToken;
      return new CompileTimeConstant(null, currentLocation.SourceDocument.GetSourceLocation(currentLocation.StartIndex, 0));
    }

    private Expression ParseUnchecked(TokenSet followers)
      //^ requires this.currentToken == Token.Unchecked;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      Expression operand = this.ParseExpression(followers|Token.RightParenthesis);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      Expression result = new UncheckedExpression(operand, slb);
      this.SkipOverTo(Token.RightParenthesis, followers);
      return result;
    }

    private Expression ParseUnaryExpression(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      switch (this.currentToken) {
        case Token.AddOne:
        case Token.SubtractOne: 
        case Token.BitwiseAnd:
        case Token.Multiply:
        case Token.Plus:
        case Token.Subtract:
        case Token.BitwiseNot:
        case Token.LogicalNot: 
          SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
          Token operatorToken = this.currentToken;
          this.GetNextToken();
          Expression operand;
          if (this.currentToken == Token.LeftParenthesis)
            operand = this.ParseCastExpression(followers);
          else
            operand = this.ParseUnaryExpression(followers);
          slb.UpdateToSpan(operand.SourceLocation);
          Expression result = AllocateUnaryExpression(operatorToken, operand, slb);
          //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
          return result;

        case Token.Exists:
        case Token.Forall:
          return this.ParseQuantifier(followers);
        case Token.Lambda:
          Expression lambda = this.ParseQuantifier(followers|Token.LeftBracket);
          return ParsePostFix(lambda, followers);


        case Token.LeftParenthesis:
          return this.ParseCastExpression(followers);

        case Token.AlignOf:
          return this.ParseAlignOf(followers);

        case Token.Sizeof: 
          return this.ParseSizeof(followers);

        case Token.LeftBrace:
          return this.ParseInitializer(followers);

        default:
          return this.ParsePostfixExpression(followers);
      }
    }

    private static Expression AllocateUnaryExpression(Token operatorToken, Expression operand, SourceLocationBuilder slb)
      //^ requires operatorToken == Token.AddOne || operatorToken == Token.BitwiseAnd || operatorToken == Token.BitwiseNot || operatorToken == Token.LogicalNot || 
      //^ operatorToken == Token.Multiply || operatorToken == Token.Plus || operatorToken == Token.Subtract || operatorToken == Token.SubtractOne;
    {
      switch (operatorToken) {
        case Token.AddOne: return new PrefixIncrement(new TargetExpression(operand), slb);
        case Token.BitwiseNot: return new OnesComplement(operand, slb);
        case Token.LogicalNot: return new LogicalNot(operand, slb);
        case Token.Multiply: return new VccAddressDereference(operand, slb);
        case Token.Plus: return new UnaryPlus(operand, slb);
        case Token.Subtract: return new UnaryNegation(operand, slb);
        case Token.SubtractOne: return new PrefixDecrement(new TargetExpression(operand), slb);
        case Token.BitwiseAnd: 
          VccPointerScopedName pointerScopedName = operand as VccPointerScopedName;
          if (pointerScopedName != null)
            return new VccPointerScopedName(new VccAddressOf(new AddressableExpression(pointerScopedName.Qualifier), slb), pointerScopedName.SimpleName, pointerScopedName.SourceLocation);
          else 
            return new VccAddressOf(new AddressableExpression(operand), slb);
        default:
          //^ assume false;
          goto case Token.AddOne;
      }
    }

    private Expression ParseTypeArgumentList(Expression expression, TokenSet followers) {
      Scanner.Snapshot snapshot = scanner.MakeSnapshot();
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      slb.UpdateToSpan(expression.SourceLocation);
      this.GetNextToken();
      if (TypeStart[this.currentToken] && (this.currentToken != Token.Identifier || this.typedefExpressions.ContainsKey(this.scanner.GetIdentifierString()))) {
        TokenSet followersOrCommaOrGreaterThan = followers|Token.Comma|Token.GreaterThan;
        List<TypeExpression> arguments = new List<TypeExpression>();
        if (this.currentToken != Token.GreaterThan) {
          TypeExpression argument = this.ParseTypeExpression(followersOrCommaOrGreaterThan);
          arguments.Add(argument);
          while (this.currentToken == Token.Comma) {
            this.GetNextToken();
            argument = this.ParseTypeExpression(followersOrCommaOrGreaterThan);
            arguments.Add(argument);
          }
        }
        arguments.TrimExcess();
        this.SkipOverTo(Token.GreaterThan, followers);
        slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        return new GenericInstanceExpression(expression, arguments, slb);
      }
      scanner.RevertToSnapshot(snapshot);
      this.currentToken = Token.LessThan;
      return expression;
      
    }

    private Expression ParsePostfixExpression(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      //TODO: first try to parse an initializer
      Expression expression = this.ParsePrimaryExpression(followers|Token.LeftBracket|Token.LeftParenthesis|Token.Dot|Token.Arrow|Token.ScopeResolution|Token.AddOne|Token.SubtractOne);
      return this.ParsePostFix(expression, followers);
    }

    private Expression ParsePostFix(Expression expression, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      for (; ; ) {
        switch (this.currentToken) {
          case Token.AddOne:
            SourceLocationBuilder slb = new SourceLocationBuilder(expression.SourceLocation);
            slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
            this.GetNextToken();
            expression = new PostfixIncrement(new TargetExpression(expression), slb);
            break;
          case Token.SubtractOne:
            slb = new SourceLocationBuilder(expression.SourceLocation);
            slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
            this.GetNextToken();
            expression = new PostfixDecrement(new TargetExpression(expression), slb);
            break;
          case Token.LessThan:
            Expression exp = this.ParseTypeArgumentList(expression, followers);
            if (exp == expression) goto default;
            expression = exp;
            break;
          case Token.Arrow:
          case Token.ScopeResolution:
          case Token.Dot:
          case Token.LeftBracket:
            expression = this.ParseIndexerCallOrSelector(expression, followers|Token.AddOne|Token.SubtractOne);
            break;
          case Token.LeftParenthesis:
            //TODO: try to parse an initializer
            goto case Token.Arrow;
          default:
            return expression;
        }
      }
    }

    private Expression ParsePrimaryExpression(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      ISourceLocation sctx = this.scanner.SourceLocationOfLastScannedToken;
      Expression expression = new DummyExpression(sctx);
      switch (this.currentToken) {
        case Token.Identifier:
          SimpleName name = this.ParseSimpleName(followers|Token.Dot);
          if (this.resultIsAKeyword && name.Name.UniqueKey == this.compilation.NameTable.Result.UniqueKey)
            expression = new VccReturnValue(name.SourceLocation);
          else if (name.Name.Value == "__this")
            expression = new VccThisReference(name.SourceLocation);
          else
            expression = name;
          break;
        case Token.IntegerLiteral:
          expression = this.ParseIntegerLiteral();
          break;
        case Token.OctalLiteral:
          expression = this.ParseOctalLiteral();
          break;
        case Token.HexLiteral:
          expression = this.ParseHexLiteral();
          break;
        case Token.RealLiteral:
          expression = this.ParseRealLiteral();
          break;
        case Token.HexFloatLiteral:
          expression = this.ParseHexFloatLiteral();
          break;
        case Token.SByteLiteral:
          expression = new CompileTimeConstant((sbyte)this.scanner.charLiteralValue, false, sctx);
          this.GetNextToken();
          break;
        case Token.CharLiteral:
          expression = new CompileTimeConstant((char)this.scanner.charLiteralValue, false, sctx);
          this.GetNextToken();
          break;
        case Token.MultiCharLiteral:
          expression = new CompileTimeConstant(this.scanner.charLiteralValue, false, sctx);
          this.GetNextToken();
          break;
        case Token.SByteStringLiteral:
          expression = this.ParseSbyteStringLiteral();
          break;
        case Token.StringLiteral:
          expression = new CompileTimeConstant(this.scanner.GetString(), false, sctx);
          this.GetNextToken();
          break;
        case Token.LeftParenthesis:
          expression = this.ParseParenthesizedExpression(false, followers);
          break;
        case Token.Old:
          expression = this.ParseOld(followers);
          break;
        case Token.Unchecked:
          expression = this.ParseUnchecked(followers);
          break;
        default:
          if (Parser.InfixOperators[this.currentToken]) {
            this.HandleError(Error.InvalidExprTerm, this.scanner.GetTokenSource());
            //^ assume this.currentToken != Token.EndOfFile; //should not be a member of InfixOperators
            this.GetNextToken();
          } else
            this.SkipTo(followers|Parser.PrimaryStart, Error.InvalidExprTerm, this.scanner.GetTokenSource());
          if (Parser.PrimaryStart[this.currentToken]) return this.ParsePrimaryExpression(followers);
          break;
      }
      this.SkipTo(followers);
      return expression;
    }

    private Expression ParseSbyteStringLiteral()
      //^ requires this.currentToken == Token.SByteStringLiteral;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      StringBuilder sb = new StringBuilder();
      string/*?*/ str = this.scanner.GetString();
      if (str != null) sb.Append(str);
      this.GetNextToken();
      while (this.currentToken == Token.SByteStringLiteral) {
        slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        str = this.scanner.GetString();
        if (str != null) sb.Append(str);
        this.GetNextToken();
      }
      return new VccByteStringLiteral(sb.ToString(), slb);
    }

    private Expression ParseCastExpression(TokenSet followers)
      //^ requires this.currentToken == Token.LeftParenthesis;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      if (this.CurrentTokenStartsTypeExpression()) {
        TypeExpression targetType = this.ParseTypeExpression(followers|Token.RightParenthesis);
        this.Skip(Token.RightParenthesis);
        Expression valueToCast = this.ParseUnaryExpression(followers);
        VccInitializerBase initializer = valueToCast as VccInitializerBase;
        if (initializer != null)
          initializer.structureTypeExpression = targetType as VccNamedTypeExpression; // null does not hurt here
        slb.UpdateToSpan(valueToCast.SourceLocation);
        Expression expression = new Cast(valueToCast, targetType, slb);
        this.SkipTo(followers);
        return expression;
      }else{
        Expression expression = this.ParseExpression(true, followers|Token.RightParenthesis|Token.LeftBracket|Token.LeftParenthesis|Token.Dot|Token.Arrow|Token.ScopeResolution|Token.AddOne|Token.SubtractOne);
        slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        expression = new Parenthesis(expression, slb);
        this.SkipOverTo(Token.RightParenthesis, followers|Token.LeftBracket|Token.LeftParenthesis|Token.Dot|Token.Arrow|Token.ScopeResolution|Token.AddOne|Token.SubtractOne);
        return this.ParsePostFix(expression, followers);
      }
    }

    private bool CurrentTokenStartsDeclaration() {
      return Parser.DeclarationStart[this.currentToken] && (this.currentToken != Token.Identifier || this.typedefExpressions.ContainsKey(this.scanner.GetIdentifierString()));
    }

    private bool CurrentTokenStartsTypeExpression() {
      return Parser.TypeStart[this.currentToken] &&
        (this.currentToken != Token.Identifier ||
          (this.typedefExpressions.ContainsKey(this.scanner.GetIdentifierString()) &&
           !this.locallyDefinedNames.ContainsKey(this.scanner.GetIdentifierString())));
    }

    private Expression ParseQualifiedName(Expression qualifier, TokenSet followers)
      //^ requires this.currentToken == Token.Arrow || this.currentToken == Token.Dot || this.currentToken == Token.ScopeResolution;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      Token tok = this.currentToken;
      SourceLocationBuilder slb = new SourceLocationBuilder(qualifier.SourceLocation);
      this.GetNextToken();
      VccSimpleName name = this.ParseSimpleName(followers);
      slb.UpdateToSpan(name.SourceLocation);
      Expression result;
      if (tok == Token.Arrow)
        result = new PointerQualifiedName(qualifier, name, slb);
      else if (tok == Token.ScopeResolution)
        result = new VccPointerScopedName(qualifier, name, slb);
      else {
        //^ assert tok == Token.Dot 
        result = new QualifiedName(qualifier, name, slb);
      }
      //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      return result;
    }

    private Expression ParseQuantifier(TokenSet followers)
      //^ requires this.currentToken == Token.Exists || this.currentToken == Token.Forall;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      Token tok = this.currentToken;
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      this.Skip(Token.LeftParenthesis);
      TokenSet followersOrLeftBraceOrRightParenthesisOrSemicolonOrUnaryStart = followers|Parser.LeftBraceOrRightParenthesisOrSemicolonOrUnaryStart;
      List<LocalDeclarationsStatement> boundVariables = this.ParseQuantifierBoundVariables(followersOrLeftBraceOrRightParenthesisOrSemicolonOrUnaryStart);
      IEnumerable<IEnumerable<Expression>> triggers = this.ParseQuantifierTriggers(followersOrLeftBraceOrRightParenthesisOrSemicolonOrUnaryStart);
      Expression condition = this.ParseExpression(followers|Token.RightParenthesis|Token.Semicolon);
      Expression body = null;
      if (this.currentToken == Token.Semicolon || tok == Token.Lambda) {
        // for the case of __regionunion, we want error message if there is ) not ;
        this.Skip(Token.Semicolon);
        body = this.ParseExpression(followers | Token.RightParenthesis);
        if (tok == Token.Forall) {
          condition = new Implies(condition, body, slb);
          body = null;
        } else if (tok == Token.Exists) {
          condition = new VccLogicalAnd(condition, body, slb);
          body = null;
        }
      }
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.Skip(Token.RightParenthesis);
      Expression result;
      if (tok == Token.Exists)
        result = new Exists(boundVariables, condition, slb);
      else if (tok == Token.Forall)
        result = new Forall(boundVariables, condition, slb);
      else if (tok == Token.Lambda)
        result = new VccLambda(boundVariables, condition, body, slb);
      else {
        //^ assert false;
        result = null;
      }
      this.compilation.ContractProvider.AssociateTriggersWithQuantifier(result, triggers);
      //^ assume followers[this.currentToken] || this.currentToken == Token.EndOfFile;
      return result;
    }

    private IEnumerable<IEnumerable<Expression>> ParseQuantifierTriggers(TokenSet followers) {
      List<IEnumerable<Expression>> result = new List<IEnumerable<Expression>>();
      while (this.currentToken == Token.LeftBrace) {
        IEnumerable<Expression>/*?*/ trigger = this.ParseQuantifierTrigger(followers);
        if (trigger != null) result.Add(trigger);
      }
      result.TrimExcess();
      return result.AsReadOnly();
    }

    private IEnumerable<Expression>/*?*/ ParseQuantifierTrigger(TokenSet followers) 
      //^ requires this.currentToken == Token.LeftBrace;
    {
      this.GetNextToken();
      if (this.currentToken == Token.RightBrace) return null;
      List<Expression> result = new List<Expression>();
      TokenSet followersOrCommaOrRightBrace = followers|Parser.CommaOrRightBrace;
      while (this.currentToken != Token.RightBrace) {
        Expression e = this.ParseExpression(followersOrCommaOrRightBrace);
        result.Add(e);
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      this.SkipOverTo(Token.RightBrace, followers);
      result.TrimExcess();
      return result.AsReadOnly();
    }

    private List<LocalDeclarationsStatement> ParseQuantifierBoundVariables(TokenSet followers) {
      List<LocalDeclarationsStatement> result = new List<LocalDeclarationsStatement>();
      TokenSet followersOrTypeStart = followers|Parser.TypeStart;
      while (this.CurrentTokenStartsTypeExpression()) {
        List<Specifier> specifiers = this.ParseSpecifiers(null, null, followers|Token.Semicolon);
        List<LocalDeclaration> declarations = new List<LocalDeclaration>(1);
        Declarator declarator = this.ParseDeclarator(followers|Token.Comma|Token.Semicolon, true);
        TypeExpression type = this.GetTypeExpressionFor(specifiers, declarator);
        SourceLocationBuilder slb = new SourceLocationBuilder(type.SourceLocation);
        slb.UpdateToSpan(declarator.SourceLocation);
        declarations.Add(new LocalDeclaration(false, false, declarator.Identifier, null, slb));
        while (this.currentToken == Token.Comma) {
          this.GetNextToken();
          declarator = this.ParseDeclarator(followers|Token.Comma, true);
          slb.UpdateToSpan(declarator.SourceLocation);
          declarations.Add(new LocalDeclaration(false, false, declarator.Identifier, null, slb));
        }
        LocalDeclarationsStatement locDecls = new LocalDeclarationsStatement(false, false, false, type, declarations, slb);
        result.Add(locDecls);
        this.SkipSemiColon(followersOrTypeStart);
      }
      return result;
    }

    private Expression ParseOld(TokenSet followers)
      //^ requires this.currentToken == Token.Old;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      Expression result;
      this.Skip(Token.LeftParenthesis);
      Expression expr = this.ParseExpression(followers|Token.RightParenthesis);
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      result = new OldValue(expr, slb);
      this.SkipOverTo(Token.RightParenthesis, followers);
      return result;
    }

    private Expression ParseAlignOf(TokenSet followers)
    //^ requires this.currentToken == Token.AlignOf;
    //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      Expression result;
      bool skipRightParenthesis = false;
      if (this.currentToken == Token.LeftParenthesis) {
        this.GetNextToken();
        skipRightParenthesis = true;
      }
      TypeExpression typeExpression = this.ParseTypeExpression(followers | Token.RightParenthesis);
      slb.UpdateToSpan(typeExpression.SourceLocation);
      result = new VccAlignOf(typeExpression, slb);
      if (skipRightParenthesis) {
        slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        this.Skip(Token.RightParenthesis);
      }
      this.SkipTo(followers);
      return result;
    }

    private Expression ParseSizeof(TokenSet followers)
      //^ requires this.currentToken == Token.Sizeof;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      this.GetNextToken();
      Expression result;
      bool skipRightParenthesis = false;
      if (this.currentToken == Token.LeftParenthesis) {
        this.GetNextToken();
        skipRightParenthesis = true;
      }
      Expression expression;
      if (this.CurrentTokenStartsTypeExpression())
        expression = this.ParseTypeExpression(followers|Token.RightParenthesis);
      else
        expression = this.ParseExpression(followers|Token.RightParenthesis);
      slb.UpdateToSpan(expression.SourceLocation);
      result = new VccSizeOf(expression, slb);
      if (skipRightParenthesis) {
        slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        this.Skip(Token.RightParenthesis);
      }
      this.SkipTo(followers);
      return result;
    }

    private TypeExpression ParseTypeExpression(TokenSet followers)
      //^ requires this.currentToken == Token.LeftParenthesis;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      List<INamespaceDeclarationMember> members = new List<INamespaceDeclarationMember>();
      List<Specifier> specifiers = this.ParseSpecifiers(members, null, followers|Token.Multiply|Token.RightParenthesis);
      Declarator declarator = this.ParseDeclarator(followers);
      slb.UpdateToSpan(declarator.SourceLocation);
      TypeExpression type = this.GetTypeExpressionFor(this.GetTypeExpressionFor(specifiers, (IdentifierDeclarator)null), declarator);
      this.SkipTo(followers);
      return type;
    }

    private Expression ParseIndexerCallOrSelector(Expression expression, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      while (true) {
        switch (this.currentToken) {
          case Token.Arrow:
          case Token.ScopeResolution:
          case Token.Dot:
            expression = this.ParseQualifiedName(expression, followers|Token.Arrow|Token.ScopeResolution|Token.Dot|Token.LeftBracket|Token.LeftParenthesis|Token.LessThan);
            break;
          case Token.LeftBracket:
            expression = this.ParseIndexer(expression, followers|Token.Arrow|Token.ScopeResolution|Token.Dot|Token.LeftBracket|Token.LeftParenthesis|Token.LessThan);
            break;
          case Token.LeftParenthesis:
            expression = this.ParseMethodCall(expression, followers|Token.Arrow|Token.ScopeResolution|Token.Dot|Token.LeftBracket|Token.LeftParenthesis|Token.LessThan);
            break;
          default: 
            this.SkipTo(followers);
            return expression;
        }
      }
    }

    private Expression ParseMethodCall(Expression method, TokenSet followers)
      //^ requires this.currentToken == Token.LeftParenthesis;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SimpleName methodName = method as SimpleName;
      if (methodName != null && methodName.Name.Value == "_vcc_atomic_op")
        return ParseAtomicOp(method, followers);
      SourceLocationBuilder slb = new SourceLocationBuilder(method.SourceLocation);
      return new VccMethodCall(method, this.ParseArgumentList(slb, followers).AsReadOnly(), slb);
    }

    private Expression ParseAtomicOp(Expression method, TokenSet followers) {
      SourceLocationBuilder slb = new SourceLocationBuilder(method.SourceLocation);
      bool saveResultIsAKeyWord = this.resultIsAKeyword;
      this.resultIsAKeyword = true;
      var args = this.ParseArgumentList(slb, followers);
      this.resultIsAKeyword = saveResultIsAKeyWord;
      var atomicOp = new VccAtomicOp(method, args.AsReadOnly(), slb);
      var atomicOpBlock = new VccAtomicOpBlock(new List<Statement>(0), atomicOp, method.SourceLocation);
      return new BlockExpression(atomicOpBlock, atomicOp, method.SourceLocation);
    }

    private void ParsePragma() 
      //^ requires this.currentToken == Token.Pragma;
    {
      //Just ignore the pragma for the time being.
      Token tok = this.scanner.GetNextToken();
      if (tok == Token.LeftParenthesis) {
        int count = 1;
        do {
          tok = this.scanner.GetNextToken();
          if (tok == Token.LeftParenthesis)
            count++;
          else if (tok == Token.RightParenthesis)
            count--;
        } while (count > 0);
        this.GetNextToken();
      }
    }

    private List<Expression> ParseArgumentList(SourceLocationBuilder slb, TokenSet followers)
      //^ requires this.currentToken == Token.LeftParenthesis;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      bool isSpecArgument = false;
      this.GetNextToken();
      TokenSet followersOrCommaOrRightParenthesisOrSpecification = followers|Token.Comma|Token.RightParenthesis|Token.Specification;
      List<Expression> arguments = new List<Expression>();
      if (this.currentToken != Token.RightParenthesis) {
        if (this.currentToken == Token.Specification) {
          isSpecArgument = true;
          this.GetNextToken();
        }
        Expression argument = this.ParseArgumentExpression(followersOrCommaOrRightParenthesisOrSpecification, isSpecArgument);
        arguments.Add(argument);
        while (this.currentToken == Token.Comma || this.currentToken == Token.Specification) {
          isSpecArgument = this.currentToken == Token.Specification;
          this.GetNextToken();
          argument = this.ParseArgumentExpression(followersOrCommaOrRightParenthesisOrSpecification, isSpecArgument);
          arguments.Add(argument);
        }
      }
      arguments.TrimExcess();
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      this.SkipOverTo(Token.RightParenthesis, followers);
      return arguments;
    }

    private Expression ParseArgumentExpression(TokenSet followers, bool isSpecArgument)
    {
      while (this.currentToken == Token.Specification) {
        isSpecArgument = true;
        this.GetNextToken();
      }
      if (isSpecArgument && this.currentToken == Token.Identifier && this.scanner.GetIdentifierString() == "out") {
        this.GetNextToken();
        var argExpr = this.ParseExpression(followers);
        var slb = new SourceLocationBuilder(argExpr.SourceLocation);
        return new OutArgument(new TargetExpression(argExpr), slb);
      } else 
        return this.ParseExpression(followers);
    }

    private Indexer ParseIndexer(Expression indexedObject, TokenSet followers)
      //^ requires this.currentToken == Token.LeftBracket;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder slb = new SourceLocationBuilder(indexedObject.SourceLocation);
      this.GetNextToken();
      List<Expression> indices = new List<Expression>();
      while (this.currentToken != Token.RightBracket) {
        Expression index = this.ParseExpression(followers|Token.Comma|Token.RightBracket);
        indices.Add(index);
        if (this.currentToken != Token.Comma) break;
        this.GetNextToken();
      }
      indices.TrimExcess();
      slb.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      Indexer result = new VccIndexer(indexedObject, indices.AsReadOnly(), slb);
      this.SkipOverTo(Token.RightBracket, followers);
      return result;
    }

    private Expression ParseParenthesizedExpression(bool keepParentheses, TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      SourceLocationBuilder sctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      if (this.currentToken == Token.LeftBrace) {
        Expression dummy = new DummyExpression(sctx);
        this.SkipTo(followers, Error.SyntaxError, "(");
        return dummy;
      }
      this.Skip(Token.LeftParenthesis);
      Expression result = this.ParseExpression(true, followers|Token.RightParenthesis|Token.Colon);
      if (keepParentheses) {
        sctx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
        result = new Parenthesis(result, sctx);
      }
      this.SkipOverTo(Token.RightParenthesis, followers);
      return result;
    }

    private Expression ParseOctalLiteral()
      //^ requires this.currentToken == Token.OctalLiteral;
    {
      string tokStr = this.scanner.GetTokenSource();
      SourceLocationBuilder ctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      TypeCode tc = this.scanner.ScanNumberSuffix();
      ctx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      CompileTimeConstant result;
      switch (tc) {
        case TypeCode.Single:
        case TypeCode.Double:
        case TypeCode.Decimal:
          this.HandleError(Error.ExpectedSemicolon);
          goto default;
        default:
          ulong ul = 0;
          foreach (char ch in tokStr){
            //^ assume '0' <= ch && ch <= '7'; //The scanner should not return a Token.OctalLiteral when this is not the case.
            ulong digit = (ulong)(ch - '0');
            //^ assume 0 <= digit && digit <= 7;
            ul = (ul << 3) + digit;
            if (ul > ulong.MaxValue-7) {
              this.HandleError(ctx, Error.IntOverflow);
              break;
            }
          }
          result = GetConstantOfSmallestIntegerTypeThatIncludes(ul, tc, ctx, true);
          break;
      }
      //^ assume this.currentToken == Token.OctalLiteral; //follows from the precondition
      this.GetNextToken();
      return result;
    }

    private Expression ParseHexFloatLiteral() {
      throw new NotImplementedException("The method or operation is not implemented.");
    }

    private CompileTimeConstant ParseHexLiteral()
      //^ requires this.currentToken == Token.HexLiteral;
    {
      string tokStr = this.scanner.GetTokenSource();
      //^ assume tokStr.StartsWith("0x") || tokStr.StartsWith("0X"); //The scanner should not return a Token.HexLiteral when this is not the case.
      SourceLocationBuilder ctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      TypeCode tc = this.scanner.ScanNumberSuffix();
      ctx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      CompileTimeConstant result;
      switch (tc) {
        case TypeCode.Single:
        case TypeCode.Double:
        case TypeCode.Decimal:
          this.HandleError(Error.ExpectedSemicolon);
          goto default;
        case TypeCode.Int64:
          long l;
          if (!Int64.TryParse(tokStr.Substring(2), System.Globalization.NumberStyles.HexNumber, System.Globalization.CultureInfo.InvariantCulture, out l)) {
            this.HandleError(ctx, Error.IntOverflow);
            l = 0;
          }
          result = new CompileTimeConstant(l, false, true, ctx);
          break;
          
        default:
          ulong ul;
          //^ assume tokStr.Length >= 2;
          if (!UInt64.TryParse(tokStr.Substring(2), System.Globalization.NumberStyles.HexNumber, System.Globalization.CultureInfo.InvariantCulture, out ul)) {
            this.HandleError(ctx, Error.IntOverflow);
            ul = 0;
          }
          result = GetConstantOfSmallestIntegerTypeThatIncludes(ul, tc, ctx, true);
          break;
      }
      //^ assume this.currentToken == Token.HexLiteral; //follows from the precondition
      this.GetNextToken();
      return result;
    }

    private CompileTimeConstant ParseIntegerLiteral() 
      //^ requires this.currentToken == Token.IntegerLiteral;
    {
      string tokStr = this.scanner.GetTokenSource();
      SourceLocationBuilder ctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      TypeCode tc = this.scanner.ScanNumberSuffix();
      ctx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      CompileTimeConstant result;
      switch (tc) {
        case TypeCode.Single:
          float f;
          if (!Single.TryParse(tokStr, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture, out f)) {
            this.HandleError(ctx, Error.FloatOverflow);
            f = float.NaN;
          }
          result = new CompileTimeConstant(f, false, ctx);
          break;
        case TypeCode.Double:
          double d;
          if (!Double.TryParse(tokStr, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture, out d)) {
            this.HandleError(ctx, Error.FloatOverflow);
            d = double.NaN;
          }
          result = new CompileTimeConstant(d, false, ctx);
          break;
        case TypeCode.Decimal:
          decimal m;
          if (!decimal.TryParse(tokStr, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture, out m)) {
            this.HandleError(ctx, Error.IntOverflow);
            m = decimal.Zero;
          }
          result = new CompileTimeConstant(m, false, ctx);
          break;
        default:
          ulong ul;
          if (!UInt64.TryParse(tokStr, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture, out ul)) {
            this.HandleError(ctx, Error.IntOverflow);
            ul = 0;
          }
          result = GetConstantOfSmallestIntegerTypeThatIncludes(ul, tc, ctx, false);
          break;
      }
      //^ assume this.currentToken == Token.IntegerLiteral; //follows from the precondition
      this.GetNextToken();
      return result;
    }

    private static CompileTimeConstant GetConstantOfSmallestIntegerTypeThatIncludes(ulong ul, TypeCode tc, SourceLocationBuilder ctx, bool hexOrOctal) {
      //TODO: there are more types than int and uint
      CompileTimeConstant result;
      if (ul <= int.MaxValue && tc == TypeCode.Empty)
        result = new CompileTimeConstant((int)ul, true, ctx);
      else if (ul <= uint.MaxValue && tc == TypeCode.Empty)
        result = new CompileTimeConstant((uint)ul, true, hexOrOctal, ctx);
      else if (ul <= uint.MaxValue && tc == TypeCode.UInt32)
        result = new CompileTimeConstant((uint)ul, true, ctx);
      else if (ul <= uint.MaxValue && tc == TypeCode.Int32)
        result = new CompileTimeConstant((int)ul, true, ctx);
      else if (ul <= long.MaxValue && (tc == TypeCode.Empty || tc == TypeCode.Int64))
        result = new CompileTimeConstant((long)ul, tc == TypeCode.Empty, ctx);
      else
        result = new CompileTimeConstant(ul, tc == TypeCode.Empty, tc == TypeCode.Empty && hexOrOctal, ctx);
      return result;
    }

    private static char[] nonZeroDigits = { '1', '2', '3', '4', '5', '6', '7', '8', '9' };

    private CompileTimeConstant ParseRealLiteral()
      //^ requires this.currentToken == Token.RealLiteral;
    {
      string tokStr = this.scanner.GetTokenSource();
      SourceLocationBuilder ctx = new SourceLocationBuilder(this.scanner.SourceLocationOfLastScannedToken);
      TypeCode tc = this.scanner.ScanNumberSuffix(false);
      ctx.UpdateToSpan(this.scanner.SourceLocationOfLastScannedToken);
      CompileTimeConstant result;
      string/*?*/ typeName = null;
      switch (tc) {
        case TypeCode.Single:
          typeName = "float";
          float fVal;
          if (!Single.TryParse(tokStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out fVal))
            this.HandleError(ctx, Error.FloatOverflow, typeName);
          else if (fVal == 0f && tokStr.IndexOfAny(nonZeroDigits) >= 0)
            this.HandleError(ctx, Error.FloatOverflow, typeName);
          result = new CompileTimeConstant(fVal, false, ctx);
          break;
        case TypeCode.Empty:
        case TypeCode.Double:
          typeName = "double";
          double dVal;
          if (!Double.TryParse(tokStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out dVal))
            this.HandleError(ctx, Error.FloatOverflow, typeName);
          else if (dVal == 0d && tokStr.IndexOfAny(nonZeroDigits) >= 0)
            this.HandleError(ctx, Error.FloatOverflow, typeName);
          result = new CompileTimeConstant(dVal, tc == TypeCode.Empty, ctx);
          break;
        case TypeCode.Decimal:
          typeName = "decimal";
          decimal decVal;
          if (!Decimal.TryParse(tokStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out decVal))
            this.HandleError(ctx, Error.FloatOverflow, typeName);
          result = new CompileTimeConstant(decVal, false, ctx);
          break;
        default:
          this.HandleError(Error.ExpectedSemicolon);
          goto case TypeCode.Empty;
      }
      //^ assume this.currentToken == Token.RealLiteral; //follows from the precondition
      this.GetNextToken();
      return result;
    }

    private void SkipOverTo(Token token, TokenSet followers)
      //^ requires token != Token.EndOfFile;
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      this.Skip(token);
      this.SkipTo(followers, Error.None);
    }

    private void Skip(Token token) 
      //^ requires token != Token.EndOfFile;
    {
      if (this.currentToken == token)
        this.GetNextToken();
      else {
        switch (token) {
          case Token.Colon: this.HandleError(Error.SyntaxError, ":"); break;
          case Token.Identifier: this.HandleError(Error.ExpectedIdentifier); break;
          case Token.LeftBrace: this.HandleError(Error.ExpectedLeftBrace); break;
          case Token.LeftParenthesis: this.HandleError(Error.SyntaxError, "("); break;
          case Token.RightBrace: this.HandleError(Error.ExpectedRightBrace); break;
          case Token.RightBracket: this.HandleError(Error.ExpectedRightBracket); break;
          case Token.RightParenthesis: this.HandleError(Error.ExpectedRightParenthesis); break;
          case Token.Semicolon: this.HandleError(Error.ExpectedSemicolon); break;
          default: this.HandleError(Error.UnexpectedToken, this.scanner.GetTokenSource()); break;
        }
      }
    }

    private void SkipSemiColon(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      if (this.currentToken == Token.Semicolon) {
        while (this.currentToken == Token.Semicolon) {
          this.GetNextToken();
        }
        this.SkipTo(followers);
      } else {
        this.Skip(Token.Semicolon);
        while (!this.scanner.TokenIsFirstAfterLineBreak && this.currentToken != Token.Semicolon && this.currentToken != Token.RightBrace && this.currentToken != Token.EndOfFile
          && (this.currentToken != Token.LeftBrace || !followers[Token.LeftBrace]))
          this.GetNextToken();
        if (this.currentToken == Token.Semicolon) 
          this.GetNextToken();
        this.SkipTo(followers);
      }
    }

    private void SkipTo(TokenSet followers)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      if (followers[this.currentToken]) return;
      Error error = Error.InvalidExprTerm;
      this.HandleError(error, this.scanner.GetTokenSource());
      while (!followers[this.currentToken] && this.currentToken != Token.EndOfFile)
        this.GetNextToken();
    }

    private void SkipTo(TokenSet followers, Error error, params string[] messages)
      //^ ensures followers[this.currentToken] || this.currentToken == Token.EndOfFile;
    {
      if (error != Error.None)
        this.HandleError(error, messages);
      while (!followers[this.currentToken] && this.currentToken != Token.EndOfFile)
        this.GetNextToken();
    }

    private QualifiedName RootQualifiedNameFor(TypeCode typeCode, ISourceLocation sctx)
      //^ requires typeCode == TypeCode.Boolean || typeCode == TypeCode.Byte || typeCode == TypeCode.Double || typeCode == TypeCode.Int16 ||
      //^   typeCode == TypeCode.Int32 || typeCode == TypeCode.Int64 || typeCode == TypeCode.SByte || typeCode == TypeCode.Single ||
      //^   typeCode == TypeCode.UInt16 || typeCode == TypeCode.UInt32 || typeCode == TypeCode.UInt64 || typeCode == TypeCode.Empty; 
    {
      switch (typeCode) {
        case TypeCode.Boolean: return new QualifiedName(systemNs, this.GetSimpleNameFor("Boolean"), sctx);
        case TypeCode.Byte: return new QualifiedName(systemNs, this.GetSimpleNameFor("Byte"), sctx);
        case TypeCode.Double: return new QualifiedName(systemNs, this.GetSimpleNameFor("Double"), sctx);
        case TypeCode.Int16: return new QualifiedName(systemNs, this.GetSimpleNameFor("Int16"), sctx);
        case TypeCode.Int32: return new QualifiedName(systemNs, this.GetSimpleNameFor("Int32"), sctx);
        case TypeCode.Int64: return new QualifiedName(systemNs, this.GetSimpleNameFor("Int64"), sctx);
        case TypeCode.SByte: return new QualifiedName(systemNs, this.GetSimpleNameFor("SByte"), sctx);
        case TypeCode.Single: return new QualifiedName(systemNs, this.GetSimpleNameFor("Single"), sctx);
        case TypeCode.UInt16: return new QualifiedName(systemNs, this.GetSimpleNameFor("UInt16"), sctx);
        case TypeCode.UInt32: return new QualifiedName(systemNs, this.GetSimpleNameFor("UInt32"), sctx);
        case TypeCode.UInt64: return new QualifiedName(systemNs, this.GetSimpleNameFor("UInt64"), sctx);
        default:
          //^ assert typeCode == TypeCode.Empty;
          return new QualifiedName(systemNs, this.GetSimpleNameFor("Void"), sctx);
      }
    }

    private VccSimpleName GetSimpleNameFor(string nameString) {
      IName name = this.GetNameFor(nameString);
      return new VccSimpleName(name, this.scanner.SourceLocationOfLastScannedToken);
    }

    private static readonly TokenSet AddOneOrSubtractOne;
    private static readonly TokenSet AssignmentOperators;
    private static readonly TokenSet CaseOrDefaultOrRightBrace;
    private static readonly TokenSet CaseOrColonOrDefaultOrRightBrace;
    private static readonly TokenSet CastFollower;
    private static readonly TokenSet CommaOrRightBrace;
    private static readonly TokenSet ContractStart;
    private static readonly TokenSet DeclarationStart;
    private static readonly TokenSet DeclaratorStart;
    private static readonly TokenSet EndOfFile;
    private static readonly TokenSet InfixOperators;
    private static readonly TokenSet LeftBraceOrRightParenthesisOrSemicolonOrUnaryStart;
    private static readonly TokenSet PrimaryStart;
    private static readonly TokenSet RightParenthesisOrSemicolon;
    private static readonly TokenSet SpecifierStart;
    private static readonly TokenSet SpecifierThatCombinesWithTypedefName;
    private static readonly TokenSet StatementStart;
    private static readonly TokenSet TypeStart;
    private static readonly TokenSet TypeOperator;
    private static readonly TokenSet UnaryStart;
    private static readonly TokenSet Term; //  Token belongs to first set for term-or-unary-operator (follows casts), but is not a predefined type.
    private static readonly TokenSet Predefined; // Token is a predefined type
    private static readonly TokenSet UnaryOperator; //  Token belongs to unary operator
    
    static Parser(){
      AddOneOrSubtractOne = new TokenSet();
      AddOneOrSubtractOne |= Token.AddOne;
      AddOneOrSubtractOne |= Token.SubtractOne;

      AssignmentOperators = new TokenSet();      
      AssignmentOperators |= Token.AddAssign;
      AssignmentOperators |= Token.Assign;
      AssignmentOperators |= Token.BitwiseAndAssign;
      AssignmentOperators |= Token.BitwiseOrAssign;
      AssignmentOperators |= Token.BitwiseXorAssign;
      AssignmentOperators |= Token.DivideAssign;
      AssignmentOperators |= Token.LeftShiftAssign;
      AssignmentOperators |= Token.MultiplyAssign;
      AssignmentOperators |= Token.RemainderAssign;
      AssignmentOperators |= Token.RightShiftAssign;
      AssignmentOperators |= Token.SubtractAssign;

      CaseOrDefaultOrRightBrace = new TokenSet();
      CaseOrDefaultOrRightBrace |= Token.Case;
      CaseOrDefaultOrRightBrace |= Token.Default;
      CaseOrDefaultOrRightBrace |= Token.RightBrace;

      CaseOrColonOrDefaultOrRightBrace = CaseOrDefaultOrRightBrace;
      CaseOrColonOrDefaultOrRightBrace |= Token.Colon;

      CommaOrRightBrace = new TokenSet();
      CommaOrRightBrace |= Token.Comma;
      CommaOrRightBrace |= Token.RightBrace;

      ContractStart = new TokenSet();
      ContractStart |= Token.Allocates;
      ContractStart |= Token.Ensures;
      ContractStart |= Token.Frees;
      ContractStart |= Token.Reads;
      ContractStart |= Token.Requires;
      ContractStart |= Token.Writes;

      DeclarationStart = new TokenSet();
      DeclarationStart |= Token.Declspec;
      DeclarationStart |= Token.Specification;
      DeclarationStart |= Token.Axiom;
      DeclarationStart |= Token.Typedef;
      DeclarationStart |= Token.Extern;
      DeclarationStart |= Token.Static;
      DeclarationStart |= Token.Auto;
      DeclarationStart |= Token.Register;
      DeclarationStart |= Token.Const;
      DeclarationStart |= Token.Restrict;
      DeclarationStart |= Token.Volatile;
      DeclarationStart |= Token.Inline;
      DeclarationStart |= Token.Void;
      DeclarationStart |= Token.Char;
      DeclarationStart |= Token.Short;
      DeclarationStart |= Token.Int;
      DeclarationStart |= Token.Int8;
      DeclarationStart |= Token.Int16;
      DeclarationStart |= Token.Int32;
      DeclarationStart |= Token.Int64;
      DeclarationStart |= Token.Long;
      DeclarationStart |= Token.Float;
      DeclarationStart |= Token.Double;
      DeclarationStart |= Token.Signed;
      DeclarationStart |= Token.Unsigned;
      DeclarationStart |= Token.Bool;
      //DeclarationStart |= Token.Complex;
      DeclarationStart |= Token.Struct;
      DeclarationStart |= Token.Template;
      DeclarationStart |= Token.Union;
      DeclarationStart |= Token.Enum;
      DeclarationStart |= Token.Identifier;

      DeclaratorStart = new TokenSet();
      DeclaratorStart |= Token.Multiply;
      DeclaratorStart |= Token.BitwiseXor;
      DeclaratorStart |= Token.Identifier;
      DeclaratorStart |= Token.LeftParenthesis;
      DeclaratorStart |= Token.Colon; // occurs in anonymous bitfields

      EndOfFile = new TokenSet();
      EndOfFile |= Token.EndOfFile;
      
      InfixOperators = new TokenSet();      
      InfixOperators |= Token.AddAssign;
      InfixOperators |= Token.Assign;
      InfixOperators |= Token.BitwiseAnd;
      InfixOperators |= Token.BitwiseAndAssign;
      InfixOperators |= Token.BitwiseOr;
      InfixOperators |= Token.BitwiseOrAssign;
      InfixOperators |= Token.BitwiseXor;
      InfixOperators |= Token.BitwiseXorAssign;
      InfixOperators |= Token.Conditional;
      InfixOperators |= Token.Comma;
      InfixOperators |= Token.Divide;
      InfixOperators |= Token.DivideAssign;
      InfixOperators |= Token.Equal;
      InfixOperators |= Token.Explies;
      InfixOperators |= Token.GreaterThan;
      InfixOperators |= Token.GreaterThanOrEqual;
      InfixOperators |= Token.IfAndOnlyIf;
      InfixOperators |= Token.Implies;
      InfixOperators |= Token.LeftShift;
      InfixOperators |= Token.LeftShiftAssign;
      InfixOperators |= Token.LessThan;
      InfixOperators |= Token.LessThanOrEqual;
      InfixOperators |= Token.LogicalAnd;
      InfixOperators |= Token.LogicalOr;
      InfixOperators |= Token.Multiply;
      InfixOperators |= Token.MultiplyAssign;
      InfixOperators |= Token.NotEqual;
      InfixOperators |= Token.Plus;
      InfixOperators |= Token.Remainder;
      InfixOperators |= Token.RemainderAssign;
      InfixOperators |= Token.RightShift;
      InfixOperators |= Token.RightShiftAssign;
      InfixOperators |= Token.Subtract;
      InfixOperators |= Token.SubtractAssign;
      InfixOperators |= Token.Arrow;
      InfixOperators |= Token.ScopeResolution;

      PrimaryStart = new TokenSet();
      PrimaryStart |= Token.Identifier;
      PrimaryStart |= Token.IntegerLiteral;
      PrimaryStart |= Token.OctalLiteral;
      PrimaryStart |= Token.HexLiteral;
      PrimaryStart |= Token.RealLiteral;
      PrimaryStart |= Token.HexFloatLiteral;
      PrimaryStart |= Token.CharLiteral;
      PrimaryStart |= Token.StringLiteral;
      PrimaryStart |= Token.LeftParenthesis;
      PrimaryStart |= Token.Unchecked;

      RightParenthesisOrSemicolon = new TokenSet();
      RightParenthesisOrSemicolon |= Token.RightParenthesis;
      RightParenthesisOrSemicolon |= Token.Semicolon;

      SpecifierThatCombinesWithTypedefName = new TokenSet();
      SpecifierThatCombinesWithTypedefName |= Token.Auto;
      SpecifierThatCombinesWithTypedefName |= Token.Register;
      SpecifierThatCombinesWithTypedefName |= Token.Static;
      SpecifierThatCombinesWithTypedefName |= Token.Extern;
      SpecifierThatCombinesWithTypedefName |= Token.Typedef;
      SpecifierThatCombinesWithTypedefName |= Token.Specification;
      SpecifierThatCombinesWithTypedefName |= Token.Declspec;
      SpecifierThatCombinesWithTypedefName |= Token.Short;
      SpecifierThatCombinesWithTypedefName |= Token.Long;
      SpecifierThatCombinesWithTypedefName |= Token.Signed;
      SpecifierThatCombinesWithTypedefName |= Token.Unsigned;
      SpecifierThatCombinesWithTypedefName |= Token.Const;
      SpecifierThatCombinesWithTypedefName |= Token.Restrict;
      SpecifierThatCombinesWithTypedefName |= Token.Volatile;
      //SpecifierThatCombinesWithTypedefName |= Token.Asm;
      SpecifierThatCombinesWithTypedefName |= Token.Based;
      SpecifierThatCombinesWithTypedefName |= Token.Cdecl;
      SpecifierThatCombinesWithTypedefName |= Token.Fastcall;
      SpecifierThatCombinesWithTypedefName |= Token.Inline;
      SpecifierThatCombinesWithTypedefName |= Token.Stdcall;
      SpecifierThatCombinesWithTypedefName |= Token.Identifier;
      //SpecifierThatCombinesWithTypedefName |= Token.LeftBracket;
      SpecifierThatCombinesWithTypedefName |= Token.Multiply;
      SpecifierThatCombinesWithTypedefName |= Token.Unaligned;
      SpecifierThatCombinesWithTypedefName |= Token.ScopeResolution;
      

      TypeStart = new TokenSet();
      TypeStart |= Token.Identifier;
      TypeStart |= Token.Bool;
      TypeStart |= Token.Short;
      TypeStart |= Token.Int;
      TypeStart |= Token.Long;
      TypeStart |= Token.Char;
      TypeStart |= Token.Float;
      TypeStart |= Token.Double;
      TypeStart |= Token.LeftBracket;
      TypeStart |= Token.Void;
      TypeStart |= Token.Int8;
      TypeStart |= Token.Int16;
      TypeStart |= Token.Int32;
      TypeStart |= Token.Int64;
      TypeStart |= Token.Signed;
      TypeStart |= Token.Unsigned;
      TypeStart |= Token.W64;
      TypeStart |= Token.Struct;
      TypeStart |= Token.Union;
      TypeStart |= Token.Enum;
      TypeStart |= Token.Const;
      TypeStart |= Token.Restrict;
      TypeStart |= Token.Volatile;
      TypeStart |= Token.Axiom;
      TypeStart |= Token.Unaligned;

      SpecifierStart = SpecifierThatCombinesWithTypedefName | TypeStart;
      //SpecifierStart |= Token.Asm;
      SpecifierStart |= Token.Based;
      SpecifierStart |= Token.Cdecl;
      SpecifierStart |= Token.Fastcall;
      SpecifierStart |= Token.Inline;
      SpecifierStart |= Token.Stdcall;

      TypeOperator = new TokenSet();
      TypeOperator |= Token.LeftBracket;
      TypeOperator |= Token.Multiply;
      TypeOperator |= Token.Plus;
      TypeOperator |= Token.Conditional;
      TypeOperator |= Token.LogicalNot;
      TypeOperator |= Token.BitwiseAnd;

      UnaryStart = new TokenSet();
      UnaryStart |= Token.Identifier;
      UnaryStart |= Token.LeftParenthesis;
      UnaryStart |= Token.LeftBracket;
      UnaryStart |= Token.Lambda;
      UnaryStart |= Token.AddOne;
      UnaryStart |= Token.SubtractOne;
      UnaryStart |= Token.AlignOf;
      UnaryStart |= Token.Sizeof;
      UnaryStart |= Token.Forall;
      UnaryStart |= Token.Exists;
      UnaryStart |= Token.Old;
      UnaryStart |= Token.HexLiteral;
      UnaryStart |= Token.IntegerLiteral;
      UnaryStart |= Token.OctalLiteral;
      UnaryStart |= Token.StringLiteral;
      UnaryStart |= Token.CharLiteral;
      UnaryStart |= Token.RealLiteral;
      UnaryStart |= Token.Unchecked;
      UnaryStart |= Token.Bool;
      UnaryStart |= Token.Short;
      UnaryStart |= Token.Int;
      UnaryStart |= Token.Long;
      UnaryStart |= Token.Char;
      UnaryStart |= Token.Float;
      UnaryStart |= Token.Double;
      UnaryStart |= Token.Plus;
      UnaryStart |= Token.BitwiseNot;
      UnaryStart |= Token.LogicalNot;
      UnaryStart |= Token.Multiply;
      UnaryStart |= Token.Subtract;
      UnaryStart |= Token.AddOne;
      UnaryStart |= Token.SubtractOne;
      UnaryStart |= Token.BitwiseAnd;

      LeftBraceOrRightParenthesisOrSemicolonOrUnaryStart = new TokenSet();
      LeftBraceOrRightParenthesisOrSemicolonOrUnaryStart |= Token.LeftBrace;
      LeftBraceOrRightParenthesisOrSemicolonOrUnaryStart |= Token.RightParenthesis;
      LeftBraceOrRightParenthesisOrSemicolonOrUnaryStart |= Token.Semicolon;
      LeftBraceOrRightParenthesisOrSemicolonOrUnaryStart |= Parser.UnaryStart;

      StatementStart = new TokenSet();
      StatementStart |= Parser.UnaryStart;
      StatementStart |= Parser.DeclarationStart;
      StatementStart |= Token.LeftBrace;
      StatementStart |= Token.Semicolon;
      StatementStart |= Token.If;
      StatementStart |= Token.Switch;
      StatementStart |= Token.While;
      StatementStart |= Token.Do;
      StatementStart |= Token.For;
      StatementStart |= Token.While;
      StatementStart |= Token.Assert;
      StatementStart |= Token.Assume;
      StatementStart |= Token.Break;
      StatementStart |= Token.Continue;
      StatementStart |= Token.Goto;
      StatementStart |= Token.Return;
      StatementStart |= Token.Const;
      StatementStart |= Token.Void;
      StatementStart |= Token.Unaligned;
      StatementStart |= Token.Block;

      Term = new TokenSet();
      Term |= Token.AlignOf;
      Term |= Token.Sizeof;
      Term |= Token.Old;
      Term |= Token.Identifier;
      Term |= Token.IntegerLiteral;
      Term |= Token.RealLiteral;
      Term |= Token.StringLiteral;
      Term |= Token.CharLiteral;
      Term |= Token.LeftParenthesis;
      Term |= Token.Unchecked;

      Predefined = new TokenSet();
      Predefined |= Token.Bool;
      Predefined |= Token.Short;
      Predefined |= Token.Int;
      Predefined |= Token.Long;
      Predefined |= Token.Char;
      Predefined |= Token.Float;
      Predefined |= Token.Double;
      Predefined |= Token.Void;

      UnaryOperator = new TokenSet();
      UnaryOperator |= Token.AlignOf;
      UnaryOperator |= Token.Sizeof;
      UnaryOperator |= Token.BitwiseAnd;
      UnaryOperator |= Token.Plus;
      UnaryOperator |= Token.Subtract;
      UnaryOperator |= Token.Multiply;
      UnaryOperator |= Token.BitwiseNot;
      UnaryOperator |= Token.LogicalNot;
      UnaryOperator |= Token.AddOne;
      UnaryOperator |= Token.SubtractOne;

      CastFollower = new TokenSet();
      CastFollower |= Token.Identifier;
      CastFollower |= Token.LeftParenthesis;
      CastFollower |= Token.AddOne;
      CastFollower |= Token.SubtractOne;
      CastFollower |= Token.AlignOf;
      CastFollower |= Token.Sizeof;
      CastFollower |= Token.Old;
      CastFollower |= Token.HexLiteral;
      CastFollower |= Token.IntegerLiteral;
      CastFollower |= Token.StringLiteral;
      CastFollower |= Token.CharLiteral;
      CastFollower |= Token.RealLiteral;
      CastFollower |= Token.Bool;
      CastFollower |= Token.Short;
      CastFollower |= Token.Int;
      CastFollower |= Token.Long;
      CastFollower |= Token.Char;
      CastFollower |= Token.Float;
      CastFollower |= Token.Double;
      CastFollower |= Token.BitwiseNot;
      CastFollower |= Token.LogicalNot;
      CastFollower |= Token.Unchecked;
    }

    private struct TokenSet { 
      private ulong bits0, bits1, bits2;

      //^ [Pure]
      [System.Diagnostics.DebuggerNonUserCode]
      public static TokenSet operator |(TokenSet ts, Token t){
        TokenSet result = new TokenSet();
        int i = (int)t;
        if (i < 64){
          result.bits0 = ts.bits0 | (1ul << i);
          result.bits1 = ts.bits1;
          result.bits2 = ts.bits2;
        }else if (i < 128){
          result.bits0 = ts.bits0;
          result.bits1 = ts.bits1 | (1ul << (i-64));
          result.bits2 = ts.bits2;
        }else {
          result.bits0 = ts.bits0;
          result.bits1 = ts.bits1;
          result.bits2 = ts.bits2 | (1ul << (i-128));
        }
        return result;
      }

      //^ [Pure]
      [System.Diagnostics.DebuggerNonUserCode]
      public static TokenSet operator|(TokenSet ts1, TokenSet ts2) {
        TokenSet result = new TokenSet();
        result.bits0 = ts1.bits0 | ts2.bits0;
        result.bits1 = ts1.bits1 | ts2.bits1;
        result.bits2 = ts1.bits2 | ts2.bits2;
        return result;
      }

      internal bool this[Token t] {
        get {
          int i = (int)t;
          if (i < 64)
            return (this.bits0 & (1ul << i)) != 0;
          else if (i < 128)
            return (this.bits1 & (1ul << (i-64))) != 0;
          else
            return (this.bits2 & (1ul << (i-128))) != 0;
        }
      }

      //^ static TokenSet(){
        //^ int i = (int)Token.EndOfFile;
        //^ assert 0 <= i && i <= 191;
      //^ }
    }

  }

  internal class LexicalScope {
    public LexicalScope(LexicalScope/*?*/ parentScope, ISourceLocation sourceLocation) {
      this.parentScope = parentScope;
      this.sourceLocation = sourceLocation;
    }
    ISourceLocation sourceLocation;
    LexicalScope/*?*/ parentScope;

    public ISourceLocation SourceLocation {
      get {
        return sourceLocation;
      }
    }

    public LexicalScope/*?*/ ParentScope {
      get {
        return parentScope;
      }
    }

    public string MangledName {
      get {
        return "sc" + this.GetHashCode();
      }
    }

    public string FullMangledName {
      get {
        if (parentScope == null) return "#" + this.MangledName;
        return parentScope.FullMangledName + "#" + this.MangledName;
      }
    }

    /// <summary>
    /// Accessor of a mangled name
    /// </summary>
    /// <param name="mangledName"></param>
    /// <returns></returns>
    public static string LexicalScopeOf(string mangledName) {
      int firstpos = mangledName.IndexOf('^');
      string result = "";
      //^ assert firstpos >=0 ;
      try {
        result = mangledName.Substring(firstpos + 1, mangledName.Length - firstpos - 1);
      } catch (ArgumentOutOfRangeException) {
        return "";
      }
      return result;
    }

    /// <summary>
    /// When leaving an inner scope, pop the last segment that represents the inner scope
    /// out of the string encoding. 
    /// </summary>
    /// <param name="scopeString">String that represents the scope from which we are leaving, a "#" separated
    /// string where each segment represents a scope.</param>
    /// <returns>The string that encodes the parent scope. </returns>
    public static string/*!*/ PopScopeString(string/*!*/ scopeString) {
      int positionOfExpletive = scopeString.LastIndexOf('#');
      //^ assert positionOfExpletive >=0;
      if (positionOfExpletive > 0) {
        return scopeString.Substring(0, positionOfExpletive);
      } else {
        // TODO: report an error.
        return "";
      }
    }

    public static string MangledNameWithOuterLexcialScope(string mangledName) {
      string scope = LexicalScopeOf(mangledName);
      string outerscope = PopScopeString(scope);
      if (outerscope == "") {
        // assert mangledName.IndexOf('^') >=0;
        return mangledName.Substring(0, mangledName.IndexOf('^'));
      }
      string result = new string(mangledName.ToCharArray());
      result = result.Replace(scope, outerscope);
      return result;
    }

    public static string UnmangledName(string mangledName) {
      int pos = mangledName.IndexOf('^');
      if (pos < 0) return mangledName;

      return mangledName.Substring(0, pos);
    }
  }
}