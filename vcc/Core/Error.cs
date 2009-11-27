//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
namespace Microsoft.Research.Vcc {
  public enum Error {
    None=0,

    BadHexDigit,
    ConstantExpected,
    EmptyCharConst,
    EmptySwitch,
    EndOfPPLineExpected,
    ErrorDirective,
    ExpectedDoubleQuote,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedLeftBrace,
    ExpectedRightBrace,
    ExpectedRightBracket,
    ExpectedRightParenthesis,
    ExpectedSemicolon,
    ExpectedSingleQuote,
    FloatOverflow,
    IllegalEscape,
    IllegalStatement,
    IntOverflow,
    InvalidExprTerm,
    InvalidLineNumber,
    InvalidPreprocExpr,
    LocalDuplicate,
    LowercaseEllSuffix,
    MissingPPFile,
    NewlineInConst,
    NoCommentEnd,
    PossibleMistakenNullStatement,
    PPDefFollowsToken,
    PPDirectiveExpected,
    ShiftCountOutOfRange,
    StmtNotInCase,
    SyntaxError,
    TooManyCharsInConst,
    UnescapedSingleQuote,
    UnexpectedToken,
    WarningDirective,
    ExpectedConstantExpression, 
    VccAttributeOnTypeDef,
    ArgumentShouldNotBePassedWithOutKeyword,
    ArgumentMustBePassedWithOutKeyword,
    IllegalIndirection,

    /// <summary>
    /// The declaration of function '{0}' already specifies contracts. Discarding the contracts of the definition.
    /// </summary>
    DiscardedContractAtDefinition,
    
    /// <summary>
    /// '&amp;' on bit field ignored
    /// </summary>
    AddressOfBitField,

    /// <summary>
    /// Equality '==' binds stronger than '&amp;&amp;' and '||' which is possibly not what you wanted;  use '&lt;==&gt;' or parenthesize the equality.
    /// </summary>
    PotentialPrecedenceErrorInLogicalExpression,

    /// <summary>
    /// redefinition of formal parameter '{0}'
    /// </summary>
    RedefinitionOfFormalParameter,

    /// <summary>
    /// The size of '{0}' is unknown in the current context; note that the use of sizeof may be due to SAL __in ,__inout, or __out annotations.
    /// </summary>
    SizeOfUnknown,

    /// <summary>
    /// Cannot use 'this' in this context.
    /// </summary>
    ThisNotAllowedHere,

    /// <summary>
    /// '{0}' : unknown element size
    /// </summary>
    UnknownElementSize,

    MissingSemicolonAfterStruct,

    /// <summary>
    /// subscript is not of integral type
    /// </summary>
    SubscriptNotOfIntegralType,

    /// <summary>
    /// illegal index
    /// </summary>
    IllegalIndex,

    /// <summary>
    /// Illegal update of map '{0}'
    /// </summary>
    IllegalMapUpdate,

    /// <summary>
    /// '{0}' is a reserved name and cannot be used as {1}.
    /// </summary>
    ReservedName,

    /// <summary>
    /// left of .{0} must have struct/union type
    /// </summary>
    StructRequiredForDot,

    /// <summary>
    /// Cannot determine initializer's type from context.
    /// </summary>
    UnableToDetermineTypeOfInitializer,

  }
}
