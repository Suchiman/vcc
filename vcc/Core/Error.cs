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
    LValueRequired,

    /// <summary>
    /// The declaration of function '{0}' already specifies contracts. Discarding the contracts of the definition.
    /// </summary>
    DiscardedContractAtDefinition,
    
    /// <summary>
    /// '&amp;' on bit field ignored
    /// </summary>
    AddressOfBitField,

    /// <summary>
    /// Access to the ghost member {0} requires a pointer value
    /// </summary>
    PointerExpectedForGhostMember,

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
    /// '{0}' : unknown size
    /// </summary>
    UnknownSize,

    /// <summary>
    /// '{0}' : unknown element size
    /// </summary>
    UnknownElementSize,

  }
}
