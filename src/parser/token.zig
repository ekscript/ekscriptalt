/// Has Token definitions
const std = @import("std");

/// Basically an enum of all Possible Token Types
/// Referred from https://github.com/antlr/grammars-v4/blob/master/javascript/typescript/TypeScriptLexer.g4
pub const TokenType = enum {
    // --- Operators and symbold ---
    MutliLineComment, // /* */
    SingleLineComment, // //
    RegularExpressionLIteral, // /[...]/
    OpenBracket, // [
    CloseBracket, // ]
    OpenParen, // "("
    CloseParen, // ")"
    OpenBrace, // "}"
    CloseBrace, // "{"
    SemiColon, // ';'
    Comma, // ','
    Assign, // '='
    QuestionMark, // '?'
    Colon, // ':'
    Ellipsis, // '...'
    Dot, // '.'
    PlusPlus, // '++'
    MinusMinus, // '--'
    Plus, // '+'
    Minus, //'-';
    BitNot, //'~';
    Not, //'!';
    Multiply, //'*';
    Divide, //'/';
    Modulus, //'%';
    RightShiftArithmetic, //'>>';
    LeftShiftArithmetic, // '<<';
    RightShiftLogical, // '>>>';
    LessThan, // '<';
    MoreThan, // '>';
    LessThanEquals, // '<=';
    GreaterThanEquals, // '>=';
    Equals_, // '==';
    NotEquals, // '!=';
    IdentityEquals, // '===';
    IdentityNotEquals, // '!==';
    BitAnd, // '&';
    BitXOr, // '^';
    BitOr, // '|';
    And, // '&&';
    Or, // '||';
    MultiplyAssign, // '*=';
    DivideAssign, // '/=';
    ModulusAssign, // '%=';
    PlusAssign, // '+=';
    MinusAssign, // '-=';
    LeftShiftArithmeticAssign, // '<<=';
    RightShiftArithmeticAssign, // '>>=';
    RightShiftLogicalAssign, // '>>>=';
    BitAndAssign, // '&=';
    BitXorAssign, // '^=';
    BitOrAssign, // '|=';
    Arrow, // '=>';
    // -- Literals --
    NullLiteral, // 'null'
    TrueLiteral, // 'true'
    FalseLiteral, // 'false'
    NumberLiteral, // includes all binary, octal, hexadecimal, underscored stuff
    // --- Keywords --
    Break, // 'break';
    Do, // 'do';
    Instanceof, // 'instanceof';
    Typeof, // 'typeof';
    Case, // 'case';
    Else, // 'else';
    New, // 'new';
    Var, // 'var';
    Catch, // 'catch';
    Finally, // 'finally';
    Return, // 'return';
    Void, // 'void';
    Continue, // 'continue';
    For, // 'for';
    Switch, // 'switch';
    While, // 'while';
    Debugger, // 'debugger';
    Function, // 'function';
    This, // 'this';
    With, // 'with';
    Default, // 'default';
    If, // 'if';
    Throw, // 'throw';
    Delete, // 'delete';
    In, // 'in';
    Try, // 'try';
    As, // 'as';
    From, // 'from';
    ReadOnly, // 'readonly';
    Async, // 'async';
    /// Future Reserved words
    Class, // 'class';
    Enum, // 'enum';
    Extends, // 'extends';
    Super, // 'super';
    Const, // 'const';
    Export, // 'export';
    Import, // 'import';
    /// The following tokens are also considered to be FutureReservedWords
    /// when parsing strict mode
    Implements, // 'implements' ;
    Let, // 'let' ;
    Private, // 'private' ;
    Public, // 'public' ;
    Interface, // 'interface' ;
    Package, // 'package' ;
    Protected, // 'protected' ;
    Static, // 'static' ;
    Yield, // 'yield' ;
    //keywords:
    Any, // 'any';
    Number, //'number';
    Boolean, //'boolean';
    String, //'string';
    Symbol, //'symbol';
    TypeAlias, // 'type'
    Get, // 'get'
    Set, // 'set'
    Constructor, //'constructor';
    Namespace, //'namespace';
    Require, //'require';
    Module, //'module';
    Declare, //'declare';
    Abstract, //'abstract';
    Is, // 'is'
    At, // '@',
    Identifier, // Any identifier
    StringLiteral, // Double quoted and single quoted
    TemplateLiteral, // "`  `"
};

pub const Token = struct {
    tok_type: TokenType,

    /// Index of the start of the token in the array
    start: i32,

    /// end of the token in the string stream
    end: i32,

    pub fn toString(self: *const @This(), allocator: *std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "({s},{d},{d})", .{
            self.tok_type,
            self.start,
            self.end,
        });
    }

};
