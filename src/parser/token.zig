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
    Question, // '?'
    QuestionQuestion, // '??'
    QuestionQuestionAssign, // '??='
    Colon, // ':'
    Ellipsis, // '...'
    Dot, // '.'
    PlusPlus, // '++'
    MinusMinus, // '--'
    Plus, // '+'
    Minus, // '-'
    BitNot, // '~';
    Not, // '!'
    Multiply, // '*'
    MultiplyAssign, // '*='
    Exponent, // '**'
    ExponentAssign, // '**='
    Divide, // '/'
    Modulus, // '%';
    LessThan, // '<'
    LessThanEquals, // '<='
    LeftShiftArithmeticAssign, // '<<='
    LeftShiftArithmetic, // '<<';
    GreaterThan, // '>'
    GreaterThanEquals, // '>='
    RightShiftArithmetic, // '>>';
    RightShiftArithmeticAssign, // '>>='
    RightShiftLogical, // '>>>';
    RightShiftLogicalAssign, // '>>>='
    Equals, // '=='
    NotEquals, // '!='
    IdentityEquals, // '==='
    IdentityNotEquals, // '!=='
    BitAnd, // '&'
    BitAndAssign, // '&='
    And, // '&&'
    AndAssign, // '&&='
    BitOr, // '|'
    BitOrAssign, // '|='
    Or, // '||'
    OrAssign, // '||='
    BitXor, // '^'
    BitXorAssign, // '^='
    DivideAssign, // '/='
    ModulusAssign, // '%='
    PlusAssign, // '+='
    MinusAssign, // '-='
    Arrow, // '=>'

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
    Class, // 'class';
    Enum, // 'enum';
    Extends, // 'extends';
    Super, // 'super';
    Const, // 'const';
    Export, // 'export';
    Import, // 'import';
    Implements, // 'implements' ;
    Let, // 'let' ;
    Private, // 'private' ;
    Public, // 'public' ;
    Interface, // 'interface' ;
    Package, // 'package' ;
    Protected, // 'protected' ;
    Static, // 'static' ;
    Yield, // 'yield' ;
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

    EOF, // "EOF"
};

const Keywords = std.StringHashMap(TokenType);

pub const Token = struct {
    tok_type: TokenType = TokenType.EOF,

    /// Index of the start of the token in the array
    start: usize = 0,

    /// end of the token in the string stream
    end: usize = 0,

    pub fn toString(self: *const @This(), allocator: *std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "({s},{d},{d})", .{
            self.tok_type,
            self.start,
            self.end,
        });
    }
};

pub fn initKeywordsMap(allocator: *std.mem.Allocator) !*Keywords {
    var key = try allocator.create(Keywords);
    key.* = Keywords.init(allocator);
    try key.put("break", TokenType.Break); // 'break';
    try key.put("do", TokenType.Do); // 'do';
    try key.put("instanceof", TokenType.Instanceof); // 'instanceof';
    try key.put("typeof", TokenType.Typeof); // 'typeof';
    try key.put("case", TokenType.Case); // 'case';
    try key.put("else", TokenType.Else); // 'else';
    try key.put("new", TokenType.New); // 'new';
    try key.put("var", TokenType.Var); // 'var';
    try key.put("catch", TokenType.Catch); // 'catch';
    try key.put("finally", TokenType.Finally); // 'finally';
    try key.put("return", TokenType.Return); // 'return';
    try key.put("void", TokenType.Void); // 'void';
    try key.put("continue", TokenType.Continue); // 'continue';
    try key.put("for", TokenType.For); // 'for';
    try key.put("switch", TokenType.Switch); // 'switch';
    try key.put("while", TokenType.While); // 'while';
    try key.put("debugger", TokenType.Debugger); // 'debugger';
    try key.put("function", TokenType.Function); // 'function';
    try key.put("this", TokenType.This); // 'this';
    try key.put("with", TokenType.With); // 'with';
    try key.put("default", TokenType.Default); // 'default';
    try key.put("if", TokenType.If); // 'if';
    try key.put("throw", TokenType.Throw); // 'throw';
    try key.put("delete", TokenType.Delete); // 'delete';
    try key.put("in", TokenType.In); // 'in';
    try key.put("try", TokenType.Try); // 'try';
    try key.put("as", TokenType.As); // 'as';
    try key.put("from", TokenType.From); // 'from';
    try key.put("readonly", TokenType.ReadOnly); // 'readonly';
    try key.put("async", TokenType.Async); // 'async';
    try key.put("class", TokenType.Class); // 'class';
    try key.put("enum", TokenType.Enum); // 'enum';
    try key.put("extends", TokenType.Extends); // 'extends';
    try key.put("super", TokenType.Super); // 'super';
    try key.put("const", TokenType.Const); // 'const';
    try key.put("export", TokenType.Export); // 'export';
    try key.put("import", TokenType.Import); // 'import';
    try key.put("implements", TokenType.Implements); // 'implements' ;
    try key.put("let", TokenType.Let); // 'let' ;
    try key.put("private", TokenType.Private); // 'private' ;
    try key.put("public", TokenType.Public); // 'public' ;
    try key.put("interface", TokenType.Interface); // 'interface' ;
    try key.put("package", TokenType.Package); // 'package' ;
    try key.put("protected", TokenType.Protected); // 'protected' ;
    try key.put("static", TokenType.Static); // 'static' ;
    try key.put("yield", TokenType.Yield); // 'yield' ;
    try key.put("any", TokenType.Any); // 'any';
    try key.put("number", TokenType.Number); //'number';
    try key.put("boolean", TokenType.Boolean); //'boolean';
    try key.put("string", TokenType.String); //'string';
    try key.put("Symbol", TokenType.Symbol); //'symbol';
    try key.put("type", TokenType.TypeAlias); // 'type'
    try key.put("get", TokenType.Get); // 'get'
    try key.put("set", TokenType.Set); // 'set'
    try key.put("constructor", TokenType.Constructor); //'constructor';
    try key.put("namespace", TokenType.Namespace); //'namespace';
    try key.put("require", TokenType.Require); //'require';
    try key.put("module", TokenType.Module); //'module';
    try key.put("declare", TokenType.Declare); //'declare';
    try key.put("abstract", TokenType.Abstract); //'abstract';
    try key.put("is", TokenType.Is); // 'is'
    try key.put("null", TokenType.NullLiteral); // 'null'
    try key.put("true", TokenType.TrueLiteral); // 'true'
    try key.put("false", TokenType.FalseLiteral); // 'false'

    return key;
}

