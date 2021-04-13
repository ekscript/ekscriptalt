/// Lexer has all the code for lexing
/// strings into tokens
const std = @import("std");
const main = @import("../main.zig");
const token = @import("./token.zig");

// print
fn println(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("\n\x1b[34m lexer.zig > \x1b[0m" ++ fmt ++ "\n", args);
}
fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("\x1b[34m lexer.zig > \x1b[0m" ++ fmt ++ "\n", args);
}

// std imports
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Token = token.Token;
const TokenType = token.TokenType;
const Tokens = ArrayList(Token);

const Keywords = std.StringHashMap(TokenType);

pub const Lexer = struct {
    const Self = @This();

    /// The allocator
    allocator: *std.mem.Allocator = std.heap.page_allocator,

    /// string pool
    lexer_arena: ArenaAllocator,

    /// code
    code: []const u8,

    /// start of the cursor that is fixed on conflicts
    start: usize = 0,

    /// cursor that moves on along the source code 
    cursor: usize = 0,

    /// List of tokens
    tokens: *Tokens,

    /// List of all Errors
    errors: *main.Errors,

    /// List of all Warnings
    warnings: *main.Errors,

    keywords: *Keywords,

    pub fn init(
        allocator: *Allocator,
        code: []const u8,
        errors: *main.Errors,
        warnings: *main.Errors,
        tokens: *Tokens,
    ) Self {
        var lexer_arena = ArenaAllocator.init(allocator);
        var keywords = token.initKeywordsMap(&lexer_arena.allocator) catch unreachable;
        return .{
            .allocator = allocator,
            .lexer_arena = lexer_arena,
            .code = code,
            .keywords = keywords,
            .tokens = tokens,
            .errors = errors,
            .warnings = warnings,
        };
    }

    pub fn lex(self: *Self) *Tokens {
        while (!self.end()) {
            self.start = self.cursor;
            self.scanToken();
        }
        self.tokens.*.append(Token{
            .start = self.code.len,
            .end = self.code.len,
        }) catch unreachable;
        return self.tokens;
    }

    /// heart of the scanner. scans individual tokens
    fn scanToken(self: *Self) void {
        const c = self.advance();
        const TT = TokenType;

        switch (c) {
            ' ', '\n', '\r', '\t' => {},
            '(' => self.addToken(TT.OpenParen),
            ')' => self.addToken(TT.CloseParen),
            '{' => self.addToken(TT.OpenBrace),
            '}' => self.addToken(TT.CloseBrace),
            '[' => self.addToken(TT.OpenBracket),
            ']' => self.addToken(TT.CloseBracket),
            ';' => self.addToken(TT.SemiColon),
            ',' => self.addToken(TT.Comma),
            ':' => self.addToken(TT.Colon),
            '~' => self.addToken(TT.BitNot),
            '@' => self.addToken(TT.At),
            '?' => switch (self.lookAhead()) {
                '?' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.QuestionQuestionAssign, 2),
                        else => self.addTokenAdvance(TT.QuestionQuestion, 1),
                    }
                },
                else => self.addToken(TT.Question),
            },
            '+' => switch (self.lookAhead()) {
                '+' => self.addTokenAdvance(TT.PlusPlus, 1),
                '=' => self.addTokenAdvance(TT.PlusAssign, 1),
                else => self.addToken(TT.Plus),
            },
            '-' => switch (self.lookAhead()) {
                '-' => self.addTokenAdvance(TT.MinusMinus, 1),
                '=' => self.addTokenAdvance(TT.MinusAssign, 1),
                else => self.addToken(TT.Minus),
            },
            '=' => switch (self.lookAhead()) {
                '=' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.IdentityEquals, 2),
                        else => self.addTokenAdvance(TT.Equals, 1),
                    }
                },
                else => self.addToken(TT.Assign),
            },
            '*' => switch (self.lookAhead()) {
                '*' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.ExponentAssign, 2),
                        else => self.addTokenAdvance(TT.Exponent, 1),
                    }
                },
                '=' => self.addTokenAdvance(TT.MultiplyAssign, 1),
                else => self.addToken(TT.Multiply),
            },
            '%' => switch (self.lookAhead()) {
                '=' => self.addTokenAdvance(TT.ModulusAssign, 1),
                else => self.addToken(TT.Modulus),
            },
            '/' => switch (self.lookAhead()) {
                '/' => {
                    while (self.lookAhead() != '\n' and !self.end()) {
                        _ = self.advance();
                    }
                    self.addToken(TT.SingleLineComment);
                },
                '*' => {
                    while (self.lookAhead() != '*' and self.lookSuperAhead() != '/' and !self.end()) {
                        _ = self.advance();
                    }
                    self.addToken(TT.MutliLineComment);
                },
                '=' => self.addTokenAdvance(TT.DivideAssign, 1),
                else => self.addToken(TT.Divide),
            },
            '!' => switch (self.lookAhead()) {
                '=' => self.addTokenAdvance(if (self.match('=')) TT.IdentityNotEquals else TT.NotEquals, 1),
                else => self.addToken(TT.Not),
            },
            '&' => switch (self.lookAhead()) {
                '&' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.AndAssign, 2),
                        else => self.addTokenAdvance(TT.And, 1),
                    }
                },
                '=' => self.addTokenAdvance(TT.BitAndAssign, 1),
                else => self.addToken(TT.BitAnd),
            },
            '|' => switch (self.lookAhead()) {
                '|' => {
                    switch (self.lookSuperAhead()) {
                        '=' => self.addTokenAdvance(TT.OrAssign, 2),
                        else => self.addTokenAdvance(TT.Or, 1),
                    }
                },
                '=' => self.addTokenAdvance(TT.BitOrAssign, 1),
                else => self.addToken(TT.BitOr),
            },
            '^' => self.addToken(if (self.match('=')) TT.BitXorAssign else TT.BitXor),
            '.' => switch (self.lookAhead()) {
                '.' => {
                    switch (self.lookSuperAhead()) {
                        '.' => self.addTokenAdvance(TT.Ellipsis, 2),
                        else => self.addError("Invalid character. Did you mean '...' or '.' ?"),
                    }
                },
                else => self.addToken(TT.Dot),
            },
            '<' => switch (self.lookAhead()) {
                '=' => self.addTokenAdvance(TT.LessThanEquals, 1),
                '<' => switch (self.lookSuperAhead()) {
                    '=' => self.addTokenAdvance(TT.LeftShiftArithmeticAssign, 2),
                    else => self.addTokenAdvance(TT.LeftShiftArithmetic, 1),
                },
                else => self.addToken(TT.LessThan),
            },
            '>' => switch (self.lookAhead()) {
                '=' => self.addTokenAdvance(TT.GreaterThanEquals, 1),
                '>' => switch (self.lookSuperAhead()) {
                    '=' => self.addTokenAdvance(TT.LeftShiftArithmetic, 2),
                    '>' => switch (self.lookSuperDuperAhead()) {
                        '=' => self.addTokenAdvance(TT.RightShiftLogicalAssign, 3),
                        else => self.addTokenAdvance(TT.RightShiftLogical, 2),
                    },
                    else => self.addTokenAdvance(TT.LeftShiftArithmeticAssign, 1),
                },
                else => self.addToken(TT.GreaterThan),
            },
            '"', '\'' => self.string(c),
            '`' => self.templateLiteral(),
            else => {
                if (std.ascii.isDigit(c)) {
                    self.number();
                } else if (std.ascii.isAlpha(c) or c == '_') {
                    self.identifier();
                } else {
                    self.addError("Unexpected character");
                }
            },
        }
    }

    /// string lexing
    /// TODO: add support for advance escape literals
    fn string(self: *Self, c: u8) void {
        var currentChar: u8 = self.lookAhead();
        while (!self.end()) : ({
            _ = self.advance();
            currentChar = self.lookAhead();
        }) {
            if (currentChar == c) {
                break;
            } else if (currentChar == '\\') {
                _ = self.advance();
            }
        }

        if (self.end()) {
            self.addError("Unterminated string");
            return;
        }
        _ = self.advance();

        // trim the surrounding quotes
        self.tokens.append(.{
            .tok_type = TokenType.String,
            .start = self.start + 1,
            .end = self.cursor - 1,
        }) catch unreachable;
    }

    /// TODO:
    /// process identifier and keywords
    fn identifier(self: *Self) void {
        var ahead = self.lookAhead();
        while (std.ascii.isAlNum(ahead) or ahead == '_') : (ahead = self.lookAhead()) {
            _ = self.advance();
        }
        const text = self.code[self.start..self.cursor];
        var tokType = self.keywords.get(text);
        self.addToken(tokType orelse TokenType.Identifier);
    }

    /// template literals
    /// TODO: add support for escape sequences.
    /// TODO: use this again during parsing to get the tokens back
    fn templateLiteral(self: *Self) void {
        var currentChar: u8 = self.lookAhead();
        while (!self.end()) : ({
            _ = self.advance();
            currentChar = self.lookAhead();
        }) {
            if (currentChar == '`') {
                break;
            } else if (currentChar == '\\') {
                _ = self.advance();
            }
        }

        if (self.end()) {
            self.addError("Unterminated string");
            return;
        }
        _ = self.advance();

        // trim the surrounding quotes
        self.tokens.append(.{
            .tok_type = TokenType.TemplateLiteral,
            .start = self.start + 1,
            .end = self.cursor - 1,
        }) catch unreachable;
    }

    /// number lexing
    fn number(self: *Self) void {
        while (std.ascii.isDigit(self.lookAhead()))
            _ = self.advance();

        if (self.lookAhead() == '.' and std.ascii.isDigit(self.lookSuperAhead())) {
            _ = self.advance();
            while (std.ascii.isDigit(self.lookAhead())) {
                _ = self.advance();
            }
        }

        self.addToken(TokenType.Number);
    }

    /// look one character ahead
    fn lookAhead(self: *Self) u8 {
        return if (self.cursor >= self.code.len) 0 else self.code[self.cursor];
    }

    /// look two characters ahead
    fn lookSuperAhead(self: *Self) u8 {
        if (self.cursor >= self.code.len) return 0;
        if (self.cursor + 1 >= self.code.len) return 0;
        return self.code[self.cursor + 1];
    }

    fn lookSuperDuperAhead(self: *Self) u8 {
        if (self.lookSuperAhead() != 0) {
            if (self.cursor + 2 >= self.code.len) return 0;
            return self.code[self.cursor + 2];
        }
        return 0;
    }

    fn match(self: *Self, expectedChar: u8) bool {
        if (self.end()) return false;
        if (self.code[self.cursor] != expectedChar) return false;
        self.*.cursor += 1;
        return true;
    }

    fn advance(self: *Self) u8 {
        self.*.cursor += 1;
        return self.*.code[self.cursor - 1];
    }

    fn end(self: *Self) bool {
        return self.*.cursor >= self.code.len;
    }

    pub fn addTokenAdvance(self: *Self, tok_type: TokenType, steps: usize) void {
        self.*.cursor += steps;
        self.addToken(tok_type);
    }

    pub fn addToken(self: *Self, tok_type: TokenType) void {
        self.tokens.append(.{
            .tok_type = tok_type,
            .start = self.start,
            .end = self.cursor,
        }) catch unreachable;
    }

    pub fn addError(self: *Self, message: []const u8) void {
        var line: usize = 1;
        var col: usize = 1;
        var i: usize = 0;
        while (i < self.start) : (i += 1) {
            if (self.code[i] == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        const tok = self.code[self.start..self.cursor];

        const errorMessage = std.fmt.allocPrint(
            &self.lexer_arena.allocator,
            "[{d}:{d}] {s} at `{s}`",
            .{
                line,
                col,
                message,
                tok,
            },
        ) catch unreachable;

        self.errors.append(main.Error{
            .line = line,
            .pos = self.start,
            .errorMessage = errorMessage,
            .errorType = main.CompilerErrorType.CompileError,
        }) catch unreachable;
    }

    pub fn deinit(self: *Self) void {
        self.lexer_arena.deinit();
    }
};
