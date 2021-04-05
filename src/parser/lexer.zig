/// Lexer has all the code for lexing
/// strings into tokens
const std = @import("std");
const main = @import("../main.zig");
const token = @import("./token.zig");

// std imports
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const Token = token.Token;
const TokenType = token.TokenType;
const Tokens = ArrayList(Token);

pub const Lexer = struct {
    /// The allocator
    allocator: *std.mem.Allocator = std.heap.page_allocator,

    code: []const u8,

    /// start of the cursor that is fixed on conflicts
    start: i32 = 0,

    /// cursor that moves on along the source code 
    cursor: i32 = 0,

    /// List of tokens
    tokens: *Tokens,

    /// List of all Errors
    errors: *main.Errors,

    /// List of all Warnings
    warnings: *main.Errors,

    pub fn init(
        allocator: *Allocator,
        code: []const u8,
        errors: *main.Errors,
        warnings: *main.Errors,
        tokens: *Tokens,
    ) Lexer {
        return .{
            .allocator = allocator,
            .code = code,
            .tokens = tokens,
            .errors = errors,
            .warnings = warnings,
        };
    }

    pub fn lex(self: *@This()) !Tokens {
        return self.tokens.*;
    }

    pub fn addToken(self: *@This(), tok_type: TokenType) !void {
        try self.tokens.append(.{
            .tok_type = tok_type,
            .start = self.start,
            .end = self.cursor,
        });
    }

    pub fn addError(self: *@This(), message: []const u8) !void {
        var line: i32 = 1;
        var col: i32 = 1;
        var i = 0;
        while (i < self.start) : (i += 1) {
            if (self.code[i] == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        const tok = code[self.start..self.cursor];

        const errorMessage = std.fmt.allocPrint(
            self.allocator,
            "[{d}:{d}] {s} at `{s}`",
            .{
                line,
                col,
                message,
                tok
            },
        );

        self.errors.append(.{
            .line = line,
            .pos = self.start,
            .errorMessage = errorMessage,
            .errorType = main.CompilerErrorType.CompileError,
        });

    }
};
