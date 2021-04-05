/// Lexer has all the code for lexing
/// strings into tokens
const std = @import("std");
const token = @import("./token.zig");

// std imports
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const Token = token.Token;
const TokenType = token.TokenType;
const Tokens = ArrayList(Token);

pub const Lexer = struct {
    code: []const u8,

    /// cursor that moves on along the source code 
    cursor: i32 = 0,

    /// List of tokens
    tokens: Tokens,

    pub fn init(code: []const u8) Lexer {
        return .{
            .code = code,
            .tokens = Tokens.init(std.heap.page_allocator),
        };
    }

    pub fn lex(self: *Self) Tokens {
        return self.tokens;
    }
};
