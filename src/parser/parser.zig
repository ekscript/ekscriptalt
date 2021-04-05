// The Parser that produces an AST from source code
// It also does the lexing
const std = @import("std");
const lexer = @import("./lexer.zig");
const main = @import("../main.zig");
const ast = @import("../ast.zig");
const token = @import("./token.zig");

// defining the type of the errors

const Allocator = std.mem.Allocator;
const Tokens = std.ArrayList(token.Token);

pub const Parser = struct {
    allocator: *Allocator = std.heap.page_allocator,
    source_code: []const u8,
    errors: *main.Errors,
    warnings: *main.Errors,
    tokens: Tokens,

    pub fn init(
        allocator: *Allocator,
        source_code: []const u8,
        errors: *main.Errors,
        warnings: *main.Errors,
    ) Parser {
        var tokens = Tokens.init(allocator);

        return Parser{
            .allocator = allocator,
            .source_code = source_code,
            .errors = errors,
            .warnings = warnings,
            .tokens = tokens,
        };
    }

    pub fn parse(self: *@This()) !ast.Tree {
        var lexInstance = lexer.Lexer.init(
            self.allocator,
            self.source_code,
            self.errors,
            self.warnings,
            &self.tokens,
        );
        self.tokens = try lexInstance.lex();
        return ast.Tree{};
    }

    pub fn deinit(self: *@This()) void {
        self.tokens.deinit();
    }
};
