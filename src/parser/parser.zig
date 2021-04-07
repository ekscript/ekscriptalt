// The Parser that produces an AST from source code
// It also does the lexing
const std = @import("std");
const lexer = @import("./lexer.zig");
const main = @import("../main.zig");
const ast = @import("../ast.zig");
const token = @import("./token.zig");

const heap = std.heap;
const TokenType = token.TokenType;
const Token = token.Token;
const Tokens = std.ArrayList(Token);
const Allocator = std.mem.Allocator;

fn getFileContent(map: main.SourceCodes, file: []const u8) []const u8 {
    return map.get(file) orelse "1 + 1;";
}

pub const Parser = struct {
    const Self = @This();

    allocator: *Allocator = heap.page_allocator,
    parser_arena: heap.ArenaAllocator,

    options: main.CompilerOptions,
    source_code: []const u8,

    errors: *main.Errors,
    warnings: *main.Errors,
    tokens: *Tokens,
    lexer: lexer.Lexer,

    cursor: Token,

    pub fn init(
        allocator: *Allocator,
        errors: *main.Errors,
        warnings: *main.Errors,
        options: main.CompilerOptions,
    ) Parser {
        var tokens = allocator.create(Tokens) catch |err| {
            unreachable;
        };
        tokens.* = Tokens.init(allocator);
        var tok = if (tokens.*.items.len != 0) tokens.*.items[0] else Token{};

        const source_code = getFileContent(options.sourceCodes, options.entry);

        var lexInstance = lexer.Lexer.init(
            allocator,
            source_code,
            errors,
            warnings,
            tokens,
        );
        const sourceCodes = options.sourceCodes;
        return Parser{
            .allocator = allocator,
            .source_code = source_code,
            .options = options,
            .errors = errors,
            .warnings = warnings,
            .tokens = tokens,
            .lexer = lexInstance,
            .parser_arena = heap.ArenaAllocator.init(allocator),
            .cursor = tok,
        };
    }

    pub fn parse(self: *@This()) !ast.Tree {
        self.tokens = self.lexer.lex();
        return ast.Tree{};
    }

    pub fn addError(self: *@This()) void {
        var line: usize = 1;
        var col: usize = 1;
        var i: usize = 0;
        while (i < self.cursor.start) : (i += 1) {
            if (self.code[i] == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        const tok = self.code[self.cursor.start..self.cursor.end];

        const errorMessage = std.fmt.allocPrint(
            &self.parser_arena.allocator,
            "[{d}:{d}] {s} at `{s}`",
            .{
                line,
                col,
                message,
                tok,
            },
        ) catch unreachable;

        self.errors.append(.{
            .line = line,
            .pos = self.start,
            .errorMessage = errorMessage,
            .errorType = main.CompilerErrorType.CompileError,
        }) catch unreachable;
    }

    pub fn deinit(self: *@This()) void {
        self.tokens.*.deinit();
        self.allocator.destroy(self.tokens);
        self.lexer.deinit();
    }
};
