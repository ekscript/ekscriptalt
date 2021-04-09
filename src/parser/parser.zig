// The Parser that produces an AST from source code
// It also does the lexing
const std = @import("std");
const lexer = @import("./lexer.zig");
const main = @import("../main.zig");
const token = @import("./token.zig");
const nodes = @import("./nodes.zig");

const heap = std.heap;
const TT = token.TokenType;
const Token = token.Token;
const Tokens = std.ArrayList(Token);
const Allocator = std.mem.Allocator;

// various nodes
const Expr = nodes.Expr;
const LiteralExpr = nodes.LiteralExpr;
const Literal = nodes.Literal;
const ExprStmt = nodes.ExprStmt;
const TopLevel = nodes.TopLevel;
const Stmt = nodes.Stmt;

fn println(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("\n\x1b[34m parser.zig > \x1b[0m" ++ fmt ++ "\n", args);
}
fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.print("\x1b[34m parser.zig > \x1b[0m" ++ fmt ++ "\n", args);
}

pub const Parser = struct {
    const Self = @This();

    allocator: *Allocator = heap.page_allocator,
    parser_arena: std.heap.ArenaAllocator,

    options: main.CompilerOptions,
    source_code: []const u8,

    errors: *main.Errors,
    warnings: *main.Errors,
    tokens: *Tokens,
    lexer: lexer.Lexer,

    topLevel: TopLevel,

    cursor: usize = 0,

    pub fn init(
        allocator: *Allocator,
        errors: *main.Errors,
        warnings: *main.Errors,
        options: main.CompilerOptions,
    ) Parser {
        var tokens = allocator.create(Tokens) catch unreachable;
        tokens.* = Tokens.init(allocator);
        const source_code = options.sourceCodes.get(options.entry) orelse "";
        var lexInstance = lexer.Lexer.init(
            allocator,
            source_code,
            errors,
            warnings,
            tokens,
        );
        const sourceCodes = options.sourceCodes;
        var parser_arena = std.heap.ArenaAllocator.init(allocator);
        return Parser{
            .allocator = allocator,
            .source_code = source_code,
            .options = options,
            .errors = errors,
            .warnings = warnings,
            .tokens = tokens,
            .lexer = lexInstance,
            .parser_arena = parser_arena,
            .topLevel = TopLevel.init(&parser_arena.allocator),
        };
    }

    pub fn parse(self: *Self) TopLevel {
        self.tokens = self.lexer.lex();
        self.topLevel = self.topLevelStmts();
        return self.topLevel;
    }

    fn topLevelStmts(self: *Self) TopLevel {
        var topLevel = TopLevel.init(self.allocator);
        while (!self.end()) {
            topLevel.append(self.declaration()) catch unreachable;
        }
        return topLevel;
    }

    fn declaration(self: *Self) Stmt {
        return .{ .exprStmt = self.expressionStmt() };
    }

    fn valDeclaration(self: *Self) Stmt {
      if (self.match(&.{})) {}
    }

    fn expressionStmt(self: *Self) *ExprStmt {
        var expr = self.parser_arena.allocator.create(Expr) catch unreachable;
        expr = self.expression();
        _ = self.consume(TT.SemiColon, "Expected ';' after expression");
        return expr;
    }

    fn expression(self: *Self) *Expr {
        return self.primary();
    }

    fn primary(self: *Self) *Expr {
        var expr = self.parser_arena.allocator.create(Expr) catch unreachable;
        if (self.match(&.{TT.FalseLiteral})) {
            expr.* = Expr{
                .literal = LiteralExpr{
                    .literal = Literal{ .boolean = false },
                },
            };
        }
        return expr;
    }

    fn match(self: *Self, types: []const TT) bool {
      var flag = false;
      for (types) |type_| {
        if (self.check(type_)) {
          _ = self.advance();
          return true;
        }
      }
      return false;
    }

    fn advance(self: *Self) Token {
      self.*.cursor += 1;
      return self.*.tokens.items[self.cursor - 1];
    }

    fn consume(self: *Self, tokenType: TT, message: []const u8) Token {
        if (self.check(tokenType)) {
            return self.advance();
        }
        self.addError(message);
        return Token{};
    }

    fn check(self: *Self, tokType: TT) bool {
      if (self.end()) return false;
      return self.lookAhead().tok_type == tokType;
    }

    fn lookAhead(self: *Self) Token {
      return self.tokens.items[self.cursor];
    }

    fn end(self: *Self) bool {
        return self.*.cursor >= self.tokens.items.len;
    }

    fn addError(self: *@This(), message: []const u8) void {
        var line: usize = 1;
        var col: usize = 1;
        const start = self.tokens.items[self.cursor].start;
        const end_ = self.tokens.items[self.cursor].end;
        var i: usize = 0;
        while (i < start) : (i += 1) {
            if (self.source_code[i] == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        const tok = self.source_code[start..end_];

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
            .pos = start,
            .errorMessage = errorMessage,
            .errorType = main.CompilerErrorType.CompileError,
        }) catch unreachable;
    }

    pub fn deinit(self: *@This()) void {
        self.tokens.*.deinit();
        self.allocator.destroy(self.tokens);
        self.lexer.deinit();
        self.parser_arena.deinit();
    }
};
