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
const ArrayList = std.ArrayList;

// various nodes
const Decorator = nodes.Decorator;
const DecoratorStmt = nodes.DecoratorStmt;
const Decorators = nodes.Decorators;
const Expr = nodes.Expr;
const LiteralExpr = nodes.LiteralExpr;
const Literal = nodes.Literal;
const ExprStmt = nodes.ExprStmt;
const DebuggerStmt = nodes.DebuggerStmt;
const TopLevel = nodes.TopLevel;
const Stmt = nodes.Stmt;
const Stmts = nodes.Stmts;

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

    top_level: TopLevel,

    cursor: usize = 0,

    const ParserError = error{SyncError};

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
            .top_level = undefined,
        };
    }

    pub fn parse(self: *Self) TopLevel {
        var top_level_stmts = Stmts.init(&self.parser_arena.allocator);
        self.top_level = TopLevel{
            .statements = top_level_stmts,
            .module_name = self.options.entry,
        };
        self.tokens = self.lexer.lex();
        self.topLevelStmts();
        return self.top_level;
    }

    fn synchronize(self: *Self) void {
        println("synchronizing...", .{});
    }

    fn topLevelStmts(self: *Self) void {
        while (!self.end()) {
            // https://ziglang.org/documentation/master/#if
            if (self.statement()) |stmt| {
                self.top_level.statements.append(stmt) catch unreachable;
            } else |err| {
                self.synchronize();
            }
            _ = self.advance();
        }
    }

    fn statement(self: *Self) ParserError!Stmt {
        const current_tok = self.advance();
        const tok_ahead = self.peek().tok_type;

        switch (current_tok.tok_type) {
            .At => return self.decoratorStatement(),
            .Export => return self.exportStatement(),
            .Import => return self.importStatement(),
            .Debugger => return self.debuggerStatement(),
            .OpenBrace => return self.blockStmt(),
            .If => return self.ifStmt(),
            .Switch => return self.switchStmt(),
            .For => return switch (try self.forStmt()) {
                .for_in => for_stmt.for_in,
                ._for => for_stmt._for,
            },
            .While => return self.whileStmt(),
            .Do => return self.doStmt(),
            .Try => return self.tryStmt(),
            .With => return self.withStmt(),
            .Break => return self.breakStmt(),
            .Continue => return self.continueStmt(),
            .Return => return self.returnStmt(),
            .Throw => return self.throwStmt(),
            .SemiColon => return Stmt.empty_stmt,

            .Async, .Function => self.functionDecl(current_tok),
            .Class => return self.classDecl(current_tok),
            .Abstract => return self.abstractClassDecl(current_tok),
            .Declare => return self.declareDecl(current_tok),
            .TypeAlias => return self.typeAliasDecl(current_tok),
            .Namespace => return self.namespaceDecl(current_tok),
            .Enum => return self.enumDecl(current_tok),
            .Interface => return self.interfaceDecl(current_tok),
            .Const, .Let => return self.lexicalDecl(current_tok),
            .Var => return self.variableDecl(current_tok),
            else => {
                if (current_tok.tok_type == TT.Identifier and tok_ahead == TT.SemiColon) {
                    return self.labeledStmt();
                }
                return self.exprStmt();
            },
        }
    }

    fn ifStmtsFiltered(self: *Self) ParserError!Stmt {
        const current_tok = self.advance();
        const tok_ahead = self.peek().tok_type;

        switch (current_tok.tok_type) {
            .Debugger => return self.debuggerStatement(),
            .OpenBrace => return self.blockStmt(),
            .If => return self.ifStmt(),
            .Switch => return self.switchStmt(),
            .For => return switch (try self.forStmt()) {
                .for_in => for_stmt.for_in,
                ._for => for_stmt._for,
            },
            .While => return self.whileStmt(),
            .Do => return self.doStmt(),
            .Try => return self.tryStmt(),
            .With => return self.withStmt(),
            .Break => return self.breakStmt(),
            .Continue => return self.continueStmt(),
            .Return => return self.returnStmt(),
            .Throw => return self.throwStmt(),
            .SemiColon => return elf.emptyStmt(),
            // .Const, .Let => return self.declaration(current_tok),
            // .Var => return self.declaration(current_tok),
            else => {
                if (current_tok.tok_type == TT.Identifier and self.match(.{TT.SemiColon})) {
                    return self.labeledStmt(current_tok);
                }
                return self.exprStmt();
            },
        }
    }

    fn statements(self: *Self, allocator: *Allocator) ParserError!Stmts {
        var stmts = Stmts.init(allocator);
        const tok = self.peek().tok_type;
        while (tok != TT.Case or tok != TT.Default or tok != TT.CloseBrace) {
            stmts.append(try self.statement());
        }
        return stmts;
    }

    fn debuggerStatement(self: *Self) ParserError!Stmt {
        return Stmt{ .debugger_stmt = self.previous() };
    }

    fn decoratorStatement(self: *Self) ParserError!Stmt {
        var al = &self.parser_arena.allocator;
        var decorators = Decorators.init(al);

        while (!self.match(.{TT.At})) {
            decorators.append(try self.decorator(&decorators));
        }

        var decorator_stmt = al.create(DecoratorStmt) catch unreachable;
        decorator_stmt.decorators = decorators;
        decorator_stmt.stmt = try self.statement();
        return Stmt{ .decorator_stmt = decorator_stmt };
    }

    fn decorator(self: *Self, decorators: *Decorators) ParserError!Decorator {
        const current_tok = self.peek(); // current identifier
        if (self.match(.{TT.Identifier})) {
            if (self.match(.{TT.Dot})) {
                const member_expr = try decoratorMemberExpr(current_tok);
                if (self.match(.{TT.OpenParen})) {
                    const arguments = try self.arguments();
                    return Decorator{
                        .call_expr = DecoratorCallExpr{
                            .function = .{ .member_expr = member_expr },
                            .arguments = arguments,
                        },
                    };
                } else {
                    return Decorator{ .member_expr = member_expr };
                }
            } else if (self.match(.{TT.OpenParen})) {
                const arguments = try self.arguments();
                return Decorator{
                    .call_expr = DecoratorCallExpr{
                        .function = .{ .identifier = current_tok },
                        .arguments = arguments,
                    },
                };
            } else {
                return Decorator{ .identifier = current_tok };
            }
        }
        return ParserError{SyncError};
    }

    fn decoratorMemberExpr(self: *Self, identifier: Identifier) ParserError!DecoratorMemberExpr {
        if (self.match(.{TT.Identifier})) {
            var current_tok = self.previous();
            var mem_expr = DecoratorMemberExpr{
                .object = .{ .identifier = Identifier },
                .property = current_tok,
            };
            while (self.match(.{TT.Dot})) {
                var next_mem_expr = try self.decoratorMemberExpr(self.peek());
                mem_expr.object = .{ .member_expr = next_mem_expr };
            }
            return mem_expr;
        }
        return ParserError{SyncError};
    }

    fn exportStatement(self: *Self) ParserError!Stmt {
        const allocator = &self.parser_arena.allocator;
        var export_stmt = allocator.create(ExportStmt);

        const StringErrorMessage = "Filename missing. Needs a string to a file path";

        if (self.match(.{TT.Star})) {
            // 'export' '*' 'from' String
            _ = try self.consume(TT.From, "'from' missing");
            var str = try self.consume(TT.String, StringErrorMessage);
            try self.semicolon();
            export_stmt.* = try str;
        } else if (self.match(.{TT.Default})) {
            // 'export' 'default' Expr
            var export_default = ExportDefaultEnum{ .expr = try self.expr() };
            if (self.match(.{TT.Async})) {
                export_default = ExportDefaultEnum{ .func_sign = try self.funcSign() };
            }
            try self.semicolon();
            export_stmt.* = ExportStmt{ .export_default = export_default };
        } else if (self.match(.{TT.OpenBrace})) {
            // 'export' ExportClause ('from' String)?
            var export_clause = try self.exportClause();
            var file: ?Token = null;
            if (self.match(.{TT.From})) {
                file = try self.consume(TT.String, StringErrorMessage);
            }
            try self.semicolon();
            export_stmt.* = ExportStmt{ .export_from = .{ .export_clause = decl, .file = file } };
        } else if (self.match(.{TT.Equal})) {
            // export '=' Identifier
            const identifier = try self.consume(TT.Identifier, "Identifier expected...");
            try self.semicolon();
            export_stmt.* = ExportStmt{ .identifier = identifier };
        } else if (self.match(.{TT.As})) {
            _ = try self.consume(TT.Namespace, "'namespace' expected!");
            var identifier = try self.consume(TT.Identifier, "Identifier expected");
            try self.semicolon();
            export_stmt.* = ExportStmt{ .export_namespace = identifier };
        } else {
            export_stmt = ExportStmt{ .export_declaration = try self.declaration(self.advance()) };
        }

        return Stmt{ .export_stmt = export_stmt };
    }

    fn importExportClause(self: *Self) ParserError!ExportClause {
        var clause = ExportClause.init(&self.parser_arena.allocator);

        // Implementation of do-while
        do: while (true) : (if (!self.match(.{TT.Comma})) break :do) {
            if (self.match(.{TT.CloseBrace})) break;
            var import_export_specifier = ImportExportSpecifier{
                .star = null,
                .name = try self.consume(TT.Identifier, "Expected an identifier for the export name"),
                .alias = null,
            };
            if (self.match(.{TT.As})) {
                import_export_specifier.alias = try self.consume(TT.Identifier, "Expected an identifier for alias");
            }
            clause.append(import_specifier) catch unreachable;
        }
        return clause;
    }

    fn importStatement(self: *Self) ParserError!Stmt {
        const allocator = &self.parser_arena.allocator;
        var import_stmt = allocator.create(ImportStmt);

        var _type = self.match(.{ TT.Type, TT.Typeof });
        var clause = ImportClause.init(&self.parser_arena.allocator);

        if (self.match(.{TT.Star})) {
            var star = self.previous();
            _ = try self.consume(TT.As, "'as' expected.");
            var alias = try self.consume(TT.Identifier, "Import Alias not found.");
            clause.append(ImportExportSpecifier{ .star = star, .alias = alias });
            _ = try self.consume(TT.From, "'from' expected");
        } else if (self.match(.{TT.String})) {
            // 'import' String;
            try self.semicolon();
            import_stmt.* = ImportStmt{ ._type = _type, .file = self.previous() };
        } else if (self.match(.{TT.Identifier})) {
            var import_name = self.previous();
            if (self.match(.{TT.Equals})) {
                _ = try self.consume(TT.OpenParen, "'(' expected");
                _ = try self.consume(TT.Require, "'require' expected");
                var file = try self.consume(TT.String, "File name not provided");
                _ = try self.consume(TT.CloseParen, "')' expected");
                try self.semicolon();
                import_stmt.* = ImportStmt{ ._type = _type, .require_name = import_name, .file = file };
                return Stmt{ .import_stmt = import_stmt };
            }

            _ = try self.consume(TT.From, "'from' expected");
            if (self.match(.{TT.Comma})) {
                _ = try self.consume(TT.OpenBrace, "'{' expected");
                clause = try self.importExportClause();
            }
            clause.append(ImportExportSpecifier{ .name = import_name });
            _ = try self.consume(TT.From, "'from' expected");
        } else if (self.match(.{TT.OpenBrace})) {
            clause = try self.importExportClause();
            if (self.match(.{TT.Comma})) {
                var identifier = try self.consume(TT.Identifier, "Default import missing.");
                clause.append(ImportExportSpecifier{ .name = identifier });
            }
            _ = try self.consume(TT.From, "'from' expected");
        }

        var file = try self.consume(TT.String);
        try self.semicolon();

        import_stmt.* = ImportStmt{ ._type = type, .import_specifier = clause, .file = file };
        return Stmt{ .import_stmt = import_stmt };
    }

    fn blockStmt(self: *Self) ParserError!BlockStmt {
        const allocator = &self.parser_arena.allocator;
        var block_stmt = allocator.create(BlockStmt);
        block_stmt.* = BlockStmt.init(allocator);

        while (!self.match(.{TT.CloseBrace})) {
            var stmt = try self.statement();
            block_stmt.append(stmt) catch unreachable;
        }
        return block_stmt;
    }

    fn ifStmt(self: *Self) ParserError!IfStmt {
        var condition = try self.parenExpr();
        var consequence = try self.statement();
        var if_stmt = ifStmt{ .condition = condition, .consequence = consequence };
        if (self.match(.{TT.Else})) {
            if_stmt.alternative = try self.statement();
        }
        return if_stmt;
    }

    fn switchStmt(self: *Self) ParserError!SwitchStmt {
        var value = try self.parenExpr();
        _ = try self.consume(TT.OpenBrace, "Expected '{'");
        var switch_body = try self.switchBody();
        var switch_stmt = SwitchStmt{ .value = value, .switch_body = switch_body };
        return switch_stmt;
    }

    fn switchBody(self: *Self) ParserError!*SwitchBody {
        var allocator = &parser.parser_arena.allocator;
        var switch_body = SwitchBody.init(allocator);

        do: while (true) : (if (self.match(.{TT.CloseBrace})) break :do) {
            if (self.match(.{TT.Case})) {
                var value = try self.seqExpr();
                _ = try self.consume(TT.Colon, "':' expected in case statement.");
                var stmts = try self.statements();

                var switch_case = allocator.create(SwitchCase);
                switch_case.* = SwitchCase{ .value = value, .stmts = stmts };
                switchBody.append(SwitchArm{ .switch_case = switch_case });
            } else if (self.match(.{TT.Default})) {
                _ = try self.consume(TT.Colon, "':' expected in case statement.");
                var stmts = try self.statements(allocator);
                var switch_default = allocator.create(SwitchCase);
                switch_default.* = SwitchCase{ .value = value, .stmts = stmts };
                switch_body.append(SwitchArm{ .switch_default = switch_default });
            }
        }

        return switch_body;
    }

    const ForEnum = union(enum) {
        for_stmt: ForStmt,
        for_in_stmt: ForInStmt,
    };

    fn forInStmt(self: *Self, is_await: bool) ParserError!?ForInStmt {
        var for_header = try self.forHeader();
        if (for_header == null) return null;

        var body = self.statement() catch return null;
        return ForInStmt{ .is_await = is_await, .for_header = for_header, .body = body };
    }

    fn forStmt(self: *Self) ParserError!?ForEnum {
        const curr_cursor = self.cursor;
        if (self.match(.{TT.Await})) return ForEnum{
            .for_in_stmt = try self.forInStmt(true) orelse return null,
        };

        var for_in_stmt = try self.forInStmt();
        if (for_in_stmt == null) {
            self.*.cursor = curr_cursor;
        }

        _ = try self.consume(TT.OpenParen, "'(' expected");
        const cursor = self.cursor;

        var initialier: ForInitializer = undefined;
        if (self.match(.{TT.SemiColon})) {
            initializer = ForInitializer.empty_stmt;
        } else if (self.match(.{ TT.Let, TT.Const, TT.Var })) {
            var lexical_decl = try self.lexicalDecl() orelse return null;
            initializer = ForInitializer{ .lexical_decl = lexical_decl };
        } else {
            var expr_stmt = (try self.exprStmt()) orelse return null;
            initializer = ForInitializer{ .expr_stmt = expr_stmt };
        }

        _ = self.consume(TT.Semicolon, "';' is expected.");

        var condition: ForCondition = undefined;
        if (self.match(.{TT.Semicolon})) {
            condition = ForCondition.EmptyStmt;
        }

        var expr_stmt: ExprStmt = try self.expStmt() orelse return null;
        condition = ForCondition{ .expr_stmt = expr_stmt };

        var increment: ?Expressions = undefined;
        if (self.lookAhead().tok_type == TT.CloseParen) {
            increment = null;
        } else {
            increment = try self.seqExpr() orelse return null;
            _ = self.consume(TT.CloseParen, "')' expected");
        }

        var body = try self.statement() orelse return null;

        var for_stmt: ForStmt = ForStmt{
            .initializer = initialier,
            .condition = condition,
            .increment = increment,
            .body = body,
        };

        return ForEnum{ .for_stmt = for_stmt };
    }

    // If null, consider for IfStmt. If Error. Its an Error
    fn forHeader(self: *Self) ParserError!?ForHeader {
        _ = try self.consume(TT.OpenParen, "'(' expected");

        var varLetConst: Token = undefined;

        if (self.match(.{ TT.Var, TT.Let, TT.Const })) {
            varLetConst = self.previous();
        } else return null;

        var left = undefined;
        if (self.match(.{TT.Identifier})) {
            left = .{ .identifier = self.previous() };
        } else if (self.match(.{ TT.OpenBracket, TT.OpenParen })) {
            var destructing_pattern = self.destructuringPattern() catch return null;
            left = .{ .destructing_pattern = destructing_pattern };
        } else return null;

        var isIn = false;
        if (self.match(.{TT.In})) {
            isIn = true;
        } else if (self.match(.{TT.Of})) {
            isIn = false;
        } else return null;

        var right = try self.seqExpr();
        _ = self.consume(TT.CloseParen, "')' expected.");
        return ForHeader{ .varLetConst = varLetConst, .isIn = isIn, .right = right, .left = left };
    }

    fn whileStmt(self: *Self) ParserError!Stmt {
        var condition = try self.parenExpr();
        var body = try self.ifStmtsFiltered();
        return WhileStmt{
            .condition = condition,
            .body = body,
        };
    }

    fn doStmt(self: *Self) ParserError!Stmt {
        var body = try self.ifStmtsFiltered();
        _ = try consume(TT.While, "'while' expected");
        var condition = try self.parenExpr();
        return DoStmt{ .body = body, .condition = condition };
    }

    fn tryStmt(self: *Self) ParserError!Stmt {
        var body = try self.blockStmt();
        var handler: ?CatchClause = null;
        var finalizer: ?FinallyClause = null;
        if (self.match(.{TT.Catch})) {
            handler = try self.catchClause();
        }
        if (self.match(.{TT.Finally})) {
            finalizer = try self.finallyClause();
        }
        return TryStmt{ .body = body, .handler = handler, .finalizer = finalizer };
    }

    fn catchClause(self: *Self) ParserError!Stmt {
        if (self.match(.{TT.OpenParen})) {
            var parameter: IdentifierDestructuring = undefined;
            if (self.match(.{TT.Identifier})) {
                parameter = IdentifierDestructuring{ .identifier = self.previous() };
            } else if (self.match(.{TT.OpenBrace})) {
                parameter = IdentifierDestructuring{
                    .identifier = try self.destructuringPattern(),
                };
            }
            _ = self.consume(TT.CloseParen, "')' expected");
            var body = try self.blockStmt();
            return CatchClause{ .parameter = parameter, .body = body };
        }
        return ParserError{};
    }

    fn finallyClause(self: *Self) ParserError!Stmt {
        return finallyClause{ .finally = try self.blockStmt() };
    }

    fn withStmt(self: *Self) ParserError!WithStmt {
        var object = try self.parenExpr();
        var body = try self.statement();
        return WithStmt{ .object = object, .body = body };
    }

    fn breakStmt(self: *Self) ParserError!BreakStmt {
        var identifier: Token = null;
        if (self.match(.{TT.Identifier})) {
            identifier = self.previous();
        }
        _ = try self.semicolon();
        return identifier;
    }

    fn continueStmt(self: *Self) ParserError!ContinueStmt {
        var identifier: Token = null;
        if (self.match(.{TT.Identifier})) {
            identifier = self.previous();
        }
        _ = try self.semicolon();
        return identifier;
    }

    fn returnStmt(self: *Self) ParserError!ReturnStmt {
        var expr: ?SeqExpr = null;
        if (self.peek() == TT.Semicolon) {
            expr = try self.seqExpr();
        }
        _ = try self.semicolon();
        return expr;
    }

    fn throwStmt(self: *Self) ParserError!ThrowStmt {
        var expr = try self.seqExpr();
        _ = try self.semicolon();
        return expr;
    }

    fn labeledStmt(self: *Self, label: Token) ParserError!LabeledStmt {
        var stmt = try self.statement();
        return LabeledStmt{ .label = label, .stmt = stmt };
    }

    fn declaration(self: *Self) ParserError!?Declaration {
        var current_tok = self.advance();
        switch (current_tok.tok_type) {
            .Async, .Function => return try self.functionDecl(current_tok),
            .Class => return try self.classDecl(),
            .AbstractClassDecl => return try self.abstractClassDecl(),
            .Type => return try self.typeAliasDecl(),
            .Enum => return try self.enumDecl(false),
            .Interface => return try self.interfaceDecl(),
            .Const, .Let => {
                var tok: Token = self.previous();
                if (self.match(.{TT.Enum})) {
                    return try self.enumDecl(tok.tok_type == TT.Const);
                }
                return try self.lexicalDecl(current_tok.tok_type == TT.Const);
            },
            .Var => return try self.variableDecl(),
            .Module => return try self.moduleDecl(),
            .Namespace => return try self.internalModule(),
            .Declare => return self.ambientDecl(),
            else => {
                return null;
            },
        }
    }

    fn functionDecl(self: *Self, current_tok: Token) ParserError!FuncDecl {
        var is_async: bool = false;
        if (current_tok.tok_type == TT.Async) {
            is_async = true;
            _ = try self.consume(TT.Function, "'function' expected");
        }
        var is_generator: bool = false;
        if (current_tok.tok_type == TT.Star) {
            is_generator = true;
        }

        var name = try self.consume(TT.Identifier, "function name expected");
        var call_sign = try self.callSign();
        var body = null;

        if (self.match(.{TT.OpenBrace})) {
            var body = try self.blockStmt();
        }

        self.semicolon() catch {};
        return FuncDecl{ .is_async = is_async, .is_generator = is_generator, .name = name, .call_sign = call_sign, .body = body };
    }

    fn callSign(self: *Self) ParserError!CallSign {
        var type_params = null;
        if (self.match(.{TT.Less})) try self.typeParams();

        _ = self.consume(TT.OpenParen, "'(' expected");
        var params = try self.formalParams();
        var return_type = null;
        if (self.match(.{TT.Colon})) {
            if (self.match(.{TT.Asserts}).tok_type) {
                return_type = .{ .asserts = try self.asserts() };
            } else {
                var peeked: TokenType = self.peek();
                var ahead: TokenType = self.lookAhead();
                if ((peeked.tok_type == TT.Identifier or peeked.tok_type == TT.This) and
                    ahead.tok_type == TT.Is)
                {
                    var type_predicate: TypePredicate = try self.typePredicate(peeked);
                    return_type = .{ .type_predicate = type_predicate };
                } else {
                    return_type = .{ .type_annotation = try self.typeAnnotation() };
                }
            }
        }
        return CallSign{
            .type_params = type_params,
            .params = params,
            .return_type = return_type,
        };
    }

    fn asserts(self: *Self) ParserError!Assert {
        if (self.match(.{TT.Identifier})) {
            var identifier: Identifier = self.previous();
            if (self.match(.{TT.Is})) {
                return Asserts{ .type_predicate = try self.typePredicate(identifier) };
            }
            return Asserts{ .identifier = identifier };
        } else if (self.match(.{TT.This})) {
            var this: Token = self.previous();
            if (self.match(.{TT.Is})) {
                return Asserts{ .type_predicate = try self.typePredicate(this) };
            }
            return Asserts{ .this = this };
        }
        return ParserError{SyncError};
    }

    fn typeParams(self: *Self) ParserError!TypeParams {
        var type_params = ArrayList(TypeParam).init(&self.parser_arena.allocator);

        do: while (true) : (if (!self.match(.{TT.Comma})) break :do) {
            type_params.append(try self.typeParam());
        }

        _ = self.consume(TT.Greater, "'>' expected.");
        return type_params;
    }

    fn typePredicate(self: *Self, identifierOrThis: Token) ParserError!TypePredicate {
        return TypePredicate{ .identifierOrThis = identifierOrThis, ._type = try self._type() };
    }

    fn classDecl(self: *Self) ParserError!Stmt {
        var name: TypeIdentifier = try self.consume(TT.Identifier, "Identifier expected.");

        var type_params: ?TypeParams = null;
        if (self.match(.{TT.Less})) type_params = try self.typeParams();

        var class_heritage: ?ClassHeritage = null;

        if (self.match(.{TT.Extends})) {
            var extends: ArrayList(ExtendsClause) = try self.extendsClause();
            var implements: ArrayList(Type) = null;
            if (self.match(.{TT.Implements})) {
                implements = try self.implementsClause();
            }
            class_heritage = ClassHeritage{ .extends = extends, .implements = implements };
        } else if (self.match(.{TT.Implements})) {
            class_heritage = ClassHeritage{
                .extends = null,
                .implements = try self.implementsClause(),
            };
        }

        var body: ClassBody = try self.classBody();

        return ClassDecl{ .name = name, .type_params = type_params, .class_heritage = class_heritage, .body = body };
    }

    fn abstractClassDecl(self: *Self) ParserError!AbstractClassDecl {
        _ = try self.consume(TT.Class, "'class' expected");
        var name: TypeIdentifier = try self.consume(TT.Identifier, "Identifier expected");

        var type_params: ?TypeParams = null;
        if (self.match(.{TT.Less})) type_params = try self.typeParams();

        var class_heritage: ?ClassHeritage = null;

        if (self.match(.{TT.Extends})) {
            var extends: ArrayList(ExtendsClause) = try self.extendsClause();
            var implements: ArrayList(Type) = null;
            if (self.match(.{TT.Implements})) {
                implements = try self.implementsClause();
            }
            class_heritage = ClassHeritage{ .extends = extends, .implements = implements };
        } else if (self.match(.{TT.Implements})) {
            class_heritage = ClassHeritage{
                .extends = null,
                .implements = try self.implementsClause(),
            };
        }

        var body: ClassBody = try self.classBody();
        return AbstractClassDecl{
            .name = name,
            .type_params = type_params,
            .class_heritage = class_heritage,
            .body = body,
        };
    }

    fn enumDecl(self: *Self, is_const: bool) ParserError!EnumDecl {
        var name = try self.consume(TT.Identifier, "identifier expected");
        _ = try self.consume(TT.OpenBrace, "'{' expected");

        var body = ArrayList(EnumField).init(&self.parser_arena.allocator);

        do: while (true) : (if (self.peek() == TT.CloseBrace) break :do) {
            var prop_name = try self.consume(TT.Identifier, "Identifier expected");
            var initializer: ?Initializer = null;
            if (self.match(.{TT.Equals})) {
                initializer = try self.expr();
            }
            if (self.peek() != TT.CloseBrace) {
                _ = try self.consume(TT.Comma, "',' expected");
            }
            body.append(EnumField{ .prop_name = prop_name, .initializer = initializer });
        }

        _ = try self.consume(TT.CloseBrace, "'}' expected");
        return EnumDecl{ .is_const = is_const, .name = name, .body = body };
    }

    fn interfaceDecl(self: *Self) ParserError!InterfaceDecl {
        var name = try self.consume(TT.Identifier, "Identifier expected");
        var type_params: ?TypeParams = null;
        if (self.match(.{TT.Less})) type_params = try self.typeParams();

        var extends: ?ArrayList(ExtendsType) = null;
        if (self.match(.{TT.Extends})) {
            extends = try self.extendsClause();
        }

        _ = try self.consume(TT.OpenBrace, "'{' expected");
        var body = try self.objectType();
        return InterfaceDecl{
            .name = name,
            .type_params = type_params,
            .extends = extends,
            .body = body,
        };
    }

    fn lexicalDecl(self: *Self, is_const: bool) ParserError!LexicalStmt {
        var variable_decls = ArrayList(VarDeclarator).init(&self.parser_arena.allocator);

        do: while (true) : (if (!self.match(.{tt.comma})) break :do) {
            var name: variablename = undefined;
            if (self.match(.{tt.identifier})) {
                name = varname{ .identifier = self.previous() };
            } else if (self.match(.{tt.openbrace})) {
                name = varname{ .destructing_pattern = self.destructuringpattern() };
            } else {
                return parsererror{syncerror};
            }

            var _type: ?typeannotation = null;
            if (self.match(.{tt.colon})) _type = try self.typeannotation();
            
            var initializer: ?initializer = null;
            if (self.match(.{tt.equals})) initializer = try self.expr();

            variable_decls.append(vardeclarator { .name = name, ._type = _type, .initializer = initializer, });
        }
        try self.semicolon();

        return LexicalDecl{ .is_const = .is_const, .variable_decls = variable_decls };
    }

    fn variableDecl(self: *Self) ParserError!Stmt {
        var variable_decls = ArrayList(VarDeclarator).init(&self.parser_arena.allocator);

        do: while (true) : (if (!self.match(.{tt.comma})) break :do) {
            var name: variablename = undefined;
            if (self.match(.{tt.identifier})) {
                name = varname{ .identifier = self.previous() };
            } else if (self.match(.{tt.openbrace})) {
                name = varname{ .destructing_pattern = self.destructuringpattern() };
            } else {
                return parsererror{syncerror};
            }

            var _type: ?typeannotation = null;
            if (self.match(.{tt.colon})) _type = try self.typeannotation();
            
            var initializer: ?initializer = null;
            if (self.match(.{tt.equals})) initializer = try self.expr();

            variable_decls.append(vardeclarator { .name = name, ._type = _type, .initializer = initializer, });
        }

        try self.semicolon();

        return variable_decls;
    }

    fn moduleDecl(self: *Self) ParserError!Module {
        var name: ModuleName = undefined;
        if(self.match(.{TT.String})) {
            name = ModuleName {.string = self.previous()};
        } else if (self.match(.{TT.Identifier})) {
            if (self.peek(.{TT.Dot})) {
                name = ModuleName{
                    .nested_identifier = try self.nestedIdentifier(self.previous()),
                };
            } else {
                name = ModuleName {.identifier = self.previous()};
            }
        } else {
            return ParserError{SyncError};
        }
        var body = try self.blockStmt();
        return Module { .name = name, .body = body };
    }

    fn internalModule(self: *Self) ParserError!InternalModule {
        return try self.moduleDecl();
    };

    fn ambientDecl(self: *Self) ParserError!AmbientDecl {
        var tok_type = self.peek().tok_type;
        var ahead_tok_type = self.lookAhead().tok_type;
        if (tok_type == TT.Global) {
            _ = self.advance();
            return AmbientDecl{ .stmt_block = try self.blockStmt() };
        } else if (tok_type == TT.Module && ahead_tok_type == TT.Dot) {
            self.*.cursor += 2;
            var prop_identifier: PropIdentifier = try self.consume(TT.Identifier, "identifier expected.");
            _ =  try self.consume(TT.Colon, "':' expected");
            var _type = try self._type();
            try self.semicolon();
            return AmbientDecl {
                .module = .{ .prop_identifier = prop_identifier, ._type = _type },
            };
        } else {
            return AmbientDecl{.declaration = try self.declaration()};
        }
    }

    fn expressionStmt(self: *Self) *ExprStmt {
        var expr = self.parser_arena.allocator.create(Expr) catch unreachable;
        expr = self.expr();
        _ = try self.consume(TT.SemiColon, "Expected ';' after expression");
        return expr;
    }

    fn expr(self: *Self) ParserError!Expr {
        return self.primary();
    }

    fn lhsExpr(self: *Self) ParserError!LhsExpr {}

    fn identifierWithReserved(self: *Self) ParserError!IdentifierWithReserved {
        const tok = self.peek().tok_type;
        return switch (tok) {
            .Identifier,
            .Get,
            .Set,
            .Async,
            .Static,
            .Export,
            .Declare,
            .Namespace,
            .Type,
            .Public,
            .Private,
            .Protected,
            .Readonly,
            .Module,
            .Any,
            .Number,
            .Boolean,
            .String,
            .Symbol,
            .Export,
            => self.advanced(),
            else => ParserError{SyncError},
        };
    }

    fn propertyName(self: *Self) ParserError!?PropName {
        switch (self.peek().tok_type) {
            .String => return PropName{ .string = self.advance() },
            .Int => return PropName{ .int = self.advance() },
            .Float => return PropName{ .float = self.advance() },
            .Number => return PropName{ .float = self.advance() },
            .Identifier => return PropName{ .identifier = try self.identifierWithReserved() },
            .OpenBracket => {
                _ = self.consume(TT.OpenBracket, "");
                var expr = try self.expr();
                _ = self.consume(TT.CloseBracket, "']' expected.");
                return PropName{ .computed_prop_name = expr };
            },
            else => return null,
        }
    }

    fn pairPattern(self: *Self) ParserError!?PairPattern {
        var key = try self.propertyName();
        if (key == null) return null;
        _ = self.consume(TT.Colon, "':' expected") catch return null;
        return PairPattern{ .key = key, .pattern = try self.pattern() };
    }

    /// TODO: Don't start with OpenBrace
    fn destructuringPattern(self: *Self, bracket_type: TokenType) ParserError!DestructuringPattern {
        var allocator = &self.parser_arena.allocator;

        if (bracket_type == TT.OpenBrace) {
            var obj_pattern = ArrayList(ObjectPattern).init(allocator);

            do: while (true) : (if (!self.match(.{TT.CloseBrace})) break :do) {
                if (self.match(.{TT.Identifier})) {
                    var identifier = self.previous();
                    if (self.match(.{TT.Colon})) {
                        var pattern = try self.pattern();
                        obj_pattern.append(PairPattern{ .key = identifier });
                    }
                } else {}
            }

            return DestructuringPattern{ .obj_pattern = obj_pattern };
        } else {}
    }

    fn memberExpr(self: *Self) ParserError!MemberExpr {
        var object = try self.expr();
        var null_check_access = false;
        var property = null;
        if (self.match(.{TT.Question})) {
            _ = try self.consume(TT.Dot, "Expected '.'");
            null_check_access = true;
            property = try self.consume(TT.Identifier, "Expected identifier");
        } else if (self.match(.{TT.Dot})) {
            property = try self.consume(TT.Identifier, "Expected identifier");
        }
        return MemberExpr{ .object = object, .null_check_access = null_check_access, .property = property };
    }

    fn subscriptExpr(self: *Self) ParserError!SubscriptExpr {
        var object = try self.expr();
        var nullable_access = false;
        if (self.match(.{TT.Question})) {
            _ = try self.consume(TT.Question, "'.' expected here");
            nullable_access = true;
        }
        var index = try self.seqExpr();
        return SubscriptExpr{ .object = object, .nullable_access = nullable_access, .index = index };
    }

    fn seqExpr(self: *Self) ParserError!SeqExpr {
        var left = try self.expr();
        if (self.match(.{TT.Comma})) {
            var right = self.parser_arena.allocator.create(SeqExpr);
            right.* = try self.seqExpr();
            return SeqExpr{ .left = left, .right = right };
        }
        return SeqExpr{ .left = expr };
    }

    fn parenExpr(self: *Self) ParserError!ParenExpr {
        var seq_expr = try self.seqExpr();
        if (seq_expr.right != null) {
            // single_expr
            _ = try self.consume(TT.CloseParen, "')' expected.");
            return ParenExpr{ .seq_exp = seq_expr };
        } else {
            var paren_expr = ParenExpr{ .single_expr = seq_expr.expr };
            if (self.match(.{TT.Colon})) {
                var type_annotation = try self.typeAnnotation();
                parent_expr.single_expr.type_annotation = type_annotation;
            }
            return paren_expr;
        }
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

    fn semicolon(self: *Self) ParserError!void {
        if (!self.match(.{ TT.Semicolon, TT.Newline, TT.EOF })) {
            return ParserError{SyncError};
        }
    }

    fn advance(self: *Self) Token {
        self.*.cursor += 1;
        return self.*.tokens.items[self.cursor - 1];
    }

    fn previous(self: *Self) Token {
        return self.tokens.items[self.cursor - 1];
    }

    fn consume(self: *Self, tokenType: TT, message: []const u8) ParserError!Token {
        if (self.check(tokenType)) {
            return self.advance();
        }
        self.addError(message);
        return ParserError{SyncError};
    }

    fn check(self: *Self, tokType: TT) bool {
        if (self.end()) return false;
        return self.peek().tok_type == tokType;
    }

    fn peek(self: *Self) Token {
        if (self.end()) {
            return Token;
        }
        return self.tokens.items[self.cursor];
    }

    fn lookAhead(self: *Self) Token {
        const cursor = self.cursor;
        if (cursor + 1 < self.tokens.items.len) {
            if (self.end()) return Token{};
            return self.tokens.items[cursor + 1];
        }
        return Token{};
    }

    fn end(self: *Self) bool {
        return self.peek().tok_type == TT.EOF;
    }

    fn addError(self: *@This(), message: []const u8) ParserError!void {
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
        return ParserError{SyncError};
    }

    pub fn deinit(self: *@This()) void {
        self.tokens.*.deinit();
        self.allocator.destroy(self.tokens);
        self.lexer.deinit();
        self.parser_arena.deinit();
        // self.allocator.destroy(self.parser_arena);
    }
};
