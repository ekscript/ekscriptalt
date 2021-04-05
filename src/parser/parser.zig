// The Parser that produces an AST from source code
// It also does the lexing
const std = @import("std");
const lexer = @import("./lexer.zig");
const main = @import("../main.zig");
const ast = @import("../ast.zig");

const Errors = std.ArrayList(main.Error);
const Warnings = std.ArrayList(main.Error);

pub const Parser = struct {
    source_code: []const u8,
    errors: *Errors,
    warnings: *Errors,

    pub fn init(source_code: []const u8, errors: *Errors, warnings: *Errors) callconv(.Inline) Parser {
        return Parser{
            .source_code = source_code,
            .errors = errors,
            .warnings = warnings,
        };
    }

    pub fn parse(self: *Parser) ast.Tree {
        var lxr = lexer.Lexer.init(self.source_code);
        return ast.Tree{};
    }
};
