const std = @import("std");
const Tree = @import("./ast.zig").Tree;
const Parser = @import("./parser/parser.zig").Parser;

// All std imports
const testing = std.testing;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

pub const CompilerBackend = enum { Zig, Js, Dart };

pub const CompilerOptions = struct {
    backend: CompilerBackend = CompilerBackend.Zig,
    entry: []const u8 = "src/index.ek",
    noOutput: bool = false,
};

pub const CompilerErrorType = enum { RuntimeError, CompileError };

pub const Error = struct { line: i32, pos: i32, errorMessage: []const u8, errorType: CompilerErrorType };

pub const SourceCodes = struct { filePath: []u8, fileContent: []u8 };

pub const Compiler = struct {
    // allocator used in the compiler
    allocator: *Allocator,

    // copmiler options
    options: CompilerOptions,

    // list of all errors
    errors: std.ArrayList(Error),

    // list of all warnings
    warnings: std.ArrayList(Error),

    // the ast produced from parser, resolver and optimizer
    tree: Tree = undefined,

    parser: Parser = undefined,

    pub fn init(options: CompilerOptions) callconv(.Inline) Compiler {
        const allocator = std.heap.page_allocator;
        const parser = Parser;
        const compiler = Compiler{
            .allocator = allocator,
            .options = options,
            .errors = std.ArrayList(Error).init(allocator),
            .warnings = std.ArrayList(Error).init(allocator),
        };
        return compiler;
    }

    pub fn parse(self: *Compiler) *Compiler {
        const entry = self.options.entry;
        self.parser = Parser.init(entry, &self.errors, &self.warnings);
        self.tree = self.parser.parse();
        return self;
    }

    pub fn printErrorsWarnings(self: *Compiler) !void {
        self.*.options.entry = "hello";
    }

    pub fn deinit(self: *const Compiler) void {
        self.errors.deinit();
        self.warnings.deinit();
    }
};

test "basic compilation and decompilation" {
    const options = CompilerOptions{};

    var compiler = Compiler.init(options).parse();
    try compiler.printErrorsWarnings();
    defer compiler.deinit();
}
