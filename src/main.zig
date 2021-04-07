const std = @import("std");
const Tree = @import("./ast.zig").Tree;
const Parser = @import("./parser/parser.zig").Parser;

// All std imports
const Allocator = std.mem.Allocator;
const test_allocator = std.testing.allocator;
const print = std.debug.print;

pub const CompilerBackend = enum { Zig, Js, Dart };

pub const SourceCodes = std.StringHashMap([]const u8);

pub const CompilerOptions = struct {
    backend: CompilerBackend = CompilerBackend.Zig,
    entry: []const u8 = "src/main.ek",
    noOutput: bool = false,
    allocator: *Allocator = std.heap.page_allocator,
    sourceCodes: SourceCodes = SourceCodes.init(std.heap.page_allocator),
};

pub const CompilerErrorType = enum { RuntimeError, CompileError };

pub const Error = struct {
    line: usize,
    pos: usize,
    errorMessage: []const u8,
    errorType: CompilerErrorType,
};

pub const Errors = std.ArrayList(Error);

pub const Compiler = struct {
    const Self = @This();

    // allocator used in the compiler
    allocator: *Allocator,

    // compiler options
    options: CompilerOptions,

    // list of all errors
    errors: std.ArrayList(Error),

    // list of all warnings
    warnings: std.ArrayList(Error),

    // the ast produced from parser, resolver and optimizer
    tree: Tree = undefined,

    parser: Parser = undefined,

    pub fn init(options: CompilerOptions) Self {
        var allocator = options.allocator;

        var errors = Errors.init(allocator);
        var warnings = Errors.init(allocator);

        const parser = Parser.init(
            allocator,
            &errors,
            &warnings,
            options,
        );

        const compiler = Self{
            .allocator = allocator,
            .options = options,
            .parser = parser,
            .errors = errors,
            .warnings = warnings,
        };
        return compiler;
    }

    pub fn parse(self: *Self) !*Self {
        const entry = self.options.entry;
        self.tree = try self.parser.parse();
        return self;
    }

    /// print errors and warnings after formatting them
    /// with [std.debug.TTY](https://ziglang.org/documentation/master/std/#std;debug.TTY.Color)
    pub fn printErrorsWarnings(self: *Compiler) void {
    }

    /// destructor
    pub fn deinit(self: *Compiler) void {
        self.errors.deinit();
        self.warnings.deinit();
        self.parser.deinit();
    }
};

test "basic compilation and decompilation" {
    var sourceCodes = SourceCodes.init(test_allocator);
    defer sourceCodes.deinit();
    try sourceCodes.put("src/main.ek", "1 + 1;");

    const options = CompilerOptions{
        .allocator = test_allocator,
        .sourceCodes = sourceCodes,
    };

    var compiler = try Compiler.init(options).parse();

    compiler.printErrorsWarnings();
    compiler.deinit();
}
