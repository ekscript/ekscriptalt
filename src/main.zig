const std = @import("std");
const Tree = @import("./ast.zig").Tree;
const Parser = @import("./parser/parser.zig").Parser;

// All std imports
const testing = std.testing;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

pub const CompilerBackend = enum { Zig, Js, Dart };

pub const SourceCodes = struct { filePath: []u8, fileContent: []u8 };

pub const CompilerOptions = struct {
    backend: CompilerBackend = CompilerBackend.Zig,
    entry: []const u8 = "src/index.ek",
    noOutput: bool = false,
    allocator: *Allocator = std.heap.page_allocator,
    sourceCodes: ?*std.ArrayList([]u8) = null,
};

pub const CompilerErrorType = enum { RuntimeError, CompileError };

pub const Error = struct {
    line: i32,
    pos: i32,
    errorMessage: []const u8,
    errorType: CompilerErrorType,
};

pub const Errors = std.ArrayList(Error);

pub const Compiler = struct {
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

    pub fn init(options: CompilerOptions) Compiler {
        var allocator = options.allocator;

        var errors = Errors.init(allocator);
        var warnings = Errors.init(allocator);

        const parser = Parser.init(allocator, options.entry, &errors, &warnings);

        const compiler = Compiler {
            .allocator = allocator,
            .options = options,
            .parser = parser,
            .errors = errors,
            .warnings = warnings,
        };
        return compiler;
    }

    pub fn parse(self: *Compiler) !*Compiler {
        const entry = self.options.entry;
        self.tree = try self.parser.parse();
        return self;
    }

    /// print errors and warnings after formatting them
    /// with [std.debug.TTY](https://ziglang.org/documentation/master/std/#std;debug.TTY.Color)
    pub fn printErrorsWarnings(self: *Compiler) void {
        std.debug.print("Num Errors: {d}, Warnings: {d}---\n\n", .{
            self.errors.items.len,
            self.warnings.items.len,
        });
    }

    /// destructor
    pub fn deinit(self: *Compiler) void {
      self.errors.deinit();
      self.warnings.deinit();
      self.parser.deinit();
    }
};

test "basic compilation and decompilation" {
    const options = CompilerOptions{ .allocator = std.testing.allocator };

    var compiler = try Compiler.init(options).parse();

    compiler.printErrorsWarnings();

    compiler.deinit();
}

