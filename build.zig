const std = @import("std");
const Files = std.ArrayList([]const u8);

pub fn build(b: *std.build.Builder) !void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("ekscriptalt", "src/main.zig");
    lib.setBuildMode(mode);
    lib.install();

    const test_step = b.step("test", "Run library tests");

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    var files = Files.init(allocator);
    try files.append("main.zig");

    for (files.items) |file| {
        const filePath = try std.fmt.allocPrint(allocator, "src/{s}", .{file});
        var main_tests = b.addTest(filePath);
        main_tests.setBuildMode(mode);

        test_step.dependOn(&main_tests.step);
        // test_step.dependOn(&b.addTest(filePath).setBuildMode(mode));
    }
}
