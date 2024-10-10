const std = @import("std");

pub fn main() !void {
    const title = "DIMS: An Open Decentralized Research Aggregation Platform";
    const msg = "- Code will go here if funded!";

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    for (title) |_| {
        try stdout.print("=", .{});
    }

    try stdout.print("\n{s}\n", .{title});

    for (title) |_| {
        try stdout.print("=", .{});
    }

    try stdout.print("\n{s}\n\n", .{msg});

    try bw.flush(); // don't forget to flush!
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
