const std = @import("std");

pub fn main() !void {
    const title = "DIMS: A Platform for Decentralized Research and Collaboration";
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

    try stdout.print("\n{s}\n", .{msg});

    // Thread pool.
    const cpus = try std.Thread.getCpuCount();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var pool: std.Thread.Pool = undefined;
    try pool.init(.{ .allocator = allocator });
    defer pool.deinit();

    try stdout.print("\n", .{});

    try stdout.print("Starting Work...\n", .{});

    for (0..cpus) |i| {
        try pool.spawn(work, .{i});
    }

    try bw.flush();
}

fn work(id: usize) void {
    std.time.sleep(1 * std.time.ns_per_s);
    std.debug.print("{} finished\n", .{id});
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
