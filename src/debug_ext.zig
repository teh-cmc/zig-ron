const std = @import("std");
const path = std.fs.path;

const meta_ext = @import("./meta_ext.zig");

const fmt_ext = @import("./fmt_ext.zig");
const formatDebug = fmt_ext.formatDebug;

test {
    @import("std").testing.refAllDecls(@This());
}

// ---

/// Use with e.g. this snippet:
/// ```
/// snippet dbg "debug var" w
/// dbg(${1});
/// endsnippet
/// ```
pub fn dbg(v: anytype) @TypeOf(v) {
    const stderr = std.io.getStdErr().writer();
    formatDebug(v, .{ .print_types = false }, stderr) catch return v;
    stderr.writeAll("\n") catch return v;
    return v;
}

/// Use with e.g. this snippet:
/// ```
/// snippet dbg "debug var verbose" w
/// dbgv(${1});
/// endsnippet
/// ```
pub fn dbgv(v: anytype) @TypeOf(v) {
    const stderr = std.io.getStdErr().writer();
    formatDebug(v, .{ .print_types = true }, stderr) catch return v;
    stderr.writeAll("\n") catch return v;
    return v;
}

/// Use with e.g. this snippet:
/// ```
/// snippet dbg "debug var src" w
/// dbgs(@src(), ${1});
/// endsnippet
/// ```
pub fn dbgs(src: std.builtin.SourceLocation, v: anytype) @TypeOf(v) {
    const stderr = std.io.getStdErr().writer();
    stderr.print("[{s}:{}:{}]: ", .{
        path.basename(src.file),
        src.line,
        src.column,
    }) catch return v;
    formatDebug(v, .{ .print_types = false }, stderr) catch return v;
    stderr.writeAll("\n") catch return v;
    return v;
}

/// Use with e.g. this snippet:
/// ```
/// snippet dbg "debug var src verbose" w
/// dbgsv(@src(), ${1});
/// endsnippet
/// ```
pub fn dbgsv(src: std.builtin.SourceLocation, v: anytype) @TypeOf(v) {
    const stderr = std.io.getStdErr().writer();
    stderr.print("[{s}:{}:{}]: ", .{
        path.basename(src.file),
        src.line,
        src.column,
    }) catch return v;
    formatDebug(v, .{ .print_types = true }, stderr) catch return v;
    stderr.writeAll("\n") catch return v;
    return v;
}
