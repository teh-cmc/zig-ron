const std = @import("std");

test {
    @import("std").testing.refAllDecls(@This());
}

const Here = @This();
pub const Exports = struct {
    pub const Value = Here.Value;
    pub const ValueMap = Here.ValueMap;
    pub const ValueSeq = Here.ValueSeq;
    pub const Char = Here.Char;
    pub const Unit = Here.Unit;
};

// ---

pub const Char = struct {
    codepoint: u21,

    pub fn fromComptimeInt(comptime c: comptime_int) Char {
        return Char{ .codepoint = c };
    }

    pub fn fromUtf8Slice(bytes: []const u8) !Char {
        const codepoint = try std.unicode.utf8Decode(bytes);
        return Char{ .codepoint = codepoint };
    }

    pub fn format(
        self: Char,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        var buf: [4]u8 = undefined;
        const n = std.unicode.utf8Encode(self.codepoint, &buf) catch unreachable;
        return writer.print("{s}", .{buf[0..n]});
    }
};

pub const Unit = struct {};

pub const ValueMap = std.StringArrayHashMap(Value);

pub const ValueSeq = std.ArrayList(Value);

pub const Value = union(enum) {
    bool: bool,
    char: Char,
    integer: i64,
    float: f64,
    string: []const u8,
    unit: Unit,
    seq: ValueSeq,
    map: ValueMap,
    option: ?*Value,
};
