const std = @import("std");
const testing = std.testing;

const meta_ext = @import("meta_ext.zig");

test {
    @import("std").testing.refAllDecls(@This());
}

// TODO: make all of this much better

// ---

pub const FormatOptions = struct {
    print_types: bool,
};

pub fn formatDebug(
    value: anytype,
    options: FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    return formatDebugMargin(value, options, writer, .{});
}

fn formatDebugMargin(
    value: anytype,
    options: FormatOptions,
    writer: anytype,
    margin_in: MarginState,
) @TypeOf(writer).Error!void {
    const T = @TypeOf(value);
    const typename = "(" ++ @typeName(T) ++ ")";

    var margin = try margin_in.fillMargin(writer);

    if (comptime std.meta.trait.isZigString(T)) {
        if (options.print_types) try writer.writeAll(typename);
        return writer.print("\"{s}\"", .{value});
    }

    if (comptime meta_ext.isArrayList(T)) {
        return formatDebugMargin(value.items, options, writer, margin);
    }

    if (comptime std.meta.trait.isSlice(T) or @typeInfo(T) == .Array) {
        if (options.print_types) try writer.writeAll(typename);
        try writer.writeAll("[");

        margin = margin.inc();
        for (value) |entry| {
            margin = (try margin.nextLineFilled(writer)).pushComma();
            try formatDebugMargin(entry, options, writer, margin.fork());
        }
        margin = margin.dec();

        margin = try margin.nextLineFilled(writer);
        try writer.writeAll("]");

        return;
    }

    if (comptime meta_ext.isHashMap(T)) {
        const name = "(" ++ meta_ext.hashMapName(T) ++ ")";
        if (options.print_types) try writer.writeAll(name);
        try writer.writeAll("{");

        margin = margin.inc();
        var it = value.iterator();
        while (it.next()) |entry| {
            margin = (try margin.nextLineFilled(writer)).pushComma();
            try writer.writeAll(".");
            try formatDebugMargin(entry.key_ptr.*, options, writer, margin.fork());
            try writer.writeAll(" = ");
            try formatDebugMargin(entry.value_ptr.*, options, writer, margin.fork());
        }
        margin = margin.dec();

        margin = try margin.nextLineFilled(writer);
        try writer.writeAll("}");

        return;
    }

    switch (@typeInfo(T)) {
        .Pointer => |info| {
            const name = "(*" ++ @typeName(info.child) ++ ")";
            if (options.print_types) try writer.writeAll(name);
            try formatDebugMargin(value.*, options, writer, margin.fork());
        },
        .Struct => |info| {
            if (options.print_types) try writer.writeAll(typename);

            if (comptime std.meta.trait.hasFn("format")(T)) {
                return value.format("", .{}, writer);
            }

            try writer.writeAll("{");

            margin = margin.inc();
            inline for (info.fields) |field| {
                margin = (try margin.nextLineFilled(writer)).pushComma();
                try writer.writeAll(".");
                try formatDebugMargin(field.name, options, writer, margin.fork());
                try writer.writeAll(" = ");
                try formatDebugMargin(@field(value, field.name), options, writer, margin.fork());
            }
            margin = margin.dec();

            margin = try margin.nextLineFilled(writer);
            try writer.writeAll("}");
        },
        .Enum => |info| {
            if (options.print_types) try writer.writeAll(typename);

            if (comptime std.meta.trait.hasFn("format")(T)) {
                return value.format("", options, writer);
            }

            if (info.is_exhaustive) {
                try writer.writeAll(".");
                try writer.writeAll(@tagName(value));
                return;
            }

            @setEvalBranchQuota(3 * info.fields.len);
            inline for (info.fields) |field| {
                if (@enumToInt(value) == field.value) {
                    try writer.writeAll(".");
                    try writer.writeAll(@tagName(value));
                    return;
                }
            }

            // try writer.writeAll("(");
            // try formatType(@enumToInt(value), actual_fmt, options, writer, max_depth);
            // try writer.writeAll(")");

        },
        .Union => |info| {
            if (options.print_types) try writer.writeAll(typename);

            if (info.tag_type) |UnionTagType| {
                try writer.writeAll("{ .");
                try writer.writeAll(@tagName(@as(UnionTagType, value)));
                try writer.writeAll(" = ");
                inline for (info.fields) |field| {
                    if (value == @field(UnionTagType, field.name)) {
                        const inner = @field(value, field.name);
                        try formatDebugMargin(inner, options, writer, margin.fork());
                    }
                }
                try writer.writeAll(" }");
            } else {
                // TODO: eh?
                try std.fmt.format(writer, "@{x}", .{@ptrToInt(&value)});
            }
        },
        .Int, .ComptimeInt, .ComptimeFloat, .Float, .Bool => {
            if (options.print_types) try writer.writeAll(typename);
            try writer.print("{}", .{value});
        },
        .Optional => {
            if (value) |inner| {
                try writer.writeAll("?(");
                try formatDebugMargin(inner, options, writer, margin);
                try writer.writeAll(")");
            } else {
                return writer.writeAll("null");
            }
        },
        else => try writer.writeAll(@typeName(T)),
    }
}

const MarginState = struct {
    required_margin: u16 = 0,
    cur_margin: u16 = 0,
    comma_stack: u8 = 0,

    const Self = @This();

    fn fork(self: Self) Self {
        var new = self;
        new.comma_stack -|= 1;
        return new;
    }

    fn fillMargin(self: Self, writer: anytype) @TypeOf(writer).Error!Self {
        try writer.writeByteNTimes(' ', self.required_margin - self.cur_margin);
        var forked = self;
        forked.cur_margin = forked.required_margin;
        return forked;
    }

    fn nextLine(self: Self, writer: anytype) @TypeOf(writer).Error!Self {
        var forked = try self.popComma(writer);
        try writer.writeAll("\n");
        forked.cur_margin = 0;
        return forked;
    }

    fn nextLineFilled(self: Self, writer: anytype) @TypeOf(writer).Error!Self {
        var forked = try self.nextLine(writer);
        return forked.fillMargin(writer);
    }

    fn pushComma(self: Self) Self {
        var forked = self;
        forked.comma_stack +|= 1;
        return forked;
    }
    fn popComma(self: Self, writer: anytype) @TypeOf(writer).Error!Self {
        if (self.comma_stack > 0) try writer.writeAll(",");
        var forked = self;
        forked.comma_stack -|= 1;
        return forked;
    }

    fn inc(self: Self) Self {
        const required = self.required_margin + 4;
        var forked = self;
        forked.required_margin = required;
        forked.cur_margin = std.math.min(self.cur_margin, required);
        return forked;
    }
    fn dec(self: Self) Self {
        const required = self.required_margin - 4;
        var forked = self;
        forked.required_margin = required;
        forked.cur_margin = std.math.min(self.cur_margin, required);
        return forked;
    }
};

test "fmt_ext: DebugFormatter: flat map" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const Map = std.StringArrayHashMap([]const f32);

    var map = Map.init(allocator);
    try map.put("aaa", &[_]f32{ 42.0, 666.0 });
    try map.put("bbb", &[_]f32{ 420.420, 666.111 });

    {
        const expected =
            \\{
            \\    ."aaa" = [
            \\        4.2e+01,
            \\        6.66e+02,
            \\    ],
            \\    ."bbb" = [
            \\        4.20420013e+02,
            \\        6.66111022e+02,
            \\    ],
            \\}
        ;
        const opts = FormatOptions{ .print_types = false };
        var buf = std.ArrayList(u8).init(allocator);
        try formatDebug(map, opts, buf.writer());

        try testing.expectEqualStrings(expected, buf.items);
    }

    {
        const expected =
            \\(std.HashMap([]const u8, []const f32)){
            \\    .([]const u8)"aaa" = ([]const f32)[
            \\        (f32)4.2e+01,
            \\        (f32)6.66e+02,
            \\    ],
            \\    .([]const u8)"bbb" = ([]const f32)[
            \\        (f32)4.20420013e+02,
            \\        (f32)6.66111022e+02,
            \\    ],
            \\}
        ;
        const opts = FormatOptions{ .print_types = true };
        var buf = std.ArrayList(u8).init(allocator);
        try formatDebug(map, opts, buf.writer());

        try testing.expectEqualStrings(expected, buf.items);
    }
}

test "fmt_ext: DebugFormatter: L2 map" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const InnerMap = std.StringArrayHashMap([]const f32);
    const Map = std.AutoHashMap(usize, InnerMap);

    var innermap = InnerMap.init(allocator);
    try innermap.put("aaa", &[_]f32{ 42.0, 666.0 });
    try innermap.put("bbb", &[_]f32{ 420.420, 666.111 });

    var map = Map.init(allocator);
    try map.put(42, innermap);
    try map.put(666, innermap);

    {
        const expected =
            \\{
            \\    .42 = {
            \\        ."aaa" = [
            \\            4.2e+01,
            \\            6.66e+02,
            \\        ],
            \\        ."bbb" = [
            \\            4.20420013e+02,
            \\            6.66111022e+02,
            \\        ],
            \\    },
            \\    .666 = {
            \\        ."aaa" = [
            \\            4.2e+01,
            \\            6.66e+02,
            \\        ],
            \\        ."bbb" = [
            \\            4.20420013e+02,
            \\            6.66111022e+02,
            \\        ],
            \\    },
            \\}
        ;
        const opts = FormatOptions{ .print_types = false };
        var buf = std.ArrayList(u8).init(allocator);
        try formatDebug(map, opts, buf.writer());

        try testing.expectEqualStrings(expected, buf.items);
    }

    {
        const expected =
            \\(std.HashMap(usize, std.HashMap([]const u8, []const f32))){
            \\    .(usize)42 = (std.HashMap([]const u8, []const f32)){
            \\        .([]const u8)"aaa" = ([]const f32)[
            \\            (f32)4.2e+01,
            \\            (f32)6.66e+02,
            \\        ],
            \\        .([]const u8)"bbb" = ([]const f32)[
            \\            (f32)4.20420013e+02,
            \\            (f32)6.66111022e+02,
            \\        ],
            \\    },
            \\    .(usize)666 = (std.HashMap([]const u8, []const f32)){
            \\        .([]const u8)"aaa" = ([]const f32)[
            \\            (f32)4.2e+01,
            \\            (f32)6.66e+02,
            \\        ],
            \\        .([]const u8)"bbb" = ([]const f32)[
            \\            (f32)4.20420013e+02,
            \\            (f32)6.66111022e+02,
            \\        ],
            \\    },
            \\}
        ;
        const opts = FormatOptions{ .print_types = true };
        var buf = std.ArrayList(u8).init(allocator);
        try formatDebug(map, opts, buf.writer());

        try testing.expectEqualStrings(expected, buf.items);
    }
}

test "fmt_ext: DebugFormatter: fmt.Formatter support" {
    const Vec2 = struct {
        x: f32,
        y: f32,
    };
    const Vec2Custom = struct {
        x: f32,
        y: f32,

        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = options;
            if (fmt.len == 0 or comptime std.mem.eql(u8, fmt, "p")) {
                return std.fmt.format(writer, "({d:.3},{d:.3})", .{ self.x, self.y });
            } else if (comptime std.mem.eql(u8, fmt, "d")) {
                return std.fmt.format(writer, "{d:.3}x{d:.3}", .{ self.x, self.y });
            } else {
                @compileError("Unknown format character: '" ++ fmt ++ "'");
            }
        }
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const vecs: []const Vec2 = &[_]Vec2{
        .{ .x = 42.0, .y = 666.0 },
        .{ .x = 420.420, .y = 666.111 },
    };
    const custom_vecs: []const Vec2Custom = &[_]Vec2Custom{
        .{ .x = 42.0, .y = 666.0 },
        .{ .x = 420.420, .y = 666.111 },
    };

    {
        const expected =
            \\[
            \\    {
            \\        ."x" = 4.2e+01,
            \\        ."y" = 6.66e+02,
            \\    },
            \\    {
            \\        ."x" = 4.20420013e+02,
            \\        ."y" = 6.66111022e+02,
            \\    },
            \\]
        ;
        const opts = FormatOptions{ .print_types = false };
        var buf = std.ArrayList(u8).init(allocator);
        try formatDebug(vecs, opts, buf.writer());

        try testing.expectEqualStrings(expected, buf.items);
    }

    {
        const slicename = @typeName(@TypeOf(custom_vecs));
        const expected =
            "(" ++ slicename ++ ")[\n" ++
            \\    (Vec2Custom)(42.000,666.000),
            \\    (Vec2Custom)(420.420,666.111),
            \\]
        ;
        const opts = FormatOptions{ .print_types = true };
        var buf = std.ArrayList(u8).init(allocator);
        try formatDebug(custom_vecs, opts, buf.writer());

        try testing.expectEqualStrings(expected, buf.items);
    }
}
