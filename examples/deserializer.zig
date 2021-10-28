const std = @import("std");
const ron = @import("ron");

const MyStruct = struct {
    bool: bool,
    float: f32,
    int: i42,
    string: []const u8,
    char: ron.Char,
    unit: ron.Unit,

    array_list: std.ArrayList(u32),
    slice: []u32,

    string_map: std.StringArrayHashMap([]const u8),
    map: std.AutoArrayHashMap(bool, []const u8),

    option: ?[]const u8,
    enums: enum { a, b, c },
    unions: union(enum) { a: bool, b: []const u8, c: f32 },
    any: ron.Value, // Generic heterogeneous values

    nested: ?*@This(),

    default: []const u8 = "some default value",
};

const data =
    \\MyStruct(
    \\  bool: true,
    \\  float: 42.420,
    \\  int: 666,
    \\  string: "ガタカへようこそ",
    \\  char: '🚁',
    \\  unit: (),
    \\
    \\  array_list: [1, 2, 3, 4],
    \\  slice: [5, 6, 7, 8],
    \\
    \\  string_map: { "hello": "world" },
    \\  map: { false: "is false", true: "is not false" },
    \\
    \\  option: Some("\u{263A}"),
    \\  enums: b,
    \\  unions: c(666.111),
    \\  any: seq([string("hey"), bool(true), float(42.42)]),
    \\
    // Tuple to struct coercion!
    \\  nested: Some((
    \\    false, 1.0, 2, "xxx", 'X', (),
    \\    [], [], {}, {},
    \\    None, a, b("nope!"), option(None),
    \\    None,
    \\  )),
    \\
    // This will be silently ignored.
    \\  field_that_doesnt_exist: DoesntExist(
    \\    xxx: 42,
    \\    42: "xxx"
    \\  ),
    \\)
;

pub fn main() !void {
    var allocator = std.heap.page_allocator;

    var deser = ron.Deserializer.init(allocator);
    // clears internal parser resources
    defer deser.deinit();
    // clears data allocated for the end-user (arrays, maps, escaped strings...)
    defer deser.parseFree();

    var tokens = &ron.Lexer.init(data).token_stream;
    const res = deser.parse(MyStruct, tokens) catch |err| {
        try deser.report(std.io.getStdErr().writer());
        return err;
    };

    _ = ron.debug_ext.dbg(res);
}
