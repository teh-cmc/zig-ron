const std = @import("std");
const testing = std.testing;

const ron = @import("ron");
const testing_ext = ron.testing_ext;

// ---

test "Value" {
    comptime var seq_values = [_]ron.Value{ .{ .bool = true }, .{ .string = "pouet" } };
    const seq = ron.ValueSeq.fromOwnedSlice(testing.allocator, &seq_values);

    var map = ron.ValueMap.init(testing.allocator);
    defer map.deinit();
    try map.put("hello", .{ .float = 42.111 });
    try map.put("world", .{ .string = "666.420" });

    comptime var deepstr = .{ .string = "deep" };
    comptime var opt = .{ .option = &deepstr };

    const All = struct {
        bool: ron.Value,
        bool_def: ron.Value = .{ .bool = true },
        char: ron.Value,
        char_def: ron.Value = .{ .char = ron.Char.fromComptimeInt('ま') },
        integer: ron.Value,
        integer_def: ron.Value = .{ .integer = 42 },
        float: ron.Value,
        float_def: ron.Value = .{ .float = 42.424242 },
        string: ron.Value,
        string_def: ron.Value = .{ .string = "coucou" },
        unit: ron.Value,
        unit_def: ron.Value = .{ .unit = .{} },
        seq: ron.Value,
        seq_def: ron.Value = .{
            .seq = ron.ValueSeq.fromOwnedSlice(testing.allocator, &seq_values),
        },
        map: ron.Value,
        // TODO: cannot do map_def unless we specialize for ComptimeStringMap
        // etc ¯\_(ツ)_/¯
        option: ron.Value,
        option_def: ron.Value = opt,
    };
    _ = All;

    const str =
        \\All(
        \\  bool: bool(true),
        \\  char: char('ま'),
        \\  integer: integer(42),
        \\  float: float(42.424242),
        \\  string: string("coucou"),
        \\  unit: unit(()),
        \\  seq: seq([bool(true), string("pouet")]),
        \\  map: map({"hello": float(42.111), "world": string("666.420"),}),
        \\  option: option(Some(string("deep"))),
        \\)
    ;
    _ = str;

    const expected = All{
        .bool = .{ .bool = true },
        .bool_def = .{ .bool = true },
        .char = .{ .char = ron.Char.fromComptimeInt('ま') },
        .char_def = .{ .char = ron.Char.fromComptimeInt('ま') },
        .integer = .{ .integer = 42 },
        .integer_def = .{ .integer = 42 },
        .float = .{ .float = 42.424242 },
        .float_def = .{ .float = 42.424242 },
        .string = .{ .string = "coucou" },
        .string_def = .{ .string = "coucou" },
        .unit = .{ .unit = .{} },
        .unit_def = .{ .unit = .{} },
        .seq = .{ .seq = seq },
        .seq_def = .{ .seq = seq },
        .map = .{ .map = map },
        .option = opt,
        .option_def = opt,
    };
    _ = expected;

    var deser = ron.Deserializer.init(testing.allocator);
    defer deser.deinit();
    defer deser.parseFree();

    const res = deser.parse(All, &ron.Lexer.init(str).token_stream) catch |err| {
        try deser.report(std.io.getStdErr().writer());
        return err;
    };
    try testing_ext.expectEqual(expected, res);
}
