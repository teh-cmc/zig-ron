const std = @import("std");
const testing = std.testing;

const meta_ext = @import("./meta_ext.zig");

test {
    @import("std").testing.refAllDecls(@This());
}

// TODO: redo this entire thing use std.meta.eql as a base.

// ---

const Error = error{
    TestExpectedEqual,
};

/// A fork of `std.testing.expectEqual` with (some) deep semantics.
pub fn expectEqual(expected: anytype, actual: @TypeOf(expected)) Error!void {
    const T = @TypeOf(actual);
    switch (@typeInfo(T)) {
        .NoReturn,
        .BoundFn,
        .Opaque,
        .Frame,
        .AnyFrame,
        => @compileError("value of type " ++ @typeName(T) ++ " encountered"),

        .Undefined,
        .Null,
        .Void,
        => return,

        .Type => {
            if (actual != expected) {
                std.debug.print("expected type {s}, found type {s}\n", .{
                    @typeName(expected),
                    @typeName(actual),
                });
                return error.TestExpectedEqual;
            }
        },

        .Bool,
        .Int,
        .Float,
        .ComptimeFloat,
        .ComptimeInt,
        .EnumLiteral,
        .Enum,
        .Fn,
        .ErrorSet,
        => {
            if (actual != expected) {
                std.debug.print("expected {}, found {}\n", .{ expected, actual });
                return error.TestExpectedEqual;
            }
        },

        .Pointer => |pointer| {
            const is_string = comptime std.meta.trait.isZigString(T);
            const is_slice = comptime std.meta.trait.isSlice(T);
            if (is_string) {
                try testing.expectEqualStrings(expected, actual);
                return;
            }
            if (is_slice) {
                const U = @typeInfo(T).Pointer.child;
                const is_nested_slice = comptime std.meta.trait.isSlice(U);
                if (is_nested_slice) {
                    if (actual.len != expected.len) {
                        std.debug.print(
                            "slices have different lengths: {*} vs. {*}\n",
                            .{ expected, actual },
                        );
                        return error.TestExpectedEqual;
                    }
                    for (expected) |expected_inner, i| {
                        try expectEqual(expected_inner, actual[i]);
                    }
                } else {
                    try testing.expectEqualSlices(U, expected, actual);
                }
                return;
            }

            switch (pointer.size) {
                .One => return expectEqual(expected.*, actual.*),
                .Many, .C => {
                    if (actual != expected) {
                        std.debug.print("expected {*}, found {*}\n", .{ expected, actual });
                        return error.TestExpectedEqual;
                    }
                },
                .Slice => {
                    if (actual.ptr != expected.ptr) {
                        std.debug.print("expected slice ptr {*}, found {*}\n", .{
                            expected.ptr,
                            actual.ptr,
                        });
                        return error.TestExpectedEqual;
                    }
                    if (actual.len != expected.len) {
                        std.debug.print("expected slice len {}, found {}\n", .{
                            expected.len,
                            actual.len,
                        });
                        return error.TestExpectedEqual;
                    }
                },
            }
        },

        .Array => |array| try testing.expectEqualSlices(array.child, &expected, &actual),

        .Vector => |vectorType| {
            var i: usize = 0;
            while (i < vectorType.len) : (i += 1) {
                if (!std.meta.eql(expected[i], actual[i])) {
                    std.debug.print("index {} incorrect. expected {}, found {}\n", .{
                        i,
                        expected[i],
                        actual[i],
                    });
                    return error.TestExpectedEqual;
                }
            }
        },

        .Struct => |structType| {
            if (comptime meta_ext.isArrayList(T)) {
                if (expected.items.len != actual.items.len) {
                    std.debug.print("expected array-list count {}, found {}\n", .{
                        expected.items.len,
                        actual.items.len,
                    });
                    return error.TestExpectedEqual;
                }
                for (expected.items) |val, i| {
                    try expectEqual(val, actual.items[i]);
                }
                return;
            }

            if (comptime meta_ext.isHashMap(T)) {
                if (expected.count() != actual.count()) {
                    std.debug.print("expected map count {}, found {}\n", .{
                        expected.count(),
                        actual.count(),
                    });
                    return error.TestExpectedEqual;
                }
                var expected_it = expected.iterator();
                while (expected_it.next()) |expected_entry| {
                    const actual_value = actual.get(expected_entry.key_ptr.*) orelse return error.TestExpectedEqual;
                    try expectEqual(expected_entry.value_ptr.*, actual_value);
                }
                return;
            }

            inline for (structType.fields) |field| {
                try expectEqual(@field(expected, field.name), @field(actual, field.name));
            }
        },

        .Union => |union_info| {
            if (union_info.tag_type == null) {
                @compileError("Unable to compare untagged union values");
            }

            const Tag = std.meta.Tag(@TypeOf(expected));

            const expectedTag = @as(Tag, expected);
            const actualTag = @as(Tag, actual);

            try expectEqual(expectedTag, actualTag);

            // we only reach this loop if the tags are equal
            inline for (std.meta.fields(@TypeOf(actual))) |fld| {
                if (std.mem.eql(u8, fld.name, @tagName(actualTag))) {
                    try expectEqual(@field(expected, fld.name), @field(actual, fld.name));
                    return;
                }
            }

            // we iterate over *all* union fields
            // => we should never get here as the loop above is
            //    including all possible values.
            unreachable;
        },

        .Optional => {
            if (expected) |expected_payload| {
                if (actual) |actual_payload| {
                    try expectEqual(expected_payload, actual_payload);
                } else {
                    std.debug.print("expected {any}, found null\n", .{expected_payload});
                    return error.TestExpectedEqual;
                }
            } else {
                if (actual) |actual_payload| {
                    std.debug.print("expected null, found {any}\n", .{actual_payload});
                    return error.TestExpectedEqual;
                }
            }
        },

        .ErrorUnion => {
            if (expected) |expected_payload| {
                if (actual) |actual_payload| {
                    try expectEqual(expected_payload, actual_payload);
                } else |actual_err| {
                    std.debug.print("expected {any}, found {}\n", .{ expected_payload, actual_err });
                    return error.TestExpectedEqual;
                }
            } else |expected_err| {
                if (actual) |actual_payload| {
                    std.debug.print("expected {}, found {any}\n", .{ expected_err, actual_payload });
                    return error.TestExpectedEqual;
                } else |actual_err| {
                    try expectEqual(expected_err, actual_err);
                }
            }
        },
    }
}
