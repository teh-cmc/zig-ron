const std = @import("std");
const testing = std.testing;

const testing_ext = @import("./testing_ext.zig");
const expectEqualDeep = testing_ext.expectEqual;

const debug_ext = @import("./debug_ext.zig");
const dbg = debug_ext.dbg;

test {
    @import("std").testing.refAllDecls(@This());
}

// TODO: merge this work with `std.meta.eql`

// ---

pub fn isArrayList(comptime T: type) bool {
    return std.meta.trait.isContainer(T) and std.meta.trait.hasFunctions(T, .{
        "append", "appendSlice", "resize",
    }) and @hasField(T, "items");
}

test "meta_ext: isArrayList" {
    const ArrayList = std.ArrayList(u32);
    const ArrayListUnmanaged = std.ArrayListUnmanaged(u32);
    const ArrayListAligned = std.ArrayListAligned(u32, null);
    const ArrayListAlignedUnmanaged = std.ArrayListAlignedUnmanaged(u32, null);

    try testing.expect(isArrayList(ArrayList));
    try testing.expect(isArrayList(ArrayListUnmanaged));
    try testing.expect(isArrayList(ArrayListAligned));
    try testing.expect(isArrayList(ArrayListAlignedUnmanaged));
}

pub fn ArrayListType(comptime S: type) type {
    const Slice = @typeInfo(S).Struct.fields[0].field_type;
    return @typeInfo(Slice).Pointer.child;
}

test "meta_ext: hashMapTypes" {
    const ArrayList = std.ArrayList(u32);
    const ArrayListUnmanaged = std.ArrayListUnmanaged(u32);
    const ArrayListAligned = std.ArrayListAligned(u32, null);
    const ArrayListAlignedUnmanaged = std.ArrayListAlignedUnmanaged(u32, null);

    try testing.expectEqual(ArrayListType(ArrayList), u32);
    try testing.expectEqual(ArrayListType(ArrayListUnmanaged), u32);
    try testing.expectEqual(ArrayListType(ArrayListAligned), u32);
    try testing.expectEqual(ArrayListType(ArrayListAlignedUnmanaged), u32);
}

// ---

pub fn isHashMap(comptime T: type) bool {
    return std.meta.trait.isContainer(T) and std.meta.trait.hasFunctions(T, .{
        "get", "put", "putNoClobber", "count",
    }) and @hasDecl(T, "KV");
}

test "meta_ext: isHashMap" {
    const AutoContext = std.hash_map.AutoContext;
    const AutoArrayContext = std.array_hash_map.AutoContext;

    const HashMap = std.HashMap(u32, bool, AutoContext(u32), 42);
    const HashMapUnmanaged = std.HashMapUnmanaged(u32, bool, AutoContext(u32), 42);
    const StringHashMap = std.StringHashMap(bool);
    const StringHashMapUnmanaged = std.StringHashMapUnmanaged(bool);
    const AutoHashMap = std.AutoHashMap(u32, bool);
    const ArrayHashMap = std.ArrayHashMap(u32, bool, AutoArrayContext(u32), false);
    const AutoArrayHashMap = std.AutoArrayHashMap(u32, bool);

    try testing.expect(isHashMap(HashMap));
    try testing.expect(isHashMap(HashMapUnmanaged));
    try testing.expect(isHashMap(StringHashMap));
    try testing.expect(isHashMap(StringHashMapUnmanaged));
    try testing.expect(isHashMap(AutoHashMap));
    try testing.expect(isHashMap(ArrayHashMap));
    try testing.expect(isHashMap(AutoArrayHashMap));
}

pub fn hashMapTypes(comptime M: type) struct { K: type, V: type } {
    const KV = @typeInfo(M.KV);
    const K = KV.Struct.fields[0].field_type;
    const V = KV.Struct.fields[1].field_type;
    return .{ .K = K, .V = V };
}

test "meta_ext: hashMapTypes" {
    const AutoContext = std.hash_map.AutoContext;
    const AutoArrayContext = std.array_hash_map.AutoContext;

    const HashMap = std.HashMap(u32, bool, AutoContext(u32), 42);
    const HashMapUnmanaged = std.HashMapUnmanaged(u32, bool, AutoContext(u32), 42);
    const StringHashMap = std.StringHashMap(bool);
    const StringHashMapUnmanaged = std.StringHashMapUnmanaged(bool);
    const AutoHashMap = std.AutoHashMap(u32, bool);
    const ArrayHashMap = std.ArrayHashMap(u32, bool, AutoArrayContext(u32), false);
    const AutoArrayHashMap = std.AutoArrayHashMap(u32, bool);

    try testing.expectEqual(hashMapTypes(HashMap), .{ .K = u32, .V = bool });
    try testing.expectEqual(hashMapTypes(HashMapUnmanaged), .{ .K = u32, .V = bool });
    try testing.expectEqual(hashMapTypes(StringHashMap), .{ .K = []const u8, .V = bool });
    try testing.expectEqual(hashMapTypes(StringHashMapUnmanaged), .{ .K = []const u8, .V = bool });
    try testing.expectEqual(hashMapTypes(AutoHashMap), .{ .K = u32, .V = bool });
    try testing.expectEqual(hashMapTypes(ArrayHashMap), .{ .K = u32, .V = bool });
    try testing.expectEqual(hashMapTypes(AutoArrayHashMap), .{ .K = u32, .V = bool });
}

pub fn hashMapName(comptime M: type) []const u8 {
    const kv = hashMapTypes(M);
    const K = comptime switch (isHashMap(kv.K)) {
        true => hashMapName(kv.K),
        false => @typeName(kv.K),
    };
    const V = comptime switch (isHashMap(kv.V)) {
        true => hashMapName(kv.V),
        false => @typeName(kv.V),
    };

    return "std.HashMap(" ++ K ++ ", " ++ V ++ ")";
}

test "meta_ext: hashMapName" {
    const AutoContext = std.hash_map.AutoContext;
    const AutoArrayContext = std.array_hash_map.AutoContext;

    const HashMap = std.HashMap(u32, bool, AutoContext(u32), 42);
    const HashMapUnmanaged = std.HashMapUnmanaged(u32, bool, AutoContext(u32), 42);
    const StringHashMap = std.StringHashMap(bool);
    const StringHashMapUnmanaged = std.StringHashMapUnmanaged(bool);
    const AutoHashMap = std.AutoHashMap(u32, bool);
    const ArrayHashMap = std.ArrayHashMap(u32, bool, AutoArrayContext(u32), false);
    const AutoArrayHashMap = std.AutoArrayHashMap(u32, bool);

    try expectEqualDeep(hashMapName(HashMap), "std.HashMap(u32, bool)");
    try expectEqualDeep(hashMapName(HashMapUnmanaged), "std.HashMap(u32, bool)");
    try expectEqualDeep(hashMapName(StringHashMap), "std.HashMap([]const u8, bool)");
    try expectEqualDeep(hashMapName(StringHashMapUnmanaged), "std.HashMap([]const u8, bool)");
    try expectEqualDeep(hashMapName(AutoHashMap), "std.HashMap(u32, bool)");
    try expectEqualDeep(hashMapName(ArrayHashMap), "std.HashMap(u32, bool)");
    try expectEqualDeep(hashMapName(AutoArrayHashMap), "std.HashMap(u32, bool)");

    const NestedKey = std.AutoHashMap(StringHashMap, i64);
    const NestedValue = std.AutoHashMap(i64, StringHashMap);
    const NestedKeyValue = std.AutoHashMap(AutoArrayHashMap, StringHashMap);

    try expectEqualDeep(
        hashMapName(NestedKey),
        "std.HashMap(std.HashMap([]const u8, bool), i64)",
    );
    try expectEqualDeep(
        hashMapName(NestedValue),
        "std.HashMap(i64, std.HashMap([]const u8, bool))",
    );
    try expectEqualDeep(
        hashMapName(NestedKeyValue),
        "std.HashMap(std.HashMap(u32, bool), std.HashMap([]const u8, bool))",
    );
}

// ---

// TODO: doc
pub fn tagPayloadValue(
    u: anytype,
    comptime tag: std.meta.Tag(@TypeOf(u)),
) ?std.meta.TagPayload(@TypeOf(u), tag) {
    if (std.meta.activeTag(u) == tag) {
        return @field(u, @tagName(tag));
    } else {
        return null;
    }
}

test "meta_ext: TagPayloadValue" {
    const IsIt = struct { is_it: bool };
    const MyTaggedUnion = union(enum) {
        a: IsIt,
        b: []const u8,
        c: f32,
        d,
    };

    {
        const expected = .{ .is_it = true };
        const u = MyTaggedUnion{ .a = expected };
        try expectEqualDeep(@as(?IsIt, expected), tagPayloadValue(u, .a));
    }
    {
        const expected = "coucou";
        const u = MyTaggedUnion{ .b = expected };
        try expectEqualDeep(@as(?[]const u8, expected), tagPayloadValue(u, .b));
    }
    {
        const expected = 42.666;
        const u = MyTaggedUnion{ .c = expected };
        try expectEqualDeep(@as(?f32, expected), tagPayloadValue(u, .c));
    }
    {
        const expected: void = .{};
        const u = MyTaggedUnion{ .d = expected };
        try expectEqualDeep(@as(?void, expected), tagPayloadValue(u, .d));
    }

    {
        const u = MyTaggedUnion{ .a = .{ .is_it = false } };
        try expectEqualDeep(@as(?f32, null), tagPayloadValue(u, .c));
    }
}
