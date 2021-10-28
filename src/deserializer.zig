const std = @import("std");
const mem = std.mem;
const testing = std.testing;

const Char = @import("./types.zig").Char;
const Unit = @import("./types.zig").Unit;

const lexer = @import("./lexer.zig");
const Lexer = lexer.Lexer;
const LexerError = lexer.Error;
const Token = lexer.Token;
const TokenDumpOptions = lexer.DumpOptions;
const TokenStream = lexer.TokenStream;

const unicode = @import("unicode.zig");
const unescape = unicode.unescape;

const meta_ext = @import("./meta_ext.zig");
const isArrayList = meta_ext.isArrayList;
const ArrayListType = meta_ext.ArrayListType;
const isHashMap = meta_ext.isHashMap;
const hashMapTypes = meta_ext.hashMapTypes;
const tagPayloadValue = meta_ext.tagPayloadValue;

const testing_ext = @import("./testing_ext.zig");
const expectEqualDeep = testing_ext.expectEqual;

const debug_ext = @import("./debug_ext.zig");
const dbg = debug_ext.dbg;
const dbgv = debug_ext.dbgv;

test {
    @import("std").testing.refAllDecls(@This());
}

const Here = @This();
pub const Exports = struct {
    pub const Deserializer = Here.Deserializer;
    pub const DeserializerError = Here.Error;
};

// ---

pub const Error = error{
    UnexpectedToken,
    UnexpectedValue,
    UnexpectedEndOfRon,
    UnterminatedEscapeSequence,
    IllegalEscapeSequence,
    IllegalUtf8Data,
    StructMissingField,
} || LexerError || error{OutOfMemory};

pub const Deserializer = struct {
    const Self = @This();

    line_nr: usize,
    col_nr: usize,
    cur_idx: usize,
    total_bytes: usize,

    arena: std.heap.ArenaAllocator,
    err: std.ArrayList(u8),

    pub fn init(allocator: *mem.Allocator) Self {
        return .{
            .line_nr = 0,
            .col_nr = 0,
            .cur_idx = 0,
            .total_bytes = 0,

            .arena = std.heap.ArenaAllocator.init(allocator),
            .err = std.ArrayList(u8).init(allocator),
        };
    }
    /// Cleans up any extra stuff that was allocated to help with deserialization
    /// itself; this won't affect any of the parsed data (see `Self.parseFree`).
    pub fn deinit(self: *Self) void {
        self.err.deinit();
    }

    pub fn reset(self: *Self) void {
        self.line_nr = 0;
        self.col_nr = 0;
        self.cur_idx = 0;
        self.err.resize(0) catch unreachable; // TODO: is that the right way?
        // Don't reset .total_bytes!
    }

    pub fn parse(self: *Self, comptime T: type, tokens: *TokenStream) Error!T {
        const typename = @typeName(T);
        try self.discardWhitespace(tokens);
        switch (comptime @typeInfo(T)) {
            .Bool => return parseBool(self, tokens),
            .Float, .ComptimeFloat => return parseFloat(self, T, tokens),
            .Int, .ComptimeInt => return parseInt(self, T, tokens),
            .Enum => return parseEnum(self, T, tokens),
            .Union => |info| {
                if (comptime info.tag_type) |Enum| {
                    const tag = try parseEnum(self, Enum, tokens);
                    const tagName = @tagName(tag);

                    _ = try self.expectToken(tokens, .OpenParen);
                    inline for (info.fields) |field| {
                        if (std.mem.eql(u8, field.name, tagName)) {
                            const value = try self.parse(field.field_type, tokens);
                            _ = try self.expectToken(tokens, .CloseParen);
                            return @unionInit(T, field.name, value);
                        }
                    }

                    // If the inner enum tag could be successfully parsed, then
                    // it must be one of the fields of the tagged union, and as
                    // such we cannot leave the inlined loop above.
                    unreachable;
                } else {
                    @compileError("cannot deserialize into untagged union `" ++ @typeName(T) ++ "`");
                }
            },
            .Struct => |info| {
                _ = info;
                if (comptime isArrayList(T)) {
                    return parseArrayList(self, T, tokens);
                } else if (comptime isHashMap(T)) {
                    return parseHashMap(self, T, tokens);
                } else if (comptime T == Char) {
                    return parseChar(self, tokens);
                } else if (comptime std.meta.trait.isTuple(T)) {
                    return parseTuple(self, T, tokens);
                } else {
                    return parseStruct(self, T, tokens);
                }
            },
            .Pointer => |info| {
                if (comptime std.meta.trait.isZigString(T)) {
                    return parseString(self, tokens);
                } else if (comptime std.meta.trait.isSlice(T)) {
                    return parseOwnedSlice(self, info.child, tokens);
                } else if (info.size == .One) {
                    const res: T = try self.arena.allocator.create(info.child);
                    errdefer self.arena.allocator.destroy(res);
                    res.* = try self.parse(info.child, tokens);
                    return res;
                }
            },
            .Optional => |info| return parseOption(self, info.child, tokens),
            else => {},
        }
        @compileError("deserializing into " ++ typename ++ " is not supported");
    }

    /// Releases the resources allocated while parsing (e.g. arrays, maps,
    /// escaped strings, etc...).
    ///
    /// Do not call this until you're done with your parsed data, or deep-copy
    /// it somewhere else first.
    pub fn parseFree(self: *Self) void {
        self.arena.deinit();
    }

    pub fn report(self: *Self, writer: anytype) !void {
        try writer.print("parsing failed ({}:{}): {s}\n", .{ self.line_nr, self.col_nr, self.err.items });
    }

    // ---

    fn next(self: *Self, tokens: *TokenStream) Error!?Token {
        if (comptime @import("builtin").is_test) {
            _ = dbg((try tokens.peek()));
        }
        const tok = (try tokens.next()) orelse return null;
        switch (tok) {
            Token.Linefeed => |lf| {
                self.line_nr += lf;
                self.col_nr = 0;
            },
            Token.Whitespace => |ws| self.col_nr += ws,
            else => {},
        }
        self.cur_idx += tok.length();
        self.total_bytes += tok.length();
        return tok;
    }

    fn expectToken(
        self: *Self,
        tokens: *TokenStream,
        comptime tag: std.meta.Tag(Token),
    ) Error!std.meta.TagPayload(Token, tag) {
        try self.discardWhitespace(tokens);

        const tok = (try tokens.peek()) orelse return error.UnexpectedEndOfRon;
        if (tagPayloadValue(tok, tag)) |payload| {
            _ = self.next(tokens) catch unreachable;
            return payload;
        } else {
            try self.err.writer().print(
                "expected `{s}`, found `{}` instead",
                .{ Token.formatTag(tag), tok },
            );
            return error.UnexpectedToken;
        }
    }

    fn discardWhitespace(self: *Self, tokens: *TokenStream) Error!void {
        while (try tokens.peek()) |tok| {
            switch (tok) {
                .Whitespace, .Linefeed => {
                    _ = self.next(tokens) catch unreachable;
                },
                else => break,
            }
        }
    }

    /// Discards the current value and advances the parser to the next.
    fn discardValue(self: *Deserializer, tokens: *TokenStream) Error!struct {
        start_idx: usize,
        end_idx: usize,
    } {
        const start_idx = self.cur_idx;

        var commas: usize = 1;
        var brackets: usize = 0;
        var parens: usize = 0;
        while (try self.next(tokens)) |tok| {
            switch (tok) {
                Token.Comma => if (brackets == 0 and parens == 0) {
                    commas -= 1;
                    if (commas == 0) break;
                },
                Token.OpenBracket => brackets += 1,
                Token.CloseBracket => brackets -= 1,
                Token.OpenParen => parens += 1,
                Token.CloseParen => parens -= 1,
                else => {},
            }
        }

        const discarded = .{ .start_idx = start_idx, .end_idx = self.cur_idx };
        return discarded;
    }

    test "parser: discard: whole object" {
        const str: []const u8 =
            \\Scene(
            \\  progress: 42.8,
            \\  active: true,
            \\  name: "final",
            \\  some_field_that_doesnt_exist: DoesntExist(
            \\      neither_does_this: [true, false, false],
            \\      yes: 42,
            \\  ),
            \\  rendering: (
            \\      msaa: true,
            \\      fov: 666.42,
            \\  ),
            \\  player: Player(
            \\      pos: (-10, 30, -200),
            \\  ),
            \\)
        ;

        var tokens = &Lexer.init(str).token_stream;
        var parser = Deserializer.init(testing.allocator);

        const discarded = try parser.discardValue(tokens);

        var buf = std.ArrayList(u8).init(testing.allocator);
        try tokens.dump(TokenDumpOptions.minify(), buf.writer());

        try expectEqualDeep(str, str[discarded.start_idx..discarded.end_idx]);
        try expectEqualDeep(@as([]const u8, ""), buf.items);
    }
    test "parser: discard: one by one" {
        const str =
            \\progress: 42.8,
            \\active: true,
            \\name: "final",
            \\some_field_that_doesnt_exist: DoesntExist(
            \\    neither_does_this: [true, false, false],
            \\    yes: 42,
            \\),
            \\rendering: (
            \\    msaa: true,
            \\    fov: 666.42,
            \\),
            \\player: Player(
            \\    pos: (-10, 30, -200),
            \\)
        ;

        var tokens = &Lexer.init(str).token_stream;
        var parser = Deserializer.init(testing.allocator);

        {
            const expected: []const u8 = "progress: 42.8,";
            const discarded = try parser.discardValue(tokens);
            try expectEqualDeep(expected, str[discarded.start_idx..discarded.end_idx]);
        }
        {
            const expected: []const u8 = "\nactive: true,";
            const discarded = try parser.discardValue(tokens);
            try expectEqualDeep(expected, str[discarded.start_idx..discarded.end_idx]);
        }
        {
            const expected: []const u8 = "\nname: \"final\",";
            const discarded = try parser.discardValue(tokens);
            try expectEqualDeep(expected, str[discarded.start_idx..discarded.end_idx]);
        }
        {
            const expected: []const u8 =
                \\
                \\some_field_that_doesnt_exist: DoesntExist(
                \\    neither_does_this: [true, false, false],
                \\    yes: 42,
                \\),
            ;
            const discarded = try parser.discardValue(tokens);
            try expectEqualDeep(expected, str[discarded.start_idx..discarded.end_idx]);
        }
        {
            const expected: []const u8 =
                \\
                \\rendering: (
                \\    msaa: true,
                \\    fov: 666.42,
                \\),
            ;
            const discarded = try parser.discardValue(tokens);
            try expectEqualDeep(expected, str[discarded.start_idx..discarded.end_idx]);
        }
        {
            const expected: []const u8 =
                \\
                \\player: Player(
                \\    pos: (-10, 30, -200),
                \\)
            ;
            const discarded = try parser.discardValue(tokens);
            try expectEqualDeep(expected, str[discarded.start_idx..discarded.end_idx]);
        }

        var buf = std.ArrayList(u8).init(testing.allocator);
        try tokens.dump(TokenDumpOptions.minify(), buf.writer());
        try expectEqualDeep(@as([]const u8, ""), buf.items);
    }

    // ---

    // Boolean:
    // bool = "true" | "false";

    pub fn parseBool(self: *Self, tokens: *TokenStream) Error!bool {
        const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;

        switch (tok) {
            .Identifier => |ident| {
                if (std.mem.eql(u8, ident, "true")) return true;
                if (std.mem.eql(u8, ident, "false")) return false;
                try self.err.writer().print(
                    "expected `true` or `false`, found `{s}` instead",
                    .{ident},
                );
                return error.UnexpectedValue;
            },
            else => {
                try self.err.writer().print(
                    "expected boolean literal, found `{}` instead",
                    .{tok},
                );
                return error.UnexpectedToken;
            },
        }
    }

    test "parser: bool" {
        try parseAndExpect(bool, true, "true");
        try parseAndExpect(bool, false, "false");

        // zig fmt: off
        try parseAndExpectError(bool, error.UnexpectedValue, "nope",
            "expected `true` or `false`, found `nope` instead");
        try parseAndExpectError(bool, error.UnexpectedToken, "42",
            "expected boolean literal, found `42` instead");
        try parseAndExpectError(bool, error.UnexpectedEndOfRon, "  ", "");
        // zig fmt: on
    }

    // Numbers:
    // digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
    // hex_digit = "A" | "a" | "B" | "b" | "C" | "c" | "D" | "d" | "E" | "e" | "F" | "f";
    // unsigned = (["0", ("b" | "o")], digit, { digit | '_' } |
    //              "0x", (digit | hex_digit), { digit | hex_digit | '_' });
    // signed = ["+" | "-"], unsigned;
    // float = float_std | float_frac;
    // float_std = ["+" | "-"], digit, { digit }, ".", {digit}, [float_exp];
    // float_frac = ".", digit, {digit}, [float_exp];
    // float_exp = ("e" | "E"), digit, {digit};

    pub fn parseFloat(self: *Self, comptime Float: type, tokens: *TokenStream) Error!Float {
        const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;

        switch (tok) {
            .Number => |str| {
                const n = std.fmt.parseFloat(Float, str) catch |err| {
                    try self.err.writer().print(
                        "expected valid float literal, found `{s}` instead ({})",
                        .{ str, err },
                    );
                    return error.UnexpectedValue;
                };
                return n;
            },
            else => {
                try self.err.writer().print(
                    "expected float literal, found `{}` instead",
                    .{tok},
                );
                return error.UnexpectedToken;
            },
        }
    }

    test "parser: floats" {
        try parseAndExpect(f16, 0.666, "+0.666");
        try parseAndExpect(f32, 0.666, "+0.666");
        try parseAndExpect(f64, 0.666, "+0.666");
        // try parseAndExpect(f128, 0.666, "0.666"); // TODO: fails somehow
        try parseAndExpect(f16, -0.666, "-0.666");
        try parseAndExpect(f32, -0.666, "-0.666");
        try parseAndExpect(f64, -0.666, "-0.666");
        // try parseAndExpect(f128, -0.666, "-0.666"); // TODO: fails somehow

        // zig fmt: off
        try parseAndExpectError(f32, error.UnexpectedValue, "42.666.69",
            "expected valid float literal, found `42.666.69` instead (error.InvalidCharacter)");
        try parseAndExpectError(f32, error.UnexpectedToken, "nope",
            "expected float literal, found `nope` instead");
        try parseAndExpectError(f32, error.UnexpectedEndOfRon, "  ", "");
        // zig fmt: on
    }

    pub fn parseInt(self: *Self, comptime Int: type, tokens: *TokenStream) Error!Int {
        const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;

        switch (tok) {
            .Number => |str| {
                const n = std.fmt.parseInt(Int, str, 0) catch |err| {
                    try self.err.writer().print(
                        "expected valid integer literal, found `{s}` instead ({})",
                        .{ str, err },
                    );
                    return error.UnexpectedValue;
                };
                return n;
            },
            else => {
                try self.err.writer().print(
                    "expected integer literal, found `{}` instead",
                    .{tok},
                );
                return error.UnexpectedToken;
            },
        }
    }

    test "parser: integers" {
        try parseAndExpect(i2, 1, "1");
        try parseAndExpect(i31, 1, "1");
        try parseAndExpect(i32, 1, "1");
        try parseAndExpect(i33, 1, "1");

        try parseAndExpect(i2, -1, "-1");
        try parseAndExpect(i31, -1, "-1");
        try parseAndExpect(i32, -1, "-1");
        try parseAndExpect(i33, -1, "-1");

        try parseAndExpect(u1, 1, "1");
        try parseAndExpect(u31, 1, "1");
        try parseAndExpect(u32, 1, "1");
        try parseAndExpect(u33, 1, "1");

        try parseAndExpect(i64, 0x507, "0x507");
        try parseAndExpect(i64, 0x1A5, "0x1A5");
        try parseAndExpect(i64, 0x53C537, "0x53C537");

        try parseAndExpect(i32, 0b101, "0b101");
        try parseAndExpect(i32, 0b001, "0b001");
        try parseAndExpect(i32, 0b100100, "0b100100");

        try parseAndExpect(u17, 0o1461, "0o1461");
        try parseAndExpect(u17, 0o051, "0o051");
        try parseAndExpect(u17, 0o150700, "0o150700");

        // zig fmt: off
        try parseAndExpectError(i32, error.UnexpectedValue, "1-1-1",
            "expected valid integer literal, found `1-1-1` instead (error.InvalidCharacter)");
        try parseAndExpectError(i32, error.UnexpectedToken, "nope",
            "expected integer literal, found `nope` instead");
        try parseAndExpectError(i32, error.UnexpectedEndOfRon, "  ", "");
        // zig fmt: on
    }

    // Strings:
    // string = string_std | string_raw;
    // string_std = "\"", { no_double_quotation_marks | string_escape }, "\"";
    // string_escape = "\\", ("\"" | "\\" | "b" | "f" | "n" | "r" | "t" | ("u", unicode_hex));
    // string_raw = "r" string_raw_content;
    // string_raw_content = ("#", string_raw_content, "#") | "\"", { unicode_non_greedy }, "\"";

    pub fn parseString(self: *Self, tokens: *TokenStream) Error![]const u8 {
        const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;
        const quoted = switch (tok) {
            .DoubleQuoted => |quoted| quoted,
            .RawQuoted => |rstr| return rstr.str,
            else => {
                try self.err.writer().print(
                    "expected (raw) string literal, found `{s}` instead",
                    .{tok},
                );
                return error.UnexpectedToken;
            },
        };

        // found some escapes!
        if (quoted.decoded_len != quoted.str.len) {
            var dst = try self.arena.allocator.alloc(u8, quoted.decoded_len);
            errdefer self.arena.allocator.free(dst);

            const n = unescape(dst, quoted.str) catch |err| {
                try self.err.writer().print(
                    "illegal escape sequence: `{s}` ({})",
                    .{ quoted.str, err },
                );
                return error.IllegalEscapeSequence;
            };

            return dst[0..n];
        }

        return quoted.str;
    }

    test "parser: strings: double-quoted" {
        { // empty
            const expected = "";
            const str = "\"" ++ expected ++ "\"";
            try parseAndExpect([]const u8, expected, str);
        }

        { // ascii
            const expected = "hello world";
            const str = "\"" ++ expected ++ "\"";
            try parseAndExpect([]const u8, expected, str);
        }

        { // unicode
            const expected = "わかりません";
            const str = "\"" ++ expected ++ "\"";
            try parseAndExpect([]const u8, expected, str);
        }

        { // escapes
            const expected = "\\ \t \" \n \r \t \x08 \x0C \x07 \u{07}";
            const str =
                \\\\ \t \" \n \r \t \x08 \x0C \x07 \u{07}
            ;
            const quoted = "\"" ++ str ++ "\"";

            try parseAndExpect([]const u8, expected, quoted);
        }

        { // some more escapes
            const expected = "\"hello world\"\r\n ... \u{65}\r\ndon't\u{0}break\u{0}me!\"";
            const str =
                \\\"hello world\"\r\n ... \u{65}\r\ndon't\u{0}break\u{0}me!\"
            ;
            const quoted = "\"" ++ str ++ "\"";

            try parseAndExpect([]const u8, expected, quoted);
        }

        // zig fmt: off
        try parseAndExpectError([]const u8, error.UnexpectedToken, "unquoted",
            "expected (raw) string literal, found `unquoted` instead");
        try parseAndExpectError([]const u8, error.IllegalEscapeSequence,
            "\"hello \\xx world\"",
            "illegal escape sequence: `hello \\xx world` (error.InvalidCharacter)");
        try parseAndExpectError([]const u8, error.UnbalancedDoubleQuotes, "\"hello ", "");
        // zig fmt: on
    }
    test "parser: strings: raw" {
        { // empty
            const expected = "";
            const str = "r##\"" ++ expected ++ "\"##";
            try parseAndExpect([]const u8, expected, str);
        }

        { // ascii
            const expected = "hello world";
            const str = "r##\"" ++ expected ++ "\"##";
            try parseAndExpect([]const u8, expected, str);
        }

        { // unicode
            const expected = "わかりません";
            const str = "r##\"" ++ expected ++ "\"##";
            try parseAndExpect([]const u8, expected, str);
        }

        { // escapes
            const raw =
                \\\\ \t \" \n \r \t \b \f \x07 \u{07}
            ;
            const expected = raw;
            const str = "r##\"" ++ raw ++ "\"##";

            try parseAndExpect([]const u8, expected, str);
        }

        { // some more escapes
            const raw =
                \\\"hello world\"\r\n ... \u{65}\r\ndon't\u{0}break\u{0}me!\"
            ;
            const expected = raw;
            const str = "r##\"" ++ raw ++ "\"##";

            try parseAndExpect([]const u8, expected, str);
        }
    }

    // Chars:
    // char = "'", (no_apostrophe | "\\\\" | "\\'"), "'";

    pub fn parseChar(self: *Self, tokens: *TokenStream) Error!Char {
        const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;
        const quoted = switch (tok) {
            .SingleQuoted => |quoted| quoted,
            else => {
                try self.err.writer().print(
                    "expected character literal, found `{s}` instead",
                    .{tok},
                );
                return error.UnexpectedToken;
            },
        };

        if (quoted.decoded_len == 0 or quoted.decoded_len > 4) {
            try self.err.writer().print(
                "character literal must contain 1 to 4 bytes of UTF-8 data: `{s}` (got {})",
                .{ quoted.str, quoted.decoded_len },
            );
            return error.IllegalUtf8Data;
        }

        const decodeChar = struct {
            fn func(parser: *Self, s: []const u8) !Char {
                return Char.fromUtf8Slice(s) catch |err| {
                    try parser.err.writer().print(
                        "character literal contains invalid UTF-8 data: `{s}` ({})",
                        .{ s, err },
                    );
                    return error.IllegalUtf8Data;
                };
            }
        }.func;

        // found some escapes!
        if (quoted.decoded_len != quoted.str.len) {
            var dst: [4]u8 = undefined;
            const n = unescape(&dst, quoted.str) catch |err| {
                try self.err.writer().print(
                    "illegal escape sequence: `{s}` ({})",
                    .{ quoted.str, err },
                );
                return error.IllegalEscapeSequence;
            };

            return decodeChar(self, dst[0..n]);
        }

        const n = std.unicode.utf8ByteSequenceLength(quoted.str[0]) catch |err| {
            try self.err.writer().print(
                "character literal contains invalid UTF-8 data: `{s}` ({})",
                .{ quoted.str, err },
            );
            return error.IllegalUtf8Data;
        };
        return decodeChar(self, quoted.str[0..n]);
    }

    test "parser: strings: single-quoted aka. char" {
        try parseAndExpect(Char, Char.fromComptimeInt(' '), "' '");
        try parseAndExpect(Char, Char.fromComptimeInt('か'), "'か'");
        try parseAndExpect(Char, Char.fromComptimeInt('\\'), "'\\\\'");
        try parseAndExpect(Char, Char.fromComptimeInt('\t'), "'\\t'");
        try parseAndExpect(Char, Char.fromComptimeInt('\x08'), "'\\x08'");
        try parseAndExpect(Char, Char.fromComptimeInt('\u{07}'), "'\\u{07}'");

        // zig fmt: off
        try parseAndExpectError(Char, error.IllegalUtf8Data, "''",
            "character literal must contain 1 to 4 bytes of UTF-8 data: `` (got 0)");
        try parseAndExpectError(Char, error.IllegalUtf8Data,
            "'aaaaaaaaaaaaaaaaaaa'",
            "character literal must contain 1 to 4 bytes of UTF-8 data: `aaaaaaaaaaaaaaaaaaa` (got 19)");
        try parseAndExpectError(Char, error.UnexpectedToken, "c",
            "expected character literal, found `c` instead");
        // this will break lexer even before it gets to us
        try parseAndExpectError(Char, error.UnterminatedEscapeSequence, "'\\u{FFFFFF}'", "");
        try parseAndExpectError(Char, error.UnbalancedSingleQuotes, "'c", "");
        // zig fmt: on
    }

    // Optionals:
    // option = "Some", ws, "(", ws, value, ws, ")";

    pub fn parseOption(self: *Self, comptime T: type, tokens: *TokenStream) Error!?T {
        // Some|None
        blk: {
            const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;
            switch (tok) {
                .Identifier => |ident| {
                    if (std.mem.eql(u8, ident, "Some")) {
                        break :blk;
                    } else if (std.mem.eql(u8, ident, "None")) {
                        return null;
                    }
                },
                else => {},
            }
            try self.err.writer().print(
                "expected `Some` or `None` literal, found `{s}` instead",
                .{tok},
            );
            return error.UnexpectedToken;
        }

        _ = try self.expectToken(tokens, .OpenParen);
        const res = try self.parse(T, tokens);
        _ = try self.expectToken(tokens, .CloseParen);

        return res;
    }

    test "parser: option" {
        try parseAndExpect(?bool, true, "Some(true)");
        try parseAndExpect(?bool, false, "Some(false)");
        try parseAndExpect(?bool, null, "None");

        try parseAndExpect(???bool, true, "Some(Some(Some(true)))");
        try parseAndExpect(???bool, false, "Some(Some(Some(false)))");
        try parseAndExpect(???bool, null, "None");

        {
            // const l1: ?bool = null;
            const l2: ??bool = null;
            const l3: ???bool = l2;
            try parseAndExpect(???bool, l3, "Some(None)");
        }
        {
            const l1: ?bool = null;
            const l2: ??bool = l1;
            const l3: ???bool = l2;
            try parseAndExpect(???bool, l3, "Some(Some(None))");
        }

        // zig fmt: off
        try parseAndExpectError(???bool, error.UnexpectedToken,
            "pouet",
            "expected `Some` or `None` literal, found `pouet` instead");
        try parseAndExpectError(???bool, error.UnexpectedToken,
            "Some[true]",
            "expected `(`, found `[` instead");
        try parseAndExpectError(???bool, error.UnexpectedToken,
            "Some(None(Some(false)))",
            "expected `)`, found `(` instead");
        try parseAndExpectError(???bool, error.UnexpectedEndOfRon, "Some(", "");
        // zig fmt: on
    }

    // Lists:
    // list = "[", [value, { comma, value }, [comma]], "]";

    pub fn parseOwnedSlice(
        self: *Self,
        comptime Elem: type,
        tokens: *TokenStream,
    ) Error![]Elem {
        return (try self.parseArrayList(std.ArrayList(Elem), tokens)).toOwnedSlice();
    }

    pub fn parseArrayList(
        self: *Self,
        comptime List: type,
        tokens: *TokenStream,
    ) Error!List {
        _ = try self.expectToken(tokens, .OpenBracket);

        const Elem = ArrayListType(List);
        var list = List.init(&self.arena.allocator);
        errdefer list.deinit();

        values: while (true) {
            try self.discardWhitespace(tokens);
            switch ((try tokens.peek()) orelse return error.UnexpectedEndOfRon) {
                .CloseBracket => { // empty list
                    _ = try self.next(tokens);
                    break :values;
                },
                else => {
                    try list.append((try self.parse(Elem, tokens)));

                    // , or ]
                    try self.discardWhitespace(tokens);
                    const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;
                    switch (tok) {
                        .Comma => {},
                        .CloseBracket => return list,
                        else => {
                            try self.err.writer().print(
                                "expected either `,` or `]`, found `{}` instead",
                                .{tok},
                            );
                            return error.UnexpectedToken;
                        },
                    }
                },
            }
        }

        return list;
    }

    test "parser: lists: simple" {
        var zero = [_]bool{};
        var t = [_]bool{true};
        var f = [_]bool{false};
        var tf = [_]bool{ true, false };

        try parseAndExpect([]bool, &zero, "[]");
        try parseAndExpect([]bool, &t, "[true]");
        try parseAndExpect([]bool, &f, "[false]");
        try parseAndExpect([]bool, &tf, "[true, false,]");

        var zeros = std.ArrayList(bool).fromOwnedSlice(testing.failing_allocator, &zero);
        try parseAndExpect(std.ArrayList(bool), zeros, "[]");
        var tfs = std.ArrayList(bool).fromOwnedSlice(testing.failing_allocator, &tf);
        try parseAndExpect(std.ArrayList(bool), tfs, "[true, false,]");
    }
    test "parser: lists: nested" {
        var zero = [_][]bool{};
        var zero_zero = [_][]bool{&[_]bool{}};
        var tf = [_]bool{ true, false };
        var ft = [_]bool{ false, true };
        var tf_ft = [_][]bool{ &tf, &ft };

        try parseAndExpect([][]bool, &zero, "[]");
        try parseAndExpect([][]bool, &zero_zero, "[[]]");
        try parseAndExpect([][]bool, &tf_ft, "[[true, false], [false, true]]");

        const NestedList = std.ArrayList(std.ArrayList(bool));

        var zeros = NestedList.init(testing.failing_allocator);
        try parseAndExpect(NestedList, zeros, "[]");

        var zero_zeros = NestedList.fromOwnedSlice(
            testing.failing_allocator,
            &[_]std.ArrayList(bool){
                std.ArrayList(bool).init(testing.failing_allocator),
            },
        );
        try parseAndExpect(NestedList, zero_zeros, "[[]]");

        var tf_fts = NestedList.fromOwnedSlice(
            testing.failing_allocator,
            &[_]std.ArrayList(bool){
                std.ArrayList(bool).fromOwnedSlice(testing.failing_allocator, &tf),
                std.ArrayList(bool).fromOwnedSlice(testing.failing_allocator, &ft),
            },
        );
        try parseAndExpect(NestedList, tf_fts, "[[true, false], [false, true]]");
    }
    test "parser: lists: errors" {
        // zig fmt: off
        try parseAndExpectError([]bool, error.UnexpectedToken,
            "[ true true ]",
            "expected either `,` or `]`, found `true` instead");
        try parseAndExpectError([]bool, error.UnexpectedToken,
            "[true,,]",
            "expected boolean literal, found `,` instead");
        try parseAndExpectError([]bool, error.UnexpectedEndOfRon, "[true,", "");
        // zig fmt: on
    }

    // Maps:
    // map = "{", [map_entry, { comma, map_entry }, [comma]], "}";
    // map_entry = value, ws, ":", ws, value;

    pub fn parseHashMap(
        self: *Self,
        comptime Map: type,
        tokens: *TokenStream,
    ) Error!Map {
        _ = try self.expectToken(tokens, .OpenBrace);

        const KV = hashMapTypes(Map);
        var map = Map.init(&self.arena.allocator);
        errdefer map.deinit();

        const parseKV = struct {
            fn func(
                _map: *Map,
                _self: *Self,
                _tokens: *TokenStream,
            ) Error!void {
                const k = try _self.parse(KV.K, _tokens);
                _ = try _self.expectToken(_tokens, .Colon);
                const v = try _self.parse(KV.V, _tokens);
                try _map.put(k, v);
            }
        }.func;

        values: while (true) {
            try self.discardWhitespace(tokens);
            switch ((try tokens.peek()) orelse return error.UnexpectedEndOfRon) {
                .CloseBrace => { // empty map
                    _ = try self.next(tokens);
                    break :values;
                },
                else => {
                    try parseKV(&map, self, tokens);

                    // , or }
                    try self.discardWhitespace(tokens);
                    const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;
                    switch (tok) {
                        .Comma => {},
                        .CloseBrace => return map,
                        else => {
                            try self.err.writer().print(
                                "expected either `,` or `}}`, found `{}` instead",
                                .{tok},
                            );
                            return error.UnexpectedToken;
                        },
                    }
                },
            }
        }

        return map;
    }

    test "parser: maps: simple" {
        {
            const Map = std.AutoHashMap(u32, bool);
            var expected = Map.init(testing.allocator);
            defer expected.deinit();

            const str = "{}";
            try parseAndExpect(Map, expected, str);
        }

        {
            const Map = std.AutoArrayHashMap(u32, bool);
            var expected = Map.init(testing.allocator);
            defer expected.deinit();
            try expected.put(42, true);
            try expected.put(666, false);

            const str =
                \\{
                \\  42: true,
                \\  666: false,
                \\}
            ;
            try parseAndExpect(Map, expected, str);
        }
    }
    test "parser: maps: string keys" {
        {
            const Map = std.StringHashMap(u32);
            var expected = Map.init(testing.allocator);
            defer expected.deinit();
            try expected.put("hello", 42);
            try expected.put("world", 777);

            const str =
                \\{
                \\  "hello": 42,
                \\  "world": 777,
                \\}
            ;
            try parseAndExpect(Map, expected, str);
        }
    }
    test "parser: maps: nested" {
        {
            const Map = std.AutoHashMap(u32, std.AutoHashMap(i2, []const u8));
            var expected = Map.init(testing.allocator);
            defer expected.deinit();

            const str = "{}";
            try parseAndExpect(Map, expected, str);
        }

        {
            const Inner = std.AutoHashMap(i2, []const u8);
            const Map = std.AutoHashMap(u32, Inner);

            var expected_inner = Inner.init(testing.allocator);
            defer expected_inner.deinit();
            var expected = Map.init(testing.allocator);
            defer expected.deinit();
            try expected.put(42, expected_inner);

            const str = "{42:{}}";
            try parseAndExpect(Map, expected, str);
        }

        {
            const Inner = std.AutoHashMap(u2, []const u8);
            const Map = std.AutoHashMap(u32, Inner);

            var expected_inner = Inner.init(testing.allocator);
            try expected_inner.put(0, "Hello");
            try expected_inner.put(1, ", ");
            try expected_inner.put(2, "world");
            try expected_inner.put(3, "!");
            defer expected_inner.deinit();
            var expected = Map.init(testing.allocator);
            defer expected.deinit();
            try expected.put(42, expected_inner);
            try expected.put(777, expected_inner);

            const str =
                \\{
                \\  42: {
                \\      0: "Hello",
                \\      1: ", ",
                \\      2: "world",
                \\      3: "!",
                \\  },
                \\  777: { 0: "Hello", 1: ", ", 2: "world", 3: "!" },
                \\}
            ;
            try parseAndExpect(Map, expected, str);
        }
    }

    // Tuples:
    // tuple = "(", [value, { comma, value }, [comma]], ")";

    pub fn parseTuple(
        self: *Self,
        comptime Tuple: type,
        tokens: *TokenStream,
    ) Error!Tuple {
        _ = try self.expectToken(tokens, .OpenParen);

        const fields = @typeInfo(Tuple).Struct.fields;
        var res: Tuple = undefined;
        {
            inline for (fields) |field, i| {
                res[i] = try self.parse(field.field_type, tokens);

                if (comptime i < fields.len - 1)
                    _ = try self.expectToken(tokens, .Comma);
            }
        }

        _ = self.expectToken(tokens, .Comma) catch {};
        _ = try self.expectToken(tokens, .CloseParen);

        return res;
    }

    test "parser: tuples: simple" {
        {
            const Tuple = std.meta.Tuple(&.{});
            const expected: Tuple = .{};
            const s = "()";
            try parseAndExpect(Tuple, expected, s);
        }
        {
            const Tuple = std.meta.Tuple(&.{});
            const expected: Tuple = .{};
            const s = "(,)";
            try parseAndExpect(Tuple, expected, s);
        }
        {
            const Tuple = std.meta.Tuple(&.{ bool, i2, []const u8 });
            const expected: Tuple = .{ true, -1, "coucou" };
            const s = "(true, -1, \"coucou\")";
            try parseAndExpect(Tuple, expected, s);
        }
        {
            const Tuple = std.meta.Tuple(&.{ bool, i2, []const u8 });
            const expected: Tuple = .{ true, -1, "coucou" };
            const s = "(true, -1, \"coucou\",)";
            try parseAndExpect(Tuple, expected, s);
        }
    }
    test "parser: tuples: nested" {
        {
            const Tuple = std.meta.Tuple(&.{
                std.meta.Tuple(&.{
                    std.meta.Tuple(&.{}),
                    std.meta.Tuple(&.{}),
                }),
            });
            const expected: Tuple = .{.{ .{}, .{} }};
            const s = "(((),(),),)";
            try parseAndExpect(Tuple, expected, s);
        }
        {
            const Tuple = std.meta.Tuple(&.{
                bool, i2, std.meta.Tuple(&.{ []const u8, []const u8 }),
            });
            const expected: Tuple = .{ true, -1, .{ "hello", "world" } };
            const s =
                \\(
                \\  true, -1, (
                \\      "hello", "world",
                \\  ),
                \\)
            ;
            try parseAndExpect(Tuple, expected, s);
        }
    }

    // Structs:
    // struct = unit_struct | tuple_struct | named_struct;
    // unit_struct = ident | "()";
    // tuple_struct = [ident], ws, tuple;
    // named_struct = [ident], ws, "(", [named_field, { comma, named_field }, [comma]], ")";
    // named_field = ident, ws, ":", value;

    // NOTE: `parseNamedStruct` & `parseAnonStruct` are misnomers: they do not
    // refer to whether the struct itself is named or not, but rather whether
    // _its fields_ are named or not (RON allows describing a struct value
    // without specifying field names, i.e. tuple coercion).

    // TODO: customizable behaviour for unknown fields
    pub fn parseStruct(
        self: *Self,
        comptime Struct: type,
        tokens: *TokenStream,
    ) Error!Struct {
        const ident = (try tokens.peek()) orelse return error.UnexpectedEndOfRon;
        if (ident == .Identifier) _ = self.next(tokens) catch unreachable;
        // TODO: check that name of Struct matches ident (tricky)

        _ = try self.expectToken(tokens, .OpenParen);
        try self.discardWhitespace(tokens);

        const tok = (try self.next(tokens)) orelse return error.UnexpectedEndOfRon;

        var res: Struct = undefined;
        const fields = @typeInfo(Struct).Struct.fields;
        var fields_seen = [_]bool{false} ** fields.len;

        if (tok != .CloseParen) { // not an empty struct
            const peeked = (try tokens.peek()) orelse return error.UnexpectedEndOfRon;
            TokenStream.refill(tokens, tok);

            const trailing_comma = switch (peeked) {
                .Colon => try self.parseNamedStruct(Struct, &res, &fields_seen, tokens),
                else => try self.parseAnonStruct(Struct, &res, &fields_seen, tokens),
            };

            if (!trailing_comma) {
                const comma = (try tokens.peek()) orelse return error.UnexpectedEndOfRon;
                if (comma == .Comma) _ = self.next(tokens) catch unreachable;
            }
            _ = try self.expectToken(tokens, .CloseParen);
        }

        inline for (fields) |field, i| {
            if (!fields_seen[i]) {
                if (field.default_value) |default| {
                    if (!field.is_comptime) {
                        @field(res, field.name) = default;
                    }
                } else {
                    try self.err.writer().print(
                        "missing field `{s}.{s}` without default value",
                        .{ @typeName(Struct), field.name },
                    );
                    return error.StructMissingField;
                }
            }
        }

        return res;
    }
    fn parseNamedStruct(
        self: *Self,
        comptime Struct: type,
        res: *Struct,
        fields_seen: []bool,
        tokens: *TokenStream,
    ) Error!bool {
        const fields = @typeInfo(Struct).Struct.fields;
        while (true) {
            const field_ident = try self.expectToken(tokens, .Identifier);
            _ = try self.expectToken(tokens, .Colon);

            // Use comptime loop unrolling to generate what pretty
            // much amounts to a big fat switch minus the jump table:
            // one case for each field of the given struct type.
            //
            // TODO: can we avoid the unnecessary comparisons here?
            // Maybe https://github.com/ziglang/zig/issues/7224 ?
            // Also check out `std.meta.stringToEnum` maybe?
            var found = false;
            inline for (fields) |field, i| {
                if (std.mem.eql(u8, field.name, field_ident)) {
                    found = true;
                    fields_seen[i] = true;
                    @field(res, field.name) = try self.parse(field.field_type, tokens);
                }
            }

            if (!found) {
                _ = try self.discardValue(tokens);
                TokenStream.refill(tokens, Token.Comma);
            }

            try self.discardWhitespace(tokens);
            const comma = (try tokens.peek()) orelse return error.UnexpectedEndOfRon;
            if (comma != .Comma) return false;
            _ = self.next(tokens) catch unreachable;

            try self.discardWhitespace(tokens);
            const rparen = (try tokens.peek()) orelse return error.UnexpectedEndOfRon;
            if (rparen == .CloseParen) return true;
        }
    }
    fn parseAnonStruct(
        self: *Self,
        comptime Struct: type,
        res: *Struct,
        fields_seen: []bool,
        tokens: *TokenStream,
    ) Error!bool {
        const fields = @typeInfo(Struct).Struct.fields;

        inline for (fields) |field, i| {
            fields_seen[i] = true;
            @field(res, field.name) = try self.parse(field.field_type, tokens);

            try self.discardWhitespace(tokens);
            const comma = (try tokens.peek()) orelse return error.UnexpectedEndOfRon;
            if (comma != .Comma) return false;
            _ = self.next(tokens) catch unreachable;

            try self.discardWhitespace(tokens);
            const rparen = (try tokens.peek()) orelse return error.UnexpectedEndOfRon;
            if (rparen == .CloseParen) return true;
        }

        return true;
    }

    test "parser: structs" {
        {
            const Struct = struct {};
            const expected: Struct = .{};
            const s = "()";
            try parseAndExpect(Struct, expected, s);
        }
        {
            const NamedStruct = struct {};
            const expected: NamedStruct = .{};
            const s = "NamedStruct()";
            try parseAndExpect(NamedStruct, expected, s);
        }

        {
            const Struct = struct {
                bool: bool,
                char: Char,
                integer: i64,
                float: f64,
                string: []const u8,
                option1: ?i3,
                option2: ?i3,
                unit: Unit,
                seq: std.ArrayList(@This()),
                map: std.StringArrayHashMap(@This()),
            };
            const child1: Struct = .{
                .bool = false,
                .char = Char.fromComptimeInt('1'),
                .integer = 1,
                .float = 1.1,
                .string = "child 1",
                .option1 = 1,
                .option2 = null,
                .unit = Unit{},
                .seq = std.ArrayList(Struct).init(testing.allocator),
                .map = std.StringArrayHashMap(Struct).init(testing.allocator),
            };
            const child2: Struct = .{
                .bool = false,
                .char = Char.fromComptimeInt('2'),
                .integer = 2,
                .float = 2.2,
                .string = "child 2",
                .option1 = null,
                .option2 = 2,
                .unit = Unit{},
                .seq = std.ArrayList(Struct).init(testing.allocator),
                .map = std.StringArrayHashMap(Struct).init(testing.allocator),
            };

            const children_seq = std.ArrayList(Struct).fromOwnedSlice(
                testing.allocator,
                &[_]Struct{ child1, child2 },
            );
            _ = children_seq;
            var children_map = blk: {
                var map = std.StringArrayHashMap(Struct).init(testing.allocator);
                errdefer map.deinit();

                try map.put("child 1", child1);
                try map.put("child 2", child2);

                break :blk map;
            };
            defer children_map.deinit();

            const expected: Struct = .{
                .bool = true,
                .char = Char.fromComptimeInt('X'),
                .integer = 42,
                .float = 666.111,
                .string = "coucou",
                .option1 = 1,
                .option2 = null,
                .unit = Unit{},
                // .seq = std.ArrayList(Struct).init(testing.allocator),
                // .map = std.StringArrayHashMap(Struct).init(testing.allocator),
                .seq = children_seq,
                .map = children_map,
            };

            const s =
                \\Struct(
                \\  bool: true,
                \\  char: 'X',
                \\  integer: 42,
                \\  float: 666.111,
                \\  string: "coucou",
                \\  option1: Some(1),
                \\  option2: None,
                \\  unit: (),
                \\  seq: [
                \\    (false, '1', 1, 1.1, "child 1", Some(1), None, (), [], {},),
                \\    (false, '2', 2, 2.2, "child 2", None, Some(2), (), [], {}),
                \\  ],
                \\  map: {
                \\    "child 1": (
                \\      bool: false, char: '1', integer: 1, float: 1.1,
                \\      string: "child 1", option1: Some(1), option2: None,
                \\      unit: (), seq: [], map: {}
                \\    ),
                \\    "child 2": (
                \\      bool: false, char: '2', integer: 2, float: 2.2,
                \\      string: "child 2", option1: None, option2: Some(2),
                \\      unit: (), seq: [], map: {}
                \\    )
                \\  },
                \\)
            ;
            try parseAndExpect(Struct, expected, s);
        }
    }
    test "parser: structs: missing fields" {
        {
            const Struct = struct { a: bool, b: i32 };
            try parseAndExpectError(
                Struct,
                error.StructMissingField,
                "()",
                "missing field `Struct.a` without default value",
            );
        }
        {
            const Struct = struct { a: bool = true, b: i32 };
            try parseAndExpectError(
                Struct,
                error.StructMissingField,
                "()",
                "missing field `Struct.b` without default value",
            );
        }
        {
            const Struct = struct { a: bool, b: i32 };
            const strs = [_][]const u8{
                "(a: true)",
                "(a: true,)",
                "(true)",
                "(true,)",
            };
            for (strs) |str| {
                try parseAndExpectError(
                    Struct,
                    error.StructMissingField,
                    str,
                    "missing field `Struct.b` without default value",
                );
            }
        }
        {
            const Struct = struct { a: bool = true, b: i32 = 42 };
            const expected = Struct{ .a = true, .b = 42 };
            try parseAndExpect(Struct, expected, "()");
        }
    }

    // Enums & tagged unions:
    // enum_variant = enum_variant_unit | enum_variant_tuple | enum_variant_named;
    // enum_variant_unit = ident;
    // enum_variant_tuple = ident, ws, tuple;
    // enum_variant_named = ident, ws, "(", [named_field, { comma, named_field }, [comma]], ")";

    pub fn parseEnum(
        self: *Self,
        comptime Enum: type,
        tokens: *TokenStream,
    ) Error!Enum {
        const fields = @typeInfo(Enum).Enum.fields;

        // TODO: check out `std.meta.stringToEnum` maybe?
        const ident = try self.expectToken(tokens, .Identifier);
        inline for (fields) |field| {
            if (std.mem.eql(u8, field.name, ident)) {
                return @field(Enum, field.name);
            }
        }

        try self.err.writer().print(
            "expected valid enum tag, found `{s}` instead",
            .{ident},
        );
        return error.UnexpectedValue;
    }

    test "parser: enums: simple" {
        const Enum = enum { a, b, c };
        try parseAndExpect(Enum, Enum.a, "a");
        try parseAndExpect(Enum, Enum.b, "b");
        try parseAndExpect(Enum, Enum.c, "c");
        try parseAndExpectError(
            Enum,
            error.UnexpectedValue,
            "d",
            "expected valid enum tag, found `d` instead",
        );
    }
    test "parser: enums: tagged unions" {
        const Enum = union(enum) {
            a: i32,
            b: []const u8,
            c: struct { x: f32, y: f32 },
        };
        try parseAndExpect(Enum, Enum{ .a = 42 }, "a(42)");
        try parseAndExpect(Enum, Enum{ .b = "coucou" }, "b(\"coucou\")");

        try parseAndExpect(
            Enum,
            Enum{ .c = .{ .x = 42.0, .y = 666.111 } },
            "c((42.0, 666.111))",
        );
        try parseAndExpect(
            Enum,
            Enum{ .c = .{ .x = 42.0, .y = 666.111 } },
            "c(DoesntMatter(42.0, 666.111))",
        );
        try parseAndExpect(
            Enum,
            Enum{ .c = .{ .x = 42.0, .y = 666.111 } },
            "c((x: 42.0, y: 666.111))",
        );
        try parseAndExpect(
            Enum,
            Enum{ .c = .{ .x = 42.0, .y = 666.111 } },
            "c(DoesntMatter(x: 42.0, y: 666.111))",
        );

        try parseAndExpectError(
            Enum,
            error.UnexpectedToken,
            "c(42.0, 666.111)",
            "expected `(`, found `42.0` instead",
        );
        try parseAndExpectError(
            Enum,
            error.UnexpectedValue,
            "d",
            "expected valid enum tag, found `d` instead",
        );
    }
};

// ---

fn parseAndExpect(comptime T: type, expected: T, data: []const u8) !void {
    try parseAndExpectRest(T, expected, "", data);
}

fn parseAndExpectRest(
    comptime T: type,
    expected: Error!T,
    expected_rest: []const u8,
    data: []const u8,
) !void {
    var tokens = &Lexer.init(data).token_stream;
    var parser = Deserializer.init(testing.allocator);
    defer parser.deinit();
    defer parser.parseFree();
    errdefer std.log.err(
        "while parsing:\n```\n```\nfailed with: {s}\n",
        .{parser.err.items},
    );

    const value = try parser.parse(T, tokens);

    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();
    try tokens.dump(TokenDumpOptions.minify(), buf.writer());

    // std.debug.print("expected {} vs {}\n", .{ @TypeOf(expected), @TypeOf(value) });
    try expectEqualDeep(expected, value);
    try expectEqualDeep(expected_rest, buf.items);
}

fn parseAndExpectError(
    comptime T: type,
    expected: Error,
    data: []const u8,
    expected_descr: []const u8,
) !void {
    var tokens = &Lexer.init(data).token_stream;
    var parser = Deserializer.init(testing.allocator);
    defer parser.deinit();
    defer parser.parseFree();

    const err = parser.parse(T, tokens);
    try testing.expectError(expected, err);
    try expectEqualDeep(expected_descr, parser.err.items);
}
