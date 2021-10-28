const std = @import("std");
const testing = std.testing;

const unicode = @import("unicode.zig");
const unescapedLength = unicode.unescapedLength;

const testing_ext = @import("./testing_ext.zig");
const expectEqualDeep = testing_ext.expectEqual;

const debug_ext = @import("./debug_ext.zig");
const dbg = debug_ext.dbg;

test {
    @import("std").testing.refAllDecls(@This());
}

const Here = @This();
pub const Exports = struct {
    pub const Lexer = Here.Lexer;
    pub const LexerError = Here.Error;
    pub const Token = Here.Token;
    pub const TokenStream = Here.TokenStream;
    pub const TokenDumpOptions = Here.DumpOptions;
};

// ---

pub const DumpOptions = struct {
    whitespace_str: []const u8,
    indent_str: []const u8,
    linefeed_str: []const u8,
    linefeed_dedup: bool,

    const Self = @This();

    pub fn minify() Self {
        return .{
            .whitespace_str = "",
            .indent_str = "",
            .linefeed_str = "",
            .linefeed_dedup = true,
        };
    }
    pub fn prettify() Self {
        return .{
            .whitespace_str = " ",
            .indent_str = "    ",
            .linefeed_str = "\n",
            .linefeed_dedup = true,
        };
    }
};

pub const TokenStream = struct {
    const Self = @This();

    peeked: ?Token = null,
    refill: ?Token = null,
    peekFn: fn (self: *Self) Error!?Token,
    nextFn: fn (self: *Self) Error!?Token,

    pub fn peek(self: *Self) Error!?Token {
        if (self.refill) |tok| {
            return @as(Error!?Token, tok);
        }
        return self.peekFn(self);
        // TODO: how does this not work???
        // if (self.peeked) |peeked| return peeked;
        // self.peeked = try self.peekFn(self);
        // return self.peeked;
    }
    pub fn next(self: *Self) Error!?Token {
        self.peeked = null;
        if (self.refill) |tok| {
            self.refill = null;
            return @as(Error!?Token, tok);
        }
        return self.nextFn(self);
    }
    pub fn refill(self: *Self, token: Token) void {
        self.refill = token;
    }

    pub fn dump(
        self: *Self,
        opts: DumpOptions,
        writer: anytype,
    ) (Error || @TypeOf(writer).Error)!void {
        while ((try self.next())) |tok| {
            try tok.dump(opts, writer);
        }
    }
};

/// A very coarse grained token, with the absolute bare minimum of syntactic
/// checks done.
///
/// Tokens are zero-copy & zero-alloc, they _always_ reference the original
/// data and never own any memory themselves.
pub const Token = union(enum) {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    Colon,
    Comma,

    /// Number of whitespace bytes in a row.
    Whitespace: usize,
    // Number of new lines in a row.
    Linefeed: usize,

    /// A single line comment:
    /// - includes the `//` prefix,
    /// - doesn't include the linefeed that ends it.
    ///
    /// Multi-line & recursive comments aren't supported.
    Comment: []const u8,

    // Anything that's not in quotes, starts with a letter and only contains
    // alphanum stuff.
    Identifier: []const u8,
    // Anything that's not in quotes, starts with a (possibly hexadecimal)
    // number and only contains (possibly hexadecimal) digits, dots and
    // underscores.
    Number: []const u8,

    /// Some UTF-8 thingy between single quotes, accounting for escaping.
    SingleQuoted: struct {
        decoded_len: u32,
        str: []const u8,
    },
    /// Some UTF-8 thingy between double quotes, accounting for escaping.
    DoubleQuoted: struct {
        decoded_len: u32,
        str: []const u8,
    },
    /// Some UTF-8 thingy using Rust's raw string notation, cannot be escaped.
    RawQuoted: struct {
        depth: u8, // number of #
        str: []const u8,
    },

    const Self = @This();

    pub fn length(self: Self) usize {
        return switch (self) {
            .OpenParen => 1,
            .CloseParen => 1,
            .OpenBracket => 1,
            .CloseBracket => 1,
            .OpenBrace => 1,
            .CloseBrace => 1,

            .Colon => 1,
            .Comma => 1,

            .Whitespace => |ws| ws,
            .Linefeed => |lf| lf,

            .Comment => |str| str.len,

            .Identifier => |str| str.len,
            .Number => |str| str.len,

            .SingleQuoted => |sstr| sstr.str.len + 2,
            .DoubleQuoted => |dstr| dstr.str.len + 2,
            .RawQuoted => |rstr| rstr.depth * 2 + 3 + rstr.str.len,
        };
    }

    pub fn dump(
        self: Self,
        opts: DumpOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self) {
            .OpenParen => try writer.writeAll("("),
            .CloseParen => try writer.writeAll(")"),
            .OpenBracket => try writer.writeAll("["),
            .CloseBracket => try writer.writeAll("]"),
            .OpenBrace => try writer.writeAll("{"),
            .CloseBrace => try writer.writeAll("}"),

            .Colon => try writer.writeAll(":"),
            .Comma => try writer.writeAll(","),

            .Whitespace => |ws| {
                var i: usize = 0;
                while (i < ws) : (i += 1) {
                    try writer.writeAll(opts.whitespace_str);
                }
            },
            .Linefeed => |lf| {
                if (opts.linefeed_dedup) {
                    try writer.writeAll(opts.linefeed_str);
                } else {
                    var i: usize = 0;
                    while (i < lf) : (i += 1) {
                        try writer.writeAll(opts.linefeed_str);
                    }
                }
            },

            .Comment => |str| try writer.writeAll(str),

            .Identifier => |str| try writer.writeAll(str),
            .Number => |str| try writer.writeAll(str),

            .SingleQuoted => |sstr| try writer.print("'{s}'", .{sstr.str}),
            .DoubleQuoted => |dstr| try writer.print("\"{s}\"", .{dstr.str}),
            .RawQuoted => |rstr| {
                try writer.writeByte('r');
                try writer.writeByteNTimes('#', rstr.depth);
                try writer.print("\"{s}\"", .{rstr.str});
                try writer.writeByteNTimes('#', rstr.depth);
            },
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        return self.dump(DumpOptions.prettify(), writer);
    }
    pub fn formatTag(comptime tag: std.meta.Tag(Self)) []const u8 {
        return switch (tag) {
            .OpenParen => "(",
            .CloseParen => ")",
            .OpenBracket => "[",
            .CloseBracket => "]",
            .OpenBrace => "{",
            .CloseBrace => "}",
            .Colon => ":",
            .Comma => ",",

            .Whitespace => "<whitespace>",
            .Linefeed => "<linefeed>",

            .Comment => "<// comment>",

            .Identifier => "<identifier>",
            .Number => "<number>",

            .SingleQuoted => "<single-quoted>",
            .DoubleQuoted => "<double-quoted>",
            .RawQuoted => "<raw string>",
        };
    }
};

test "tokens: dump" {
    const str =
        \\TestCase(
        \\  // Some random comment regarding the field below:
        \\  single_quoted: 'hello world \xFF \u{999} \' "" \\ !!',
        \\  double_quoted: "hello world \xFF \u{999} \" '' \\ !!",
        \\  raw_quoted1: r"hello world \xFF \u{999} \"\" '' \\ !!",
        \\  raw_quoted2: r#"hello world \xFF \u{999} "" '' \\ !!"#,
        \\  raw_quoted3: r#"  "\\#  "#,
        \\  raw_quoted4: r#""\\#"x#"#,
        \\  raw_quoted5: r###""\\#"x#  "##    "###,
        \\  number: +0x18e_42.300---ffff,
        \\  identifier: true,
        \\  array: [1, 2, 3],
        \\  map: { 1: "one", 2: "two", 3: "three" },
        \\)
    ;
    _ = str;

    {
        var buf = std.ArrayList(u8).init(testing.allocator);
        defer buf.deinit();
        var w = buf.writer();

        var lex = &Lexer.init(str).token_stream;
        try lex.dump(DumpOptions.prettify(), w);

        try expectEqualDeep(@as([]const u8, str), buf.items);
    }
}

// ---

pub const Error = error{
    UnbalancedSingleQuotes,
    UnbalancedDoubleQuotes,
    UnbalancedRawQuotes,
    InvalidSyntax,
    UnterminatedEscapeSequence,
};

/// A very coarse grained lexer, with the absolute bare minimum of syntactic
/// checks done.
///
/// General shape and overall high-level syntax is accounted for; the reste will
/// have to wait for the parsing stage since we don't even know what type we're
/// deserializing into at this point.
pub const Lexer = struct {
    line_nr: usize,
    col_nr: usize,
    total_bytes: usize,

    pos: usize,
    data: []const u8,

    token_stream: TokenStream = .{
        .peekFn = peek,
        .nextFn = next,
    },

    const Self = @This();

    pub fn init(data: []const u8) Self {
        return .{
            .line_nr = 0,
            .col_nr = 0,
            .total_bytes = 0,

            .pos = 0,
            .data = data,
        };
    }

    fn peek(ts: *TokenStream) Error!?Token {
        var self = @fieldParentPtr(Self, "token_stream", ts);

        if (self.pos >= self.data.len) return null;

        const str = self.data[self.pos..];
        switch (str[0]) {
            '(' => return Token.OpenParen,
            ')' => return Token.CloseParen,
            '[' => return Token.OpenBracket,
            ']' => return Token.CloseBracket,
            '{' => return Token.OpenBrace,
            '}' => return Token.CloseBrace,

            ':' => return Token.Colon,
            ',' => return Token.Comma,

            else => {},
        }

        if (isWhitespace(str[0])) {
            var pos: usize = 1;
            for (str[pos..]) |b| {
                if (isWhitespace(b)) {
                    pos += 1;
                } else break;
            }
            return Token{ .Whitespace = pos };
        }
        if (isLinefeed(str[0])) {
            var pos: usize = 1;
            for (str[pos..]) |b| {
                if (isLinefeed(b)) {
                    pos += 1;
                } else break;
            }
            return Token{ .Linefeed = pos };
        }

        if (isComment(str)) {
            var pos: usize = 2;
            for (str[pos..]) |b| {
                if (isLinefeed(b)) {
                    return Token{ .Comment = str[0..pos] };
                }
                pos += 1;
            }
            return Token{ .Comment = str[0..pos] };
        }

        // Raw strings must be checked _BEFORE_ anything else!
        if (try doRawQuotes(str)) |tok| return tok;

        // Identifier
        if (std.ascii.isAlpha(str[0])) {
            var pos: usize = 1;
            for (str[pos..]) |b| {
                if (std.ascii.isAlNum(b) or b == '_') {
                    pos += 1;
                } else break;
            }
            return Token{ .Identifier = str[0..pos] };
        }
        // Number
        if (std.ascii.isXDigit(str[0]) or str[0] == '+' or str[0] == '-') {
            var pos: usize = 1;
            for (str[pos..]) |b| {
                if (std.ascii.isXDigit(b)) {
                    pos += 1;
                } else {
                    switch (b) {
                        '_', '.', 'x', 'b', 'o', '+', '-' => {
                            pos += 1;
                        },
                        else => break,
                    }
                }
            }
            return Token{ .Number = str[0..pos] };
        }

        const quotes = [_]u8{ '\'', '\"' };
        inline for (quotes) |quote| {
            if (try doSimpleQuotes(str, quote)) |s| {
                const decoded_len = unescapedLength(s) catch return error.UnterminatedEscapeSequence;
                const decoded_len_u32 = @intCast(u32, decoded_len);
                // TODO: there are most definitely nicer ways to do the
                // following with union/enum reflection.
                return switch (comptime quote) {
                    '\'' => Token{ .SingleQuoted = .{
                        .decoded_len = decoded_len_u32,
                        .str = s,
                    } },
                    '"' => Token{ .DoubleQuoted = .{
                        .decoded_len = decoded_len_u32,
                        .str = s,
                    } },
                    else => @compileError("`" ++ quote ++ "` is in fact not a quote"),
                };
            }
        }

        return error.InvalidSyntax;
    }

    fn next(ts: *TokenStream) Error!?Token {
        var self = @fieldParentPtr(Self, "token_stream", ts);
        const tok = try ts.peek();
        self.pos += (tok orelse return null).length();
        return tok;
    }
};

fn doSimpleQuotes(str: []const u8, comptime quote: u8) Error!?[]const u8 {
    if (str[0] == quote) {
        var pos: usize = 1;
        var escapin = false;
        for (str[pos..]) |b| {
            switch (b) {
                '\\' => {
                    escapin = !escapin;
                    pos += 1;
                    continue;
                },
                quote => if (!escapin) {
                    return str[1..pos];
                },
                else => {},
            }

            escapin = false;
            pos += 1;
        }

        return switch (comptime quote) {
            '\'' => error.UnbalancedSingleQuotes,
            '"' => error.UnbalancedDoubleQuotes,
            else => @compileError("`" ++ quote ++ "` is in fact not a quote"),
        };
    }
    return null;
}

fn doRawQuotes(str: []const u8) Error!?Token {
    if (str[0] != 'r') return null;

    var pos: usize = 1;
    var depth: u8 = 0;
    for (str[pos..]) |b| {
        if (b == '#') depth += 1 else break;
    }

    pos += depth;
    if (pos >= str.len or str[pos] != '"') return null;

    if (depth == 0) {
        return Token{ .RawQuoted = .{
            .depth = 0,
            .str = (try doSimpleQuotes(str[pos..], '"')) orelse return error.InvalidSyntax,
        } };
    }

    pos += 1;
    var leavin = false;
    outer: while (pos < str.len) {
        const b = str[pos];

        switch (b) {
            '"' => {
                leavin = true;
                pos += 1;
                continue;
            },
            '#' => if (leavin) {
                const ret = str[depth + 2 .. pos - 1];
                leavin = false;

                var i: usize = 0;
                while (i < depth) : ({
                    i += 1;
                    pos += 1;
                }) {
                    if (pos >= str.len) return error.UnbalancedRawQuotes;
                    if (str[pos] != '#') continue :outer;
                }

                return Token{ .RawQuoted = .{
                    .depth = depth,
                    .str = ret,
                } };
            },
            else => {},
        }

        leavin = false;
        pos += 1;
    }

    return error.UnbalancedRawQuotes;
}

fn isWhitespace(b: u8) bool {
    return b == ' ' or b == '\t' or b == '\r';
}

fn isLinefeed(b: u8) bool {
    return b == '\n';
}

fn isComment(str: []const u8) bool {
    return str.len > 1 and std.mem.eql(u8, str[0..2], "//");
}

fn isDigit(b: u8) bool {
    return std.ascii.isXDigit(b) or b == '+' or b == '-';
}

test "lexer: all tokens" {
    const str =
        \\TestCase(
        \\  // Some random comment regarding the field below:
        \\  single_quoted: 'hello world \xFF \u{999} \' "" \\ !!',
        \\  double_quoted: "hello world \xFF \u{999} \" '' \\ !!",
        \\  raw_quoted1: r"hello world \xFF \u{999} \"\" '' \\ !!",
        \\  raw_quoted2: r#"hello world \xFF \u{999} "" '' \\ !!"#,
        \\  raw_quoted3: r#"  "\\#  "#,
        \\  raw_quoted4: r#""\\#"x#"#,
        \\  raw_quoted5: r###""\\#"x#  "##    "###,
        \\  raw_quoted6: r####"\"hello world\""####,
        \\  number: +0x18e_42.300---ffff,
        \\  identifier: true,
        \\  array: [1, 2, 3],
        \\  map: { 1: "one", 2: "two", 3: "three" },
        \\)
    ;

    const checkStructField = struct {
        fn func(lex: *TokenStream, indent: usize, id: []const u8, value: Token) !void {
            try expectEqualDeep(Token{ .Linefeed = 1 }, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = indent * 2 }, (try lex.next()).?);
            try expectEqualDeep(Token{ .Identifier = id }, (try lex.next()).?);
            try expectEqualDeep(Token.Colon, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
            try expectEqualDeep(value, (try lex.next()).?);
            try expectEqualDeep(Token.Comma, (try lex.next()).?);
        }
    }.func;
    _ = checkStructField;

    var lex = &Lexer.init(str).token_stream;

    try expectEqualDeep(Token{ .Identifier = "TestCase" }, (try lex.next()).?);
    try expectEqualDeep(Token.OpenParen, (try lex.next()).?);
    {
        try expectEqualDeep(Token{ .Linefeed = 1 }, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 2 }, (try lex.next()).?);
        try expectEqualDeep(Token{
            .Comment = "// Some random comment regarding the field below:",
        }, (try lex.next()).?);

        try checkStructField(lex, 1, "single_quoted", Token{
            .SingleQuoted = .{
                .decoded_len = 27,
                .str = "hello world \\xFF \\u{999} \\' \"\" \\\\ !!",
            },
        });
        try checkStructField(lex, 1, "double_quoted", Token{
            .DoubleQuoted = .{
                .decoded_len = 27,
                .str = "hello world \\xFF \\u{999} \\\" '' \\\\ !!",
            },
        });
        try checkStructField(lex, 1, "raw_quoted1", Token{ .RawQuoted = .{
            .depth = 0,
            .str = "hello world \\xFF \\u{999} \\\"\\\" '' \\\\ !!",
        } });
        try checkStructField(lex, 1, "raw_quoted2", Token{ .RawQuoted = .{
            .depth = 1,
            .str = "hello world \\xFF \\u{999} \"\" '' \\\\ !!",
        } });
        try checkStructField(lex, 1, "raw_quoted3", Token{
            .RawQuoted = .{
                .depth = 1,
                .str = "  \"\\\\#  ",
            },
        });
        try checkStructField(lex, 1, "raw_quoted4", Token{
            .RawQuoted = .{
                .depth = 1,
                .str = "\"\\\\#\"x#",
            },
        });
        try checkStructField(lex, 1, "raw_quoted5", Token{
            .RawQuoted = .{
                .depth = 3,
                .str = "\"\\\\#\"x#  \"##    ",
            },
        });
        try checkStructField(lex, 1, "raw_quoted6", Token{
            .RawQuoted = .{
                .depth = 4,
                .str = "\\\"hello world\\\"",
            },
        });

        try checkStructField(lex, 1, "number", Token{ .Number = "+0x18e_42.300---ffff" });
        try checkStructField(lex, 1, "identifier", Token{ .Identifier = "true" });

        try expectEqualDeep(Token{ .Linefeed = 1 }, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 2 }, (try lex.next()).?);
        try expectEqualDeep(Token{ .Identifier = "array" }, (try lex.next()).?);
        try expectEqualDeep(Token.Colon, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
        try expectEqualDeep(Token.OpenBracket, (try lex.next()).?);
        {
            try expectEqualDeep(Token{ .Number = "1" }, (try lex.next()).?);
            try expectEqualDeep(Token.Comma, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);

            try expectEqualDeep(Token{ .Number = "2" }, (try lex.next()).?);
            try expectEqualDeep(Token.Comma, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);

            try expectEqualDeep(Token{ .Number = "3" }, (try lex.next()).?);
            try expectEqualDeep(Token.CloseBracket, (try lex.next()).?);
            try expectEqualDeep(Token.Comma, (try lex.next()).?);
        }

        try expectEqualDeep(Token{ .Linefeed = 1 }, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 2 }, (try lex.next()).?);
        try expectEqualDeep(Token{ .Identifier = "map" }, (try lex.next()).?);
        try expectEqualDeep(Token.Colon, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
        try expectEqualDeep(Token.OpenBrace, (try lex.next()).?);
        {
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
            try expectEqualDeep(Token{ .Number = "1" }, (try lex.next()).?);
            try expectEqualDeep(Token.Colon, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
            try expectEqualDeep(Token{ .DoubleQuoted = .{
                .decoded_len = 3,
                .str = "one",
            } }, (try lex.next()).?);

            try expectEqualDeep(Token.Comma, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
            try expectEqualDeep(Token{ .Number = "2" }, (try lex.next()).?);
            try expectEqualDeep(Token.Colon, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
            try expectEqualDeep(Token{ .DoubleQuoted = .{
                .decoded_len = 3,
                .str = "two",
            } }, (try lex.next()).?);

            try expectEqualDeep(Token.Comma, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
            try expectEqualDeep(Token{ .Number = "3" }, (try lex.next()).?);
            try expectEqualDeep(Token.Colon, (try lex.next()).?);
            try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
            try expectEqualDeep(Token{ .DoubleQuoted = .{
                .decoded_len = 5,
                .str = "three",
            } }, (try lex.next()).?);
        }
        try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
        try expectEqualDeep(Token.CloseBrace, (try lex.next()).?);
        try expectEqualDeep(Token.Comma, (try lex.next()).?);

        try expectEqualDeep(Token{ .Linefeed = 1 }, (try lex.next()).?);
        try expectEqualDeep(Token.CloseParen, (try lex.next()).?);

        try expectEqualDeep(@as(?Token, null), (try lex.next()));
        try expectEqualDeep(@as(?Token, null), (try lex.next()));
        try expectEqualDeep(@as(?Token, null), (try lex.next()));
    }
}

test "lexer: all errors" {
    {
        const unbalanced_single_quotes =
            \\unbalanced: 'that,
        ;

        var lex = &Lexer.init(unbalanced_single_quotes).token_stream;
        try expectEqualDeep(Token{ .Identifier = "unbalanced" }, (try lex.next()).?);
        try expectEqualDeep(Token.Colon, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
        try std.testing.expectError(error.UnbalancedSingleQuotes, lex.next());
    }

    {
        const unbalanced_double_quotes =
            \\unbalanced: "that,
        ;

        var lex = &Lexer.init(unbalanced_double_quotes).token_stream;
        try expectEqualDeep(Token{ .Identifier = "unbalanced" }, (try lex.next()).?);
        try expectEqualDeep(Token.Colon, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
        try std.testing.expectError(error.UnbalancedDoubleQuotes, lex.next());
    }

    {
        const unbalanced_raw_quotes =
            \\unbalanced: r###"that's not right"##,
        ;

        var lex = &Lexer.init(unbalanced_raw_quotes).token_stream;
        try expectEqualDeep(Token{ .Identifier = "unbalanced" }, (try lex.next()).?);
        try expectEqualDeep(Token.Colon, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
        try std.testing.expectError(error.UnbalancedRawQuotes, lex.next());
    }

    {
        const invalid_syntax =
            \\invalid = 'that\'s not right'
        ;

        var lex = &Lexer.init(invalid_syntax).token_stream;
        try expectEqualDeep(Token{ .Identifier = "invalid" }, (try lex.next()).?);
        try expectEqualDeep(Token{ .Whitespace = 1 }, (try lex.next()).?);
        try std.testing.expectError(error.InvalidSyntax, lex.next());
    }
}
