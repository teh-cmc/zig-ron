const std = @import("std");
const mem = std.mem;
const testing = std.testing;

test {
    @import("std").testing.refAllDecls(@This());
}

// ---

pub fn unescape(dst: []u8, str: []const u8) !usize {
    var i: usize = 0;
    var j: usize = 0;
    // zig fmt: off
        while (i < str.len) {
            if (str[i] != '\\') {
                dst[j] = str[i];
                i += 1; j += 1;
                continue;
            }

            i += 1;
            if (i >= str.len) return error.UnterminatedEscapeSequence;

            switch (str[i]) {
                '\'', '"', '\\' => { dst[j] = str[i]; i += 1; j += 1; },
                'n' => { dst[j] = '\n'; i += 1; j += 1; },
                'r' => { dst[j] = '\r'; i += 1; j += 1; },
                't' => { dst[j] = '\t'; i += 1; j += 1; },
                'x' => {
                    i += 1;
                    if (str[i..].len < 2) return error.UnterminatedEscapeSequence;

                    const c = try std.fmt.parseInt(u8, str[i..][0..2], 16);
                    dst[j] = c;

                    i += 2; j += 1;
                },
                'u' => {
                    i += 1;
                    if (i >= str.len or str[i] != '{')
                        return error.UnterminatedEscapeSequence;

                    i += 1;

                    var n: usize = 0;
                    while (n < str[i..].len and str[i..][n] != '}') : (n += 1) {}
                    if (n >= str[i..].len) return error.UnterminatedEscapeSequence;

                    const c = try std.fmt.parseInt(u21, str[i..][0..n], 16);
                    const clen = try std.unicode.utf8Encode(c, dst[j..]);

                    i += n+1; j += clen;
                },
                else => return error.UnterminatedEscapeSequence,
            }
        }
        // zig fmt: on

    return j;
}

test "unicode: unescape" {
    var dst: [100]u8 = undefined;
    {
        const str = "\x07";
        const expected = "\x07";
        const n = try unescape(&dst, str);
        try testing.expectEqualSlices(u8, expected, dst[0..n]);
    }
    {
        const str = "\\x07";
        const expected = "\x07";
        const n = try unescape(&dst, str);
        try testing.expectEqualSlices(u8, expected, dst[0..n]);
    }
    {
        const str = "\\u{7}";
        const expected = "\u{07}";
        const n = try unescape(&dst, str);
        try testing.expectEqualSlices(u8, expected, dst[0..n]);
    }
    {
        const str = "\\u{07}";
        const expected = "\u{07}";
        const n = try unescape(&dst, str);
        try testing.expectEqualSlices(u8, expected, dst[0..n]);
    }
    {
        const str = "\\u{007}";
        const expected = "\u{07}";
        const n = try unescape(&dst, str);
        try testing.expectEqualSlices(u8, expected, dst[0..n]);
    }
    {
        const str = "\\u{0007}";
        const expected = "\u{07}";
        const n = try unescape(&dst, str);
        try testing.expectEqualSlices(u8, expected, dst[0..n]);
    }
    {
        const expected = "\\ \t \" \n \r \t \x08 \x0C \x07 \u{07}";
        const str =
            \\\\ \t \" \n \r \t \x08 \x0C \x07 \u{07}
        ;

        const n = try unescape(&dst, str);
        errdefer {
            std.debug.print("\n```\nexp:{any}\n", .{expected.*});
            std.debug.print("got:{any}\n```\n", .{dst[0..n]});
        }
        try testing.expectEqualSlices(u8, expected, dst[0..n]);
    }
}

// ---

pub fn unescapedLength(str: []const u8) !usize {
    var i: usize = 0;
    var j: usize = 0;
    // zig fmt: off
        while (i < str.len) {
            if (str[i] != '\\') {
                i += 1; j += 1;
                continue;
            }

            i += 1;
            if (i >= str.len) return error.UnterminatedEscapeSequence;

            switch (str[i]) {
                '\'', '"', '\\' => { i += 1; j += 1; },
                'n' => { i += 1; j += 1; },
                'r' => { i += 1; j += 1; },
                't' => { i += 1; j += 1; },
                'x' => {
                    i += 1;
                    if (str[i..].len < 2) return error.UnterminatedEscapeSequence;
                    i += 2; j += 1;
                },
                'u' => {
                    i += 1;
                    errdefer std.debug.print("`{s}`", .{str[i..]});
                    if (i >= str.len or str[i] != '{')
                        return error.UnterminatedEscapeSequence;

                    i += 1;

                    var n: usize = 0;
                    while (n < str[i..].len and str[i..][n] != '}') : (n += 1) {}
                    if (n >= str[i..].len) return error.UnterminatedEscapeSequence;

                    const c = try std.fmt.parseInt(u21, str[i..][0..n], 16);
                    const clen = try std.unicode.utf8CodepointSequenceLength(c);

                    i += n+1; j += clen;
                },
                else => return error.UnterminatedEscapeSequence,
            }
        }
        // zig fmt: on

    return j;
}

test "unicode: unescaped length" {
    {
        const str = "\x07";
        try testing.expectEqual((try unescapedLength(str)), 1);
    }
    {
        const str = "\\x07";
        try testing.expectEqual((try unescapedLength(str)), 1);
    }
    {
        const str = "\\u{7}";
        try testing.expectEqual((try unescapedLength(str)), 1);
    }
    {
        const str = "\\u{07}";
        try testing.expectEqual((try unescapedLength(str)), 1);
    }
    {
        const str = "\\u{007}";
        try testing.expectEqual((try unescapedLength(str)), 1);
    }
    {
        const str = "\\u{0007}";
        try testing.expectEqual((try unescapedLength(str)), 1);
    }
    {
        const str =
            \\hello world \xFF \u{999} \' "" \\ !!
        ;
        try testing.expectEqual((try unescapedLength(str)), 27);
    }
    {
        const str =
            \\hello world \xFF \u{999} \" '' \\ !!
        ;
        try testing.expectEqual((try unescapedLength(str)), 27);
    }
    {
        const str =
            \\\\ \t \" \n \r \t \x08 \x0C \x07 \u{07}
        ;
        try testing.expectEqual((try unescapedLength(str)), 19);
    }
}
