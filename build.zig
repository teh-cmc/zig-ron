const std = @import("std");

// TODO: clean all this mess

fn addTestCmd(
    b: *std.build.Builder,
    target: anytype,
    mode: anytype,
    pkg_opt: ?std.build.Pkg,
    name: []const u8,
    description: []const u8,
    comptime files: []const []const u8,
    valgrind: bool,
) *std.build.Step {
    const cmd = b.step(name, description);
    inline for (files) |test_file| {
        const step = b.addTest(test_file);
        step.setTarget(target);
        step.setBuildMode(mode);
        if (pkg_opt) |pkg| step.addPackage(pkg);

        if (valgrind) {
            step.linkLibC();
            step.valgrind_support = true;
            step.setExecCmd(&.{
                "valgrind",
                "--leak-check=full",
                "--track-origins=yes",
                "--show-leak-kinds=all",
                "--num-callers=15",
                null,
            });
        }

        cmd.dependOn(&step.step);
    }

    return cmd;
}

fn addExampleCmd(
    b: *std.build.Builder,
    target: anytype,
    mode: anytype,
    pkg_opt: ?std.build.Pkg,
    name: []const u8,
    description: []const u8,
    root_src: []const u8,
    valgrind: bool,
) *std.build.Step {
    const cmd = b.step(name, description);

    const exe = b.addExecutable(name, root_src);
    exe.setTarget(target);
    exe.setBuildMode(mode);
    if (pkg_opt) |pkg| exe.addPackage(pkg);
    if (valgrind) {
        exe.linkLibC();
        exe.valgrind_support = true;
    }
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    cmd.dependOn(&run_cmd.step);

    return cmd;
}

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const pkg_ron = std.build.Pkg{
        .name = b.dupe("ron"),
        .path = .{ .path = b.dupe("ron.zig") },
    };

    // zig fmt: off
    const cmd_example_deser = addExampleCmd(
        b, target, mode, pkg_ron,
        "example-deser", "Run basic deserializer example",
        "examples/deserializer.zig", false,
    );
    const cmd_example_deser_dbg = addExampleCmd(
        b, target, mode, pkg_ron,
        "example-deser-dbg", "Run basic deserializer example through Valgrind",
        "examples/deserializer.zig", true,
    );

    const cmd_test = addTestCmd(
        b, target, mode, null,
        "test", "Run unit tests",
        &.{"ron.zig"}, false,
    );
    const cmd_test_dbg = addTestCmd(
        b, target, mode, null,
        "test-dbg", "Run unit tests through Valgrind",
        &.{"ron.zig"}, true,
    );

    const cmd_test_integr = addTestCmd(
        b, target, mode, pkg_ron,
        "test-integr", "Run integration tests",
        &.{"tests/value.zig"}, false,
    );
    const cmd_test_integr_dbg = addTestCmd(
        b, target, mode, pkg_ron,
        "test-integr-dbg", "Run integration tests through Valgrind",
        &.{"tests/value.zig"}, true,
    );
    // zig fmt: on

    // TODO: move these in dedicated lib in due time
    const cmd_test_meta = addTestCmd(b, target, mode, null, "meta", "", &.{"src/meta_ext.zig"}, false);
    _ = cmd_test_meta;
    const cmd_test_fmt = addTestCmd(b, target, mode, null, "fmt", "", &.{"src/fmt_ext.zig"}, false);
    _ = cmd_test_fmt;
    const cmd_test_lex = addTestCmd(b, target, mode, null, "lex", "", &.{"src/lexer.zig"}, false);
    _ = cmd_test_lex;
    const cmd_test_de = addTestCmd(b, target, mode, null, "de", "", &.{"src/deserializer.zig"}, false);
    _ = cmd_test_de;

    _ = cmd_example_deser;
    _ = cmd_example_deser_dbg;
    _ = cmd_test;
    _ = cmd_test_dbg;
    _ = cmd_test_integr;
    _ = cmd_test_integr_dbg;

    b.default_step = cmd_example_deser;
}
