const std = @import("std");
const ArrayList = std.ArrayList;

const Range = struct {
    left: u32,
    right: u32,

    fn within(self: *const Range, other: Range) bool {
        return self.left >= other.left and self.right <= other.right;
    }

    fn overlaps(self: *const Range, other: Range) bool {
        return (self.left <= other.right and self.left >= other.left) or (self.right >= other.left and self.right <= other.right);
    }

    fn fromStr(str: []const u8) ?Range {
        const result = splitTwo(u8, str, '-').?;
        const left = std.fmt.parseInt(u32, result[0], 10) catch return null;
        const right = std.fmt.parseInt(u32, result[1], 10) catch return null;

        return Range{ .left = left, .right = right };
    }
};

fn splitTwo(comptime T: type, haystack: []const T, delimiter: T) ?[2]([]const T) {
    const index = std.mem.indexOfScalar(T, haystack, delimiter).?;
    if ((index + 1) == haystack.len) return null;
    return [_]([]const T){ haystack[0..index], haystack[(index + 1)..] };
}

fn parseLine(line: []const u8) ?[2]Range {
    const result = splitTwo(u8, line, ',').?;
    const first = Range.fromStr(result[0]).?;
    const second = Range.fromStr(result[1]).?;
    return [2]Range{ first, second };
}

pub fn main() anyerror!void {
    var buffer: [512]u8 = undefined;
    var buf = std.io.bufferedReader(std.io.getStdIn().reader());
    var reader = buf.reader();

    var part1: u32 = 0;
    var part2: u32 = 0;

    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const ranges = parseLine(line) orelse continue;
        const left = ranges[0];
        const right = ranges[1];

        if (left.within(right) or right.within(left)) {
            part1 += 1;
        }

        if (left.overlaps(right) or right.overlaps(left)) {
            // std.debug.print("= left: {?} right: {?} overlaps\n", .{ left, right });
            part2 += 1;
        }
    }

    std.debug.print("part1: {d}\n", .{part1});
    std.debug.print("part2: {d}\n", .{part2});
}

test "parseLine" {
    var line = "1-2,2-3";
    parseLine(line) orelse return;
}
