const std = @import("std");

const ArrayList = std.ArrayList;
const expect = std.testing.expect;

fn NthIterator(comptime T: type) type {
    return struct {
        data: []const T,
        nth: usize,
        index: usize = 0,

        fn next(self: *NthIterator(T)) ?T {
            if (self.index > self.data.len) {
                return null;
            }
            const result = self.data[self.index];
            self.index = self.index + self.nth;
            return result;
        }
    };
}

pub fn range(len: usize) []const u0 {
    return @as([*]u0, undefined)[0..len];
}

fn parseMap(input: []const u8) anyerror!ArrayList(ArrayList(u8)) {
    var lines = std.mem.split(u8, input, "\n");
    var stacks_initialized = false;
    var stacks: ArrayList(ArrayList(u8)) = undefined;

    outer: while (lines.next()) |line| {
        // init
        if (!stacks_initialized) {
            const size = (line.len + 1) / 4;
            stacks = try ArrayList(ArrayList(u8)).initCapacity(std.heap.c_allocator, size);

            for (range(size)) |_| {
                stacks.appendAssumeCapacity(ArrayList(u8).init(std.heap.c_allocator));
            }

            stacks_initialized = true;
        }

        var iter = NthIterator(u8){ .data = line, .nth = 4, .index = 1 };
        var column: usize = 0;

        while (iter.next()) |char| {
            var stack = &stacks.items[column];
            column += 1;

            if (std.ascii.isDigit(char)) break :outer;

            if (char != ' ') {
                try stack.append(char);
            }
        }
    }

    for (range(stacks.items.len)) |_, i| {
        // std.debug.print("\n== {d} before: {any} ==\n", .{i, stacks.items[i].items});
        std.mem.reverse(u8, stacks.items[i].items);
    }

    return stacks;
}

test "parse map" {
    const map = try parseMap(
        \\    [D]    \
        \\[N] [C]    \
        \\[Z] [M] [P]\
        \\ 1   2   3
    );

    try expect(null == null);
    try expect(map.items.len == 3);
    try expect(std.mem.eql(u8, map.items[0].items, &[_]u8{ 'Z', 'N' }));
    try expect(std.mem.eql(u8, map.items[1].items, &[_]u8{ 'M', 'C', 'D' }));
    try expect(std.mem.eql(u8, map.items[2].items, &[_]u8{'P'}));
}

const Instruction = struct {
    size: usize,
    from: usize,
    to: usize,

    fn parse(line: []const u8) ?Instruction {
        var iter = std.mem.split(u8, line, " ");
        var parts:[6]([]const u8) = undefined;
        var i: usize = 0;

        while(iter.next()) |part| {
            parts[i] = part;
            i += 1;
        }

        return Instruction {
            .size = std.fmt.parseInt(u32, parts[1], 10) catch return null,
            .from = ( std.fmt.parseInt(u32, parts[3], 10) catch return null ) - 1,
            .to = ( std.fmt.parseInt(u32, parts[5], 10) catch return null ) - 1,
        };
    }

    fn execute(self: *const Instruction, map: *ArrayList(ArrayList(u8))) error{OutOfMemory}!void {
        var from = &map.items[self.from];
        var to = &map.items[self.to];
        for (range(self.size)) |_| {
            try to.append(from.popOrNull() orelse continue);
        }
    }

    fn execute2(self: *const Instruction, map: *ArrayList(ArrayList(u8))) error{OutOfMemory}!void {
        var from = &map.items[self.from];
        var to = &map.items[self.to];
        var tmp = try ArrayList(u8).initCapacity(std.heap.c_allocator, self.size);

        for (range(self.size)) |_| {
            tmp.appendAssumeCapacity(from.popOrNull() orelse continue);
        }

        for (range(tmp.items.len)) |_| {
            try to.append(tmp.pop());
        }
    }
};

test "parse instruction" {
    const instruction = Instruction.parse("move 1 from 2 to 1").?;
    try expect(instruction.size == 1);
    try expect(instruction.from == 1);
    try expect(instruction.to == 0);

    const instr = Instruction.parse("move 18 from 23 to 51").?;
    try expect(instr.size == 18);
    try expect(instr.from == 22);
    try expect(instr.to == 50);
}

test "execute instruction" {
    const instruction = Instruction.parse("move 1 from 2 to 1").?;
    var map = try parseMap(
        \\    [D]    \
        \\[N] [C]    \
        \\[Z] [M] [P]\
        \\ 1   2   3
    );
    try instruction.execute(&map);
    try expect(std.mem.eql(u8, map.items[0].items, &[_]u8{ 'Z', 'N', 'D' }));
    try expect(std.mem.eql(u8, map.items[1].items, &[_]u8{ 'M', 'C' }));
    try expect(std.mem.eql(u8, map.items[2].items, &[_]u8{'P'}));
}

fn printMap(map: *ArrayList(ArrayList(u8))) void {
    std.debug.print("\n", .{});
    for (range(map.items.len)) |_, i| {
        std.debug.print("[{d}] = ", .{i});
        for (range(map.items[i].items.len)) |_, j| {
            std.debug.print("[{c}] ", .{map.items[i].items[j]});
        }
        std.debug.print("\n", .{});
    }
}

pub fn main() anyerror!void {
    var buffer: [512]u8 = undefined;
    var buf = std.io.bufferedReader(std.io.getStdIn().reader());
    var reader = buf.reader();

    var rawMapBuffer = try ArrayList(u8).initCapacity(std.heap.c_allocator, 10240);
    var rawMap = rawMapBuffer.writer();

    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (line.len == 0) {
            break;
        }
        _ = try rawMap.write(line);
        _ = try rawMap.write("\n");
    }

    var map = try parseMap(rawMapBuffer.items);
    printMap(&map);

    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const instr = Instruction.parse(line) orelse continue;
        instr.execute2(&map) catch continue;
        printMap(&map);
    }
}
