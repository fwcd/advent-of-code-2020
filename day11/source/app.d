import std.stdio, std.file, std.array, std.algorithm;

private char OCCUPIED = '#';
private char EMPTY = 'L';

private dchar[] neighbors(dchar[][] grid, int x, int y) {
	return [
		grid[x - 1][y - 1], grid[x][y - 1], grid[x + 1][y - 1],
		grid[x - 1][y],                     grid[x + 1][y],
		grid[x - 1][y + 1], grid[x][y + 1], grid[x + 1][y + 1]
	];
}

private dchar[][] step(dchar[][] input)
{
	dchar[][] result = new dchar[][](input[0].length, input.length);
	bool changed = false;

	for (int x = 1; x < input[0].length - 1; x++) {
		for (int y = 1; y < input.length - 1; y++) {
			const nbs = neighbors(input, x, y);
			const occupiedNbs = nbs.filter!(c => c == OCCUPIED).array;

			if (occupiedNbs.length == 0) {
				// Get occupied
				result[x][y] = OCCUPIED;
				changed = true;
			} else if (input[x][y] == OCCUPIED && occupiedNbs.length >= 4) {
				// Get empty
				result[x][y] = EMPTY;
				changed = true;
			} else {
				result[x][y] = input[x][y];
			}
		}
	}

	return changed ? result : null;
}

void main()
{
	dchar[][] grid = readText("resources/input.txt")
		.split('\n')
		.filter!(line => line.length > 0)
		.map!(line => line.map!(c => cast(dchar) c).array)
		.array;

	bool running = true;
	int i = 0;
	while (running) {
		writefln("%d. iteration", i);
		dchar[][] next = step(grid);
		if (next != null) {
			grid = next;
		} else {
			running = false;
		}
		i++;
	}

	auto part1 = grid.map!(l => l).joiner.filter!(c => c == OCCUPIED).array.length;
	writefln("Part 1: %d", part1);
}
