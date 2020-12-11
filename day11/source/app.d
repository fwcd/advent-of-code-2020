import std.stdio, std.file, std.array, std.container, std.algorithm, std.conv, std.utf;

private dchar OCCUPIED = '#';
private dchar EMPTY = 'L';

private dchar[] neighbors(dchar[][] grid, int x, int y) {
	auto nbs = Array!dchar();
	for (int nx = x - 1; nx <= x + 1; nx++) {
		for (int ny = y - 1; ny <= y + 1; ny++) {
			if ((nx != x || ny != y) && nx >= 0 && nx < grid.length && ny >= 0 && ny < grid[0].length) {
				nbs.insert(grid[nx][ny]);
			}
		}
	}
	return nbs.array;
}

// Higher-order functions are slow, therefore we have some redundancy
// among the step/simulation functions between part 1 and 2.

private dchar[][] part1Step(dchar[][] input)
{
	dchar[][] result = input.map!(xs => xs.array).array;
	bool changed = false;

	for (int x = 0; x < input.length; x++) {
		for (int y = 0; y < input[0].length; y++) {
			const nbs = neighbors(input, x, y);
			const occupiedNbs = nbs.filter!(c => c == OCCUPIED).array;

			if (input[x][y] == EMPTY && occupiedNbs.length == 0) {
				// Get occupied
				result[x][y] = OCCUPIED;
			} else if (input[x][y] == OCCUPIED && occupiedNbs.length >= 4) {
				// Get empty
				result[x][y] = EMPTY;
			}

			changed |= input[x][y] != result[x][y];
		}
	}

	return changed ? result : null;
}

private dchar[][] part2Step(dchar[][] input)
{
	dchar[][] result = input.map!(xs => xs.array).array;
	bool changed = false;

	for (int x = 0; x < input.length; x++) {
		for (int y = 0; y < input[0].length; y++) {
			const nbs = neighbors(input, x, y);
			const occupiedNbs = nbs.filter!(c => c == OCCUPIED).array;

			if (input[x][y] == EMPTY && occupiedNbs.length == 0) {
				// Get occupied
				result[x][y] = OCCUPIED;
			} else if (input[x][y] == OCCUPIED && occupiedNbs.length >= 4) {
				// Get empty
				result[x][y] = EMPTY;
			}

			changed |= input[x][y] != result[x][y];
		}
	}

	return changed ? result : null;
}

private dchar[][] simulatePart1(dchar[][] grid)
{
	while (true) {
		dchar[][] next = part1Step(grid);
		if (next != null) {
			grid = next;
		} else {
			return grid;
		}
	}
}

private dchar[][] simulatePart2(dchar[][] grid)
{
	while (true) {
		dchar[][] next = part2Step(grid);
		if (next != null) {
			grid = next;
		} else {
			return grid;
		}
	}
}

private ulong totalOccupied(dchar[][] grid)
{
	return grid.map!(l => l).joiner.filter!(c => c == OCCUPIED).array.length;
}

void main()
{
	dchar[][] grid = readText("resources/input.txt")
		.split('\n')
		.filter!(line => line.length > 0)
		.map!(line => to!(dchar[])(line.toUTF32))
		.array;

	writefln("Part 1: %d", totalOccupied(simulatePart1(grid)));
	writefln("Part 2: %d", totalOccupied(simulatePart2(grid)));
}
