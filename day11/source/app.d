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

private dchar[][] step(dchar[][] input)
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

void main()
{
	dchar[][] grid = readText("resources/input.txt")
		.split('\n')
		.filter!(line => line.length > 0)
		.map!(line => to!(dchar[])(line.toUTF32))
		.array;

	bool running = true;
	int i = 0;
	while (running) {
		// writefln("%s", grid.map!(cs => cs.toUTF8).join());
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
