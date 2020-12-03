import functools

def count_trees(forest, right, down):
    i = 1
    count = 0
    while (i * down) < len(forest):
        if forest[i * down][(i * right) % len(forest[0])] == "#":
            count += 1
        i += 1
    return count

def main():
    with open("resources/input.txt", "r") as f:
        forest = [l.strip() for l in f.readlines()]

        part1 = count_trees(forest, 3, 1)
        print(f"Part 1: {part1}")

        part2 = functools.reduce(lambda x, y: x * y, [count_trees(forest, r, d) for (r, d) in [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]])
        print(f"Part 2: {part2}")

if __name__ == "__main__":
    main()
