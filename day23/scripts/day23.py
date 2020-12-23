def dest_cup(x, xs):
    if x < min(xs):
        print("Wrapping")
        return dest_cup(max(xs), xs)
    else:
        try:
            return xs.index(x)
        except:
            return dest_cup(x - 1, xs)

def move(xs):
    x = xs[0]
    taken = xs[1:4]
    rest = [xs[0]] + xs[4:]
    i = dest_cup(x - 1, rest)
    xs = rest[:(i + 1)] + taken + rest[(i + 1):]
    return xs[1:] + [xs[0]]

def main():
    xs = [3, 8, 9, 1, 2, 5, 4, 6, 7]
    # xs = [9, 1, 6, 4, 3, 8, 2, 7, 5]
    for i in range(100):
        print(100 - i, xs)
        xs = move(xs)
    i = (xs.index(1) + 1) % len(xs)
    xs = xs[i:] + xs[:i]
    print(f"Part 1: {''.join(str(x) for x in xs)[:-1]}")

if __name__ == "__main__":
    main()
