import itertools
import functools

with open("resources/input.txt", "r") as f:
    lines = [l.strip() for l in f.readlines()]
    ts = int(lines[0])
    busses = [int(l) if l != "x" else None for l in lines[1].split(",")]
    i = ts
    while not all([(i + k) % b == 0 for k, b in enumerate(busses) if b]):
        l = [b for k, b in enumerate(busses) if b and (i + k) % b == 0]
        prod = 1
        for b in l:
            prod *= b
        i += prod
        print(f'{prod} step @ {i}')
    print(i)
