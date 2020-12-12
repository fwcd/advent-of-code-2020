# A quick solution for day 12 part 2 in Python
# for debugging of the Pascal version

x = 0
y = 0
wx = 10
wy = -1

with open('resources/input.txt', 'r') as f:
    for l in f.readlines():
        l = l.strip()
        i = l[0]
        n = int(l[1:])
        if i == 'N':
            wy -= n
        elif i == 'W':
            wx -= n
        elif i == 'S':
            wy += n
        elif i == 'E':
            wx += n
        elif i == 'F':
            x += wx * n
            y += wy * n
        elif i == 'L':
            for i in range(n // 90):
                (wx, wy) = (wy, -wx)
        elif i == 'R':
            for i in range(n // 90):
                (wx, wy) = (-wy, wx)
        print(f'{l} - pos x: {x}, y: {y}, wp x: {wx}, y: {wy}')
    print(f'Part 2: {abs(x) + abs(y)}')
