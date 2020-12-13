with open("resources/input.txt", "r") as f:
    lines = [l.strip() for l in f.readlines()]
    ts = int(lines[0])
    busses = [int(l) for l in lines[1].split(",") if l != "x"]
    i = ts
    while not any([i % b == 0 for b in busses]):
        i += 1
    bus_id = [b for b in busses if i % b == 0][0]
    print(bus_id * (i - ts))
