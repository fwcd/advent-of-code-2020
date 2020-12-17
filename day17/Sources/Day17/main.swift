import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
    let z: Int

    var withNeighbors: [Point] {
        (-1...1).flatMap { dz in
            (-1...1).flatMap { dy in
                (-1...1).map { dx in
                    Point(x: x + dx, y: y + dy, z: z + dz)
                }
            }
        }
    }
    var neighbors: [Point] {
        withNeighbors.filter { $0 != self }
    }
}

struct ConwayGrid {
    private var active: Set<Point> = []
    public var activeCount: Int { active.count }

    public subscript(_ z: Int, _ y: Int, _ x: Int) -> Bool {
        get { self[Point(x: x, y: y, z: z)] }
        set { self[Point(x: x, y: y, z: z)] = true }
    }

    public subscript(_ point: Point) -> Bool {
        get { active.contains(point) }
        set {
            if newValue {
                active.insert(point)
            } else {
                active.remove(point)
            }
        }
    }

    public func step() -> ConwayGrid {
        var next = ConwayGrid()
        for point in Set(active.flatMap(\.withNeighbors)) {
            let isActive = self[point]
            let activeNbCount = point.neighbors.filter { self[$0] }.count

            if isActive {
                next[point] = [2, 3].contains(activeNbCount)
            } else if activeNbCount == 3 {
                next[point] = true
            }
        }
        return next
    }
}

func main() throws {
    let input = try String(contentsOf: URL(fileURLWithPath: "Resources/input.txt"))
    var grid = ConwayGrid()

    for (y, line) in input.split(separator: "\n").map(String.init).enumerated() {
        for (x, cell) in line.enumerated() {
            if cell == "#" {
                grid[0, y, x] = true
            }
        }
    }

    for i in 0..<6 {
        grid = grid.step()
        print("at \(i): \(grid.activeCount)")
    }

    print("Part 1: \(grid.activeCount)")
}

try main()
