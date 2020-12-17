import Foundation

protocol Point {
    var withNeighbors: [Self] { get }
}

extension Point where Self: Equatable {
    var neighbors: [Self] { withNeighbors.filter { $0 != self } }
}

struct Point3: Point, Hashable {
    let x: Int
    let y: Int
    let z: Int

    var withNeighbors: [Point3] {
        (-1...1).flatMap { dz in
            (-1...1).flatMap { dy in
                (-1...1).map { dx in
                    Point3(x: x + dx, y: y + dy, z: z + dz)
                }
            }
        }
    }
}

struct Point4: Point, Hashable {
    let x: Int
    let y: Int
    let z: Int
    let w: Int

    var withNeighbors: [Point4] {
        (-1...1).flatMap { dw in
            (-1...1).flatMap { dz in
                (-1...1).flatMap { dy in
                    (-1...1).map { dx in
                        Point4(x: x + dx, y: y + dy, z: z + dz, w: w + dw)
                    }
                }
            }
        }
    }
}

struct ConwayGrid<P> where P: Point & Hashable {
    private var active: Set<P> = []
    public var activeCount: Int { active.count }

    public subscript(_ point: P) -> Bool {
        get { active.contains(point) }
        set {
            if newValue {
                active.insert(point)
            } else {
                active.remove(point)
            }
        }
    }

    public func step() -> Self {
        var next = Self()
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
    var grid1 = ConwayGrid<Point3>()
    var grid2 = ConwayGrid<Point4>()

    for (y, line) in input.split(separator: "\n").map(String.init).enumerated() {
        for (x, cell) in line.enumerated() {
            if cell == "#" {
                grid1[Point3(x: x, y: y, z: 0)] = true
                grid2[Point4(x: x, y: y, z: 0, w: 0)] = true
            }
        }
    }

    for i in 0..<6 {
        grid1 = grid1.step()
        grid2 = grid2.step()
        print("at \(i): \(grid1.activeCount) - \(grid2.activeCount)")
    }

    print("Part 1: \(grid1.activeCount)")
    print("Part 2: \(grid2.activeCount)")
}

try main()
