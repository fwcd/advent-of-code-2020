#include <cmath>
#include <exception>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <string>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

struct Vec2 {
    int x;
    int y;

    Vec2() : Vec2(0, 0) {}

    Vec2(int x, int y) : x(x), y(y) {}

    Vec2 operator+(Vec2 rhs) {
        return Vec2(x + rhs.x, y + rhs.y);
    }

    std::string str() {
        std::stringstream ss;
        ss << '(' << x << ", " << y << ')';
        return ss.str();
    }
};

enum Side {
    Top = 0,
    Right,
    Bottom,
    Left
};

Side rotateSide(Side s, int n) {
    return static_cast<Side>((s + n) % 4);
}

Side oppositeSide(Side s) {
    return rotateSide(s, 2);
}

Vec2 sideToDir(Side s) {
    switch (s) {
    case Top:
        return Vec2(0, -1);
    case Right:
        return Vec2(1, 0);
    case Bottom:
        return Vec2(0, 1);
    case Left:
        return Vec2(-1, 0);
    }
}

std::string sideToString(Side s) {
    switch (s) {
    case Top:
        return "Top";
    case Right:
        return "Right";
    case Bottom:
        return "Bottom";
    case Left:
        return "Left";
    }
}

class Tile {
private:
    std::vector<std::string> lines;
public:
    unsigned long long id;
    int parent;
    bool flipped;
    int rotation;
    std::unordered_map<Side, std::optional<int>> neighbors;

    Tile(unsigned long long id, const std::vector<std::string>& lines) :
        id(id),
        lines(lines),
        flipped(false),
        rotation(0),
        neighbors({{Top, std::nullopt}, {Left, std::nullopt}, {Bottom, std::nullopt}, {Right, std::nullopt}}) {}

    std::string getEdge(Side side) {
        std::string result;

        switch (rotateSide(side, rotation)) {
        case Top:
            result = lines.front();
            break;
        case Bottom:
            result = lines.back();
            break;
        case Left:
            for (int y = 0; y < lines.size(); y++) {
                result.push_back(lines[y].front());
            }
            break;
        case Right:
            for (int y = 0; y < lines.size(); y++) {
                result.push_back(lines[y].back());
            }
            break;
        }

        if (flipped) {
            std::reverse(result.begin(), result.end());
        }

        return result;
    }

    void flip() {
        flipped = !flipped;
    }

    const std::string str() const {
        std::string result;

        for (const std::string& line : lines) {
            result += line;
            result.push_back('\n');
        }

        return result;
    }
};

// The jigsaw solver uses a union-find structure for efficiency

class Jigsaw {
private:
    int puzzleSideLength;
public:
    std::vector<std::vector<int>> grid;
    std::vector<Tile> tiles;
    int sideLength;
    Vec2 minCorner;
    Vec2 maxCorner;

    void addTile(const std::vector<std::string>& lines) {
        std::vector<std::string> linesMut{lines};
        std::regex headerPattern{"Tile (\\d+):"};
        std::string header{linesMut.front()};
        std::smatch matches;

        std::regex_search(header, matches, headerPattern);
        auto id{static_cast<unsigned long long>(std::stoi(matches[1]))};

        linesMut.erase(linesMut.begin());
        while (linesMut.back().empty()) {
            linesMut.pop_back();
        }

        Tile tile{id, linesMut};
        tiles.push_back(tile);
    }

    void setUp() {
        puzzleSideLength = static_cast<int>(std::sqrt(tiles.size())) + 1;
        sideLength = 2 * puzzleSideLength;
        grid = std::vector(sideLength, std::vector(sideLength, -1));
        minCorner = Vec2(sideLength - 1, sideLength - 1);
    }

    bool solve() {
        int i{0};
        grid[sideLength / 2][sideLength / 2] = i;
        printGrid();
        std::unordered_set<int> used = {i};
        return solve(used);
    }

    int get(int y, int x) {
        if (y < 0 || y >= sideLength || x < 0 || x >= sideLength) {
            return -1;
        }
        return grid[y][x];
    }

    bool linesUp(int i, int nx, int ny, Side s1, Side s2) {
        int j{get(ny, nx)};
        if (j < 0) {
            return true;
        }
        return tiles[i].getEdge(s1) == tiles[j].getEdge(s2);
    }

    bool canPlace(int i, int x, int y) {
        return (get(y - 1, x) + get(y + 1, x) + get(y, x - 1) + get(y, x + 1) > -4)
            && linesUp(i, x - 1, y, Left, Right)
            && linesUp(i, x + 1, y, Right, Left)
            && linesUp(i, x, y - 1, Top, Bottom)
            && linesUp(i, x, y + 1, Bottom, Top);
    }

    bool isRectangular() {
        Vec2 min{minCorner};
        Vec2 max{maxCorner};
        for (int y = min.y; y <= max.y; y++) {
            for (int x = min.x; x <= max.x; x++) {
                if (grid[y][x] < 0) {
                    return false;
                }
            }
        }
        return true;
    }

    void printGrid() {
        for (int y = 0; y < sideLength; y++) {
            for (int x = 0; x < sideLength; x++) {
                if (grid[y][x] < 0) {
                    std::cout << ".... ";
                } else {
                    std::cout << tiles[grid[y][x]].id << ' ';
                }
            }
            std::cout << std::endl;
        }
        std::cout << std::endl;
    }

    bool solve(std::unordered_set<int>& used, bool first = true) {
        int count{static_cast<int>(tiles.size())};
        // if ((used.size() > 1) && (((maxCorner.x - minCorner.x) > puzzleSideLength) || ((maxCorner.y - minCorner.y) > puzzleSideLength))) {
        //     std::cout << "Rejected at " << used.size() << " (sl: " << puzzleSideLength << ") max: " << maxCorner.str() << ", min: " << minCorner.str() << std::endl;
        //     return false;
        // }
        if (used.size() >= count) {
            printGrid();
            return isRectangular();
        } else {
            for (int i = 1; i < count; i++) {
                if (!used.contains(i)) {
                    for (int y = 0; y < sideLength; y++) {
                        for (int x = 0; x < sideLength; x++) {
                            if (grid[y][x] < 0) {
                                for (int rot = 0; rot < 4; rot++) {
                                    for (bool flipped : {false, true}) {
                                        tiles[i].rotation = rot;
                                        tiles[i].flipped = flipped;

                                        grid[y][x] = i;
                                        printGrid();
                                        if (canPlace(i, x, y)) {
                                            Vec2 prevMax{maxCorner};
                                            Vec2 prevMin{minCorner};
                                            if (x > maxCorner.x) maxCorner.x = x;
                                            if (x < minCorner.x) minCorner.x = x;
                                            if (y > maxCorner.y) maxCorner.y = y;
                                            if (y < minCorner.y) minCorner.y = y;
                                            used.insert(i);
                                            if (solve(used, false)) {
                                                return true;
                                            }
                                            used.erase(i);
                                            maxCorner = prevMax;
                                            minCorner = prevMin;
                                        }
                                        grid[y][x] = -1;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return false;
        }
    }
};

std::vector<std::string> split(const std::string& s, const std::string& delim) {
    size_t last{0};
    size_t pos{0};
    std::vector<std::string> result;

    while ((pos = s.find(delim, last)) != std::string::npos) {
        result.push_back(s.substr(last, pos - last));
        last = pos + delim.size();
    }
    
    result.push_back(s.substr(last, pos));
    return result;
}

Jigsaw parseJigsaw(const std::string& raw) {
    Jigsaw jigsaw;

    for (auto rawTile : split(raw, "\n\n")) {
        jigsaw.addTile(split(rawTile, "\n"));
    }

    jigsaw.setUp();
    return jigsaw;
}

int main() {
    std::ifstream file{"resources/example2.txt"};
    std::stringstream ss;
    ss << file.rdbuf();

    std::string input{ss.str()};
    Jigsaw jigsaw{parseJigsaw(input)};

    std::cout << jigsaw.tiles.size() << " tiles parsed!" << std::endl;

    bool solved{jigsaw.solve()};

    std::cout << "Solved: " << solved << std::endl;
    if (!solved) return -1;
    
    // Find the corners

    Vec2 tl, tr, bl, br;
    int tlTile, trTile, blTile, brTile;

    for (int y = 0; y < jigsaw.sideLength; y++) {
        for (int x = 0; x < jigsaw.sideLength; x++) {
            Vec2 pos{x, y};
            int i = jigsaw.grid[y][x];

            if (i >= 0) {
                if (pos.x < tl.x || pos.y < tl.y) {
                    tl = pos;
                    tlTile = i;
                }
                if (pos.x > tr.x || pos.y < tr.y) {
                    tr = pos;
                    trTile = i;
                }
                if (pos.x < bl.x || pos.y > bl.y) {
                    bl = pos;
                    blTile = i;
                }
                if (pos.x > br.x || pos.y > br.y) {
                    br = pos;
                    brTile = i;
                }
            }
        }
    }

    unsigned long long tlId = jigsaw.tiles[tlTile].id, trId = jigsaw.tiles[trTile].id, blId = jigsaw.tiles[blTile].id, brId = jigsaw.tiles[brTile].id;
    std::cout << "Corners: " << tlId << ", " << trId << ", " << blId << ", " << brId << std::endl;
    std::cout << "Part 1: " << (tlId * trId * blId * brId) << std::endl;

    return 0;
}
