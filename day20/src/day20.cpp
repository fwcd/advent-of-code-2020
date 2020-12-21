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
        std::stringstream sts;
        sts << '(' << x << ", " << y << ')';
        return sts.str();
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
public:
    std::vector<std::string> lines;
    unsigned long long id;
    bool flipped;
    int rotation;
    std::unordered_map<Side, std::optional<int>> neighbors;

    Tile(unsigned long long id, const std::vector<std::string>& lines) :
        lines(lines),
        id(id),
        flipped(false),
        rotation(0),
        neighbors({{Top, std::nullopt}, {Left, std::nullopt}, {Bottom, std::nullopt}, {Right, std::nullopt}}) {}
    
    std::string rawRow(int i) const {
        return lines[i];
    }

    std::string rawColumn(int i) const {
        std::string result;
        for (int y = 0; y < lines.size(); y++) {
            result.push_back(lines[y][i]);
        }
        return result;
    }

    std::string row(int i) const {
        std::string result;
        bool dir = rotation == Top || rotation == Right;
        bool flip = flipped ^ (rotation == Right || rotation == Bottom);
        if (rotation % 2 == 0) {
            result = rawRow(dir ? i : (lines.size() - 1) - i);
        } else {
            result = rawColumn(dir ? i : (lines[0].size() - 1) - i);
        }
        if (flip) {
            std::reverse(result.begin(), result.end());
        }
        return result;
    }

    std::string column(int i) const {
        std::string result;
        for (int y = 0; y < height(); y++) {
            result.push_back(row(y)[i]);
        }
        return result;
    }

    std::string getEdge(Side side) {
        switch (side) {
        case Top:
            return row(0);
        case Bottom:
            return row(height() - 1);
        case Left:
            return column(0);
        case Right:
            return column(width() - 1);
        }
    }

    int width() const {
        return rotation % 2 == 0 ? lines[0].size() : lines.size();
    }

    int height() const {
        return rotation % 2 == 0 ? lines.size() : lines[0].size();
    }

    const std::string str() const {
        std::string result;

        for (int i = 0; i < height(); i++) {
            result += row(i);
            result.push_back('\n');
        }

        return result;
    }
};

// The jigsaw solver uses backtracking

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

    Tile& tileAt(Vec2 pos) {
        return tiles[grid[pos.y][pos.x]];
    }

    bool solve() {
        int i{0};
        int x{sideLength / 2};
        grid[x][x] = i;
        minCorner = Vec2(x, x);
        maxCorner = Vec2(x, x);
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

    bool solve(std::unordered_set<int>& used, bool first = true) {
        int count{static_cast<int>(tiles.size())};
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
                                    for (bool flip : {false, true}) {
                                        tiles[i].rotation = rot;
                                        tiles[i].flipped = flip;

                                        if (canPlace(i, x, y)) {
                                            grid[y][x] = i;
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
                                            grid[y][x] = -1;
                                            maxCorner = prevMax;
                                            minCorner = prevMin;
                                        }
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

    void printVerbose() {
        std::cout << fullMap(true, true);
    }

    std::string fullMap(bool withSpaces = false, bool withBorder = false) {
        if (!isRectangular()) throw std::runtime_error("Can only print rectangular grids verbosely!");
        std::stringstream sts;

        int tileWidth = tiles[0].width();
        int tileHeight = tiles[0].height();

        int pad = withBorder ? 0 : 1;

        for (int y = minCorner.y; y <= maxCorner.y; y++) {
            for (int ty = pad; ty < tileHeight - pad; ty++) {
                for (int x = minCorner.x; x <= maxCorner.x; x++) {
                    for (int tx = pad; tx < tileWidth - pad; tx++) {
                        sts << tiles[grid[y][x]].lines[ty][tx];
                    }
                    if (withSpaces) {
                        sts << ' ';
                    }
                }
                sts << std::endl;
            }
            if (withSpaces) {
                sts << std::endl;
            }
        }

        return sts.str();
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

std::string readFile(const char* path) {
    std::ifstream file{path};
    std::stringstream sts;
    sts << file.rdbuf();
    return sts.str();
}

int main() {
    // Parse the input

    std::string input{readFile("resources/example.txt")};
    Jigsaw jigsaw{parseJigsaw(input)};
    std::cout << jigsaw.tiles.size() << " tiles parsed!" << std::endl;

    // Solve the puzzle

    bool solved{jigsaw.solve()};
    std::cout << "Solved: " << solved << std::endl;
    if (!solved) return -1;
    
    // Find the corners

    Vec2 tl{jigsaw.minCorner};
    Vec2 br{jigsaw.maxCorner};
    Vec2 tr{br.x, tl.y};
    Vec2 bl{tl.x, br.y};

    unsigned long long tlId = jigsaw.tileAt(tl).id, trId = jigsaw.tileAt(tr).id, blId = jigsaw.tileAt(bl).id, brId = jigsaw.tileAt(br).id;
    std::cout << "Corners: " << tlId << ", " << trId << ", " << blId << ", " << brId << std::endl;
    std::cout << "Part 1: " << (tlId * trId * blId * brId) << std::endl;

    // Parse the sea monster

    std::string rawMonster{readFile("resources/seaMonster.txt")};
    Tile monster{0, split(rawMonster, "\n")};
    
    // Search for sea monsterso

    Tile map{0, split(jigsaw.fullMap(), "\n")};

    for (int rot = 0; rot < 4; rot++) {
        for (bool flip : {false, true}) {
            monster.rotation = rot;
            monster.flipped = flip;

        }
    }

    return 0;
}
