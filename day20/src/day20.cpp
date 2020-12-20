#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

// The Jigsaw-puzzle-solver uses a union-find structure to efficiently
// represent the partially assembled tiles in memory.

enum Side {
    Top = 0,
    Right,
    Bottom,
    Left
};

class Vec2 {
public:
    int x;
    int y;
    
    Vec2(int x, int y) : x(x), y(y) {}
    
    Vec2 operator+(Vec2 rhs) const {
        return Vec2(x + rhs.x, y + rhs.y);
    }
    
    Vec2 operator*(int scale) const {
        return Vec2(x * scale, y * scale);
    }
    
    Vec2 rotate(int quarters) const {
        Vec2 result{*this};
        for (int i = 0; i < (quarters % 4); i++) {
            int tmp = result.x;
            result.x = -result.y;
            result.y = tmp;
        }
        return result;
    }
};

class Tile {
private:
    int parent;
    std::vector<std::string> lines;
public:
    Tile(int parent, const std::vector<std::string>& lines) : parent(parent), lines(lines) {}
    
    char operator[](Vec2 pos) const {
        return lines[pos.y][pos.x];
    }
    
    Vec2 getCorner(Side side) const {
        switch (side) {
        case Top:
            return Vec2(0, 0);
        case Right:
            return Vec2(lines[0].size() - 1, 0);
        case Bottom:
            return Vec2(lines[0].size() - 1, lines.size() - 1);
        case Left:
            return Vec2(0, lines.size() - 1);
        }
    }
    
    Tile rotate(int quarters) const {
        std::vector<std::string> result{lines};
        Vec2 offset{getCorner(Side(quarters % 4))};
        Vec2 dx{Vec2(1, 0).rotate(quarters)};
        Vec2 dy{dx.rotate(1)};
        
        for (int y = 0; y < lines.size(); y++) {
            for (int x = 0; x < lines[y].size(); x++) {
                Vec2 target{offset + (dy * y) + (dx * x)};
                result[target.y][target.x] = lines[y][x];
            }
        }
        
        return Tile(parent, result);
    }
    
    const std::string toString() const {
        std::string result;

        for (const std::string& line : lines) {
            result += line;
            result.push_back('\n');
        }

        return result;
    }
};

class Jigsaw {
private:
    std::vector<Tile> tiles;
public:
    void addTile(const std::vector<std::string>& lines) {
        int i{static_cast<int>(tiles.size())};
        std::vector<std::string> linesMut{lines};
        std::regex headerPattern{"Tile: (\\d+)"};
        std::string header{linesMut[0]};
        linesMut.erase(linesMut.begin());
        Tile tile{i, linesMut};
        tiles.push_back(tile);
    }
    
    int size() const {
        return tiles.size();
    }
    
    const std::vector<Tile>& getTiles() const {
        return tiles;
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

    return jigsaw;
}

int main() {
    std::ifstream file{"resources/example.txt"};
    std::stringstream ss;
    ss << file.rdbuf();

    std::string input{ss.str()};
    Jigsaw jigsaw{parseJigsaw(input)};

    std::cout << jigsaw.getTiles().size() << " tiles parsed!" << std::endl;
    return 0;
}
