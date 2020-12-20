#include <fstream>
#include <iostream>
#include <optional>
#include <regex>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>

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
    std::vector<std::vector<int>> ids;
    std::vector<std::string> lines;
public:
    Tile(const std::vector<std::vector<int>>& ids, const std::vector<std::string>& lines) : ids(ids), lines(lines) {}
    
    char operator[](Vec2 pos) const {
        return lines[pos.y][pos.x];
    }

    bool operator==(Tile other) const {
        return ids == other.ids && lines == other.lines;
    }

    std::size_t hash() const {
        std::size_t result{0};
        for (const std::vector<int>& idsLine : ids) {
            for (int id : idsLine) {
                result ^= std::hash<int>()(id);
            }
        }
        for (const std::string& line : lines) {
            result ^= std::hash<std::string>()(line);
        }
        return result;
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
        
        return Tile(ids, result);
    }

    /** Tries to glue the top of this tile with the bottom of the other. */
    std::optional<Tile> glue(Tile other) const {
        if (lines[0] == other.lines[other.lines.size() - 1]) {
            std::vector<std::string> combined;
            std::merge(lines.begin(), lines.end(), other.lines.begin(), other.lines.end(), std::back_inserter(combined));
            return Tile(ids, combined);
        } else {
            return std::nullopt;
        }
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

namespace std {
    template<>
    struct hash<Tile> {
        std::size_t operator()(const Tile& tile) const {
            return tile.hash();
        }
    };
}

class Jigsaw {
private:
    std::unordered_set<Tile> tiles;
public:
    void addTile(const std::vector<std::string>& lines) {
        int i{static_cast<int>(tiles.size())};
        std::vector<std::string> linesMut{lines};
        std::regex headerPattern{"Tile: (\\d+)"};
        std::string header{linesMut[0]};
        linesMut.erase(linesMut.begin());
        Tile tile{{{i}}, linesMut};
        tiles.insert(tile);
    }
    
    int size() const {
        return tiles.size();
    }
    
    const std::unordered_set<Tile>& getTiles() const {
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
