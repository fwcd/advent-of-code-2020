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

template<typename T>
class Vec2 {
public:
    T x;
    T y;
    
    Vec2(T x, T y) : x(x), y(y) {}
    
    Vec2<T> operator+(Vec2<T> rhs) const {
        return Vec2(x + rhs.x, y + rhs.y);
    }
    
    Vec2<T> operator*(T scale) const {
        return Vec2(x * scale, y * scale);
    }
    
    Vec2<T> rotate(int quarters) const {
        Vec2<T> result{*this};
        for (int i = 0; i < (quarters % 4); i++) {
            int tmp{result.x};
            result.x = -result.y;
            result.y = tmp;
        }
        return result;
    }

    Vec2<T> abs() const {
        return Vec2(std::abs(x), std::abs(y));
    }
};

class Tile {
private:
    std::vector<std::vector<int>> ids;
    std::vector<std::string> lines;
public:
    Tile(const std::vector<std::vector<int>>& ids, const std::vector<std::string>& lines) : ids(ids), lines(lines) {}
    
    char operator[](Vec2<int> pos) const {
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
        return result;
    }
    
    Tile rotate(int quarters) const {
        Vec2<int> offset{0, 0};
        Vec2<int> idsOffset{0, 0};
        Vec2<int> dx{1, 0};
        Vec2<int> dy{0, 1};
        Vec2<int> newSize{static_cast<int>(lines[0].size()), static_cast<int>(lines.size())};
        Vec2<int> newIdSize{static_cast<int>(ids[0].size()), static_cast<int>(ids.size())};

        dx = dx.rotate(quarters);
        dy = dy.rotate(quarters);
        newSize = newSize.rotate(quarters).abs();
        newIdSize = newIdSize.rotate(quarters).abs();

        switch (quarters % 4) {
        case 0:
            offset = Vec2(0, 0);
            idsOffset = Vec2(0, 0);
            break;
        case 1:
            offset = Vec2(newSize.x - 1, 0);
            idsOffset = Vec2(newIdSize.x - 1, 0);
            break;
        case 2:
            offset = Vec2(newSize.x - 1, newSize.y - 1);
            idsOffset = Vec2(newIdSize.x - 1, newIdSize.y - 1);
            break;
        case 3:
            offset = Vec2(0, newSize.y - 1);
            idsOffset = Vec2(0, newIdSize.y - 1);
            break;
        }

        std::vector<std::string> newLines{static_cast<std::size_t>(newSize.y), std::string(static_cast<std::size_t>(newSize.x), ' ')};
        std::vector<std::vector<int>> newIds{static_cast<std::size_t>(newIdSize.y), std::vector(static_cast<std::size_t>(newIdSize.x), 0)};

        for (int y = 0; y < lines.size(); y++) {
            for (int x = 0; x < lines[y].size(); x++) {
                Vec2 target{offset + (dy * y) + (dx * x)};
                newLines[target.y][target.x] = lines[y][x];
            }
        }

        for (int y = 0; y < ids.size(); y++) {
            for (int x = 0; x < ids[y].size(); x++) {
                Vec2 target{idsOffset + (dy * y) + (dx * x)};
                newIds[target.y][target.x] = ids[y][x];
            }
        }
        
        return Tile(ids, newLines);
    }

    /** Tries to glue the top of this tile with the bottom of the other. */
    std::optional<Tile> glue(Tile other) const {
        if (lines[0] == other.lines[other.lines.size() - 1]) {
            std::vector<std::string> combined;
            std::vector<std::vector<int>> combinedIds;
            std::merge(lines.begin(), lines.end(), other.lines.begin(), other.lines.end(), std::back_inserter(combined));
            std::merge(ids.begin(), ids.end(), other.ids.begin(), other.ids.end(), std::back_inserter(combinedIds));
            return Tile(combinedIds, combined);
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

    Tile solve() const {
        std::unordered_set<Tile> remaining{tiles};

        while (remaining.size() > 1) {
            for (const Tile& a : remaining) {
                for (const Tile& b : remaining) {
                    for (int sideA = Top; sideA <= Left; sideA++) {
                        for (int sideB = Top; sideB <= Left; sideB++) {
                            std::optional<Tile> glued{a.rotate(static_cast<Side>(sideA)).glue(b.rotate(static_cast<Side>(sideB)))};
                            if (glued.has_value()) {
                                remaining.erase(a);
                                remaining.erase(b);
                                remaining.insert(*glued);
                                goto outer;
                            }
                        }
                    }
                }
            }
            outer: {}
        }

        return *remaining.begin();
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
    // std::ifstream file{"resources/example.txt"};
    // std::stringstream ss;
    // ss << file.rdbuf();

    // std::string input{ss.str()};
    // Jigsaw jigsaw{parseJigsaw(input)};

    Tile test{
        {{0}},
        {
            "abc",
            "def"
        }
    };
    auto t = test.rotate(3);
    std::cout << t.toString() << std::endl << test.rotate(2).toString() << std::endl;

    // std::cout << jigsaw.getTiles().size() << " tiles parsed!" << std::endl;

    // Tile result{jigsaw.solve()};
    // std::cout << result.toString() << std::endl;

    return 0;
}
