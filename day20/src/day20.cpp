#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <string>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

enum Side {
    Top = 0,
    Right,
    Bottom,
    Left
};

Side oppositeSide(Side s) {
    return static_cast<Side>((s + 2) % 4);
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
    int id;
    int parent;
    std::unordered_map<Side, std::optional<int>> neighbors;

    Tile(int parent, int id, const std::vector<std::string>& lines) :
        id(id),
        lines(lines),
        parent(parent),
        neighbors({{Top, std::nullopt}, {Left, std::nullopt}, {Bottom, std::nullopt}, {Right, std::nullopt}}) {}

    std::string getEdge(Side side) {
        std::string result;

        switch (side) {
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

        return result;
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

// The jigsaw solver uses a union-find structure for efficiency

class Jigsaw {
private:
    std::unordered_set<int> roots;
public:
    std::vector<Tile> tiles;

    void addTile(const std::vector<std::string>& lines) {
        int i{static_cast<int>(tiles.size())};
        std::vector<std::string> linesMut{lines};
        std::regex headerPattern{"Tile (\\d+):"};
        std::string header{linesMut.front()};
        std::smatch matches;

        std::regex_search(header, matches, headerPattern);
        int id{std::stoi(matches[1])};

        linesMut.erase(linesMut.begin());
        while (linesMut.back().empty()) {
            linesMut.pop_back();
        }

        Tile tile{i, id, linesMut};
        tiles.push_back(tile);
        roots.insert(i);
    }

    void solve() {
        while (roots.size() > 1) {
            int count = tiles.size();
            for (int i = 0; i < count; i++) {
                for (int j = 0; j < count; j++) {
                    if (findRoot(i) != findRoot(j)) {
                        for (int sa = Top; sa <= Left; sa++) {
                            for (int sb = Top; sb <= Left; sb++) {
                                Side sideA{static_cast<Side>(sa)};
                                Side sideB{static_cast<Side>(sb)};

                                if (tiles[i].getEdge(sideA) == tiles[j].getEdge(sideB)) {
                                    connect(i, j, sideA, sideB);
                                    std::cout << roots.size() << " disjoint pieces (joined " << tiles[i].id << " " << sideToString(sideA) << " with "
                                                                                             << tiles[j].id << " " << sideToString(sideB) << ")!" << std::endl;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    int findRoot(int i) const {
        const Tile& tile{tiles[i]};
        if (tile.parent != i) {
            return findRoot(tile.parent);
        } else {
            return i;
        }
    }

    void connect(int i, int j, Side sideA, Side sideB) {
        int root1{findRoot(i)};
        int root2{findRoot(j)};

        roots.erase(root2);
        roots.insert(root1);

        tiles[i].neighbors[sideA] = j;
        tiles[j].neighbors[sideB] = i;
        tiles[i].parent = root1;
        tiles[j].parent = root1;
    }
    
    std::unordered_set<int> findRoots() const {
        std::unordered_set<int> roots;

        for (int i = 0; i < tiles.size(); i++) {
            roots.insert(findRoot(i));
        }

        return roots;
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

    std::cout << jigsaw.tiles.size() << " tiles parsed!" << std::endl;

    jigsaw.solve();

    return 0;
}
