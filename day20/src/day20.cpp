#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

// The Jigsaw-puzzle-solver uses a union-find structure to efficiently
// represent the partially assembled tiles in memory.

class Tile {
private:
    int parent;
    std::vector<std::string> lines;
public:
    Tile(int parent, const std::vector<std::string>& lines) : parent(parent), lines(lines) {}
};

class Jigsaw {
private:
    std::vector<Tile> tiles;
public:
    void addTile(const std::vector<std::string>& lines) {
        int i{static_cast<int>(tiles.size())};
        Tile tile{i, lines};
        tiles.push_back(tile);
    }
    
    int size() {
        return tiles.size();
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
    
    if (last < s.size()) {
        result.push_back(s.substr(last, pos));
    }

    return result;
}

Jigsaw parseJigsaw(const std::string& raw) {
    Jigsaw jigsaw;

    for (auto rawTile : split(raw, "\n\n")) {
        jigsaw.addTile(split(rawTile, " "));
    }

    return jigsaw;
}

int main() {
    std::ifstream file{"resources/example.txt"};
    std::stringstream ss;
    ss << file.rdbuf();

    std::string input{ss.str()};
    Jigsaw jigsaw{parseJigsaw(input)};

    std::cout << jigsaw.size() << " tiles parsed!" << std::endl;
    return 0;
}
