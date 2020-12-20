#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

int main() {
    std::ifstream file{"resources/input.txt"};
    std::stringstream ss;
    ss << file.rdbuf();
    std::string input{ss.str()};

    std::cout << input << std::endl;
    return 0;
}
