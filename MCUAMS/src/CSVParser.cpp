#include "CSVParser.h"
#include <iostream>
#include <fstream>
#include <sstream>

CSVParser::CSVParser(const std::string& filename, char delimiter)
        : filename(filename), delimiter(delimiter) {}

bool CSVParser::readCSV(std::vector<std::vector<std::string>>& data) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open the file for reading." << std::endl;
        return false;
    }

    data.clear(); // Clear the data vector

    std::string line;
    while (std::getline(file, line)) {
        std::vector<std::string> row;
        std::istringstream lineStream(line);
        std::string cell;

        while (std::getline(lineStream, cell, delimiter)) {
            row.push_back(cell);
        }

        data.push_back(row);
    }

    file.close();
    return true;
}
