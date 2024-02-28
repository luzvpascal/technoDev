#ifndef CSVPARSER_H
#define CSVPARSER_H

#include <vector>
#include <string>

class CSVParser {
public:
    CSVParser(const std::string& filename, char delimiter = ',');

    bool readCSV(std::vector<std::vector<std::string>>& data);

private:
    std::string filename;
    char delimiter;
};

#endif
