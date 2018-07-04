#include "osrm/table_parameters.hpp"

#include "osrm/coordinate.hpp"
#include "osrm/engine_config.hpp"
#include "osrm/json_container.hpp"

#include "osrm/osrm.hpp"
#include "osrm/status.hpp"

#include <exception>
#include <iostream>
#include <string>
#include <utility>

#include <iterator>
#include <vector>
#include <algorithm>
#include <fstream>

#include <cstdlib>

int main(int argc, const char *argv[])
{
    if (argc < 3)
    {
        std::cerr << "Usage: " << argv[0] << " input.txt data.osrm\n";
        return EXIT_FAILURE;
    }

    using namespace osrm;

    // Configure based on a .osrm base path, and no datasets in shared mem from osrm-datastore
    EngineConfig config;

    config.storage_config = {argv[2]};
    config.use_shared_memory = false;

    // We support two routing speed up techniques:
    // - Contraction Hierarchies (CH): requires extract+contract pre-processing
    // - Multi-Level Dijkstra (MLD): requires extract+partition+customize pre-processing
    //
    config.algorithm = EngineConfig::Algorithm::CH;
    // config.algorithm = EngineConfig::Algorithm::MLD;

    // Routing machine with several services (such as Route, Table, Nearest, Trip, Match)
    const OSRM osrm{config};

    // The following attempts to use the Table service; configure this service
    TableParameters params;

    // Read from external file
    //
    int i = 0;
    std::ifstream infile;
    infile.open(argv[1]);
    if(infile.fail()) {
        std::cout << "error" << std::endl;
        return 1;
    }

    int number_of_lines = 0;
    std::string line;
    while(std::getline(infile, line)) {
        ++number_of_lines;
    }

    const int nrow = number_of_lines;
    int col1[nrow];
    std::string col2[nrow];
    float col3[nrow]; // longitude
    float col4[nrow]; // latitude

    infile.close();
    infile.open(argv[1]);
    while(!infile.eof()) {
        infile >> col1[i] >> col2[i] >> col3[i] >> col4[i];
        std::cout << i << ": " << col1[i] << "," << col2[i] << "," << col3[i] << "," << col4[i] << std::endl;

        params.coordinates.push_back({util::FloatLongitude{col3[i]}, util::FloatLatitude{col4[i]}});

        // Define sources and destination by providing indexes
        if(col2[i] == "S") {
            params.sources.push_back(col1[i]);
        }
        else if(col2[i] == "D") {
            params.destinations.push_back(col1[i]);
        }
        else {
            std::cout << "Specify S(ource) or (D)estination in input file." << std::endl;
            return 1;
        }

        ++i;
        if(i > nrow-1) break;
    }

    // Response is in JSON format
    json::Object result;

    // Execute routing request, this does the heavy lifting
    const auto status = osrm.Table(params, result);

    if (status == Status::Ok)
    {
        auto &tables = result.values["durations"].get<json::Array>();

        // Write output
        std::ofstream outfile("../data/osrm-output.csv");
        if(outfile.is_open()) {
            // Loop over tables values
            for(int i = 0; i < tables.values.size(); i++) {
                auto &table = tables.values.at(i).get<json::Array>();
                for(int j = 0; j < table.values.size(); j++) {
                    const auto duration = table.values.at(j).get<json::Number>().value; 

                    // Warn users if extract does not contain the default coordinates from above
                    if (duration == 0)
                    {
                        std::cout << "Note: distance or duration is zero. ";
                        std::cout << "You are probably doing a query outside of the OSM extract.\n\n";
                    }

                    outfile << i << "," << duration << "\n";
                    // std::cout << "From origin " << i << " to destination " << j << ": " << duration
                    //    << " seconds." << std::endl;
                }
            }
            outfile.close();
        }
        else std::cout << "Unable to open file." << std::endl;

        return EXIT_SUCCESS;
    }
    else if (status == Status::Error)
    {
        const auto code = result.values["code"].get<json::String>().value;
        const auto message = result.values["message"].get<json::String>().value;

        std::cout << "Code: " << code << "\n";
        std::cout << "Message: " << code << "\n";
        return EXIT_FAILURE;
    }
}
