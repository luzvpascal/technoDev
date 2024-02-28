cmake_minimum_required(VERSION 3.22)
project(MCUAMS)

set(CMAKE_CXX_STANDARD 14)

# Create a variable containing the source files directory
set(SRC_DIR "${CMAKE_SOURCE_DIR}/src")

# Use file globbing to find all source files in the src directory
file(GLOB SRC_FILES "${SRC_DIR}/*.cpp" "${SRC_DIR}/*.h")

# Add the executable and include all source files
add_executable(MCUAMS ${SRC_FILES})
