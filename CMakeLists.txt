cmake_minimum_required(VERSION 3.0)
project(expected VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_library(lib-expected INTERFACE)

target_include_directories(lib-expected
    INTERFACE include
)

if (BUILD_TESTS)
    add_subdirectory(test)
endif()

