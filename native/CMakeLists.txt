cmake_minimum_required(VERSION 3.18 FATAL_ERROR)

project(mesha)

set(CMAKE_CXX_STANDARD 11)

set(LIB_DIR "${CMAKE_CURRENT_LIST_DIR}"
    CACHE PATH "libmesha Directory")

set(BLEND2D_DIR "${LIB_DIR}/../extern/blend2d"
    CACHE PATH "Location of 'blend2d'")

set(BLEND2D_STATIC FALSE)
include("${BLEND2D_DIR}/CMakeLists.txt")

# find_package(SDL2 REQUIRED CONFIG REQUIRED COMPONENTS SDL2)
# find_package(SDL2 REQUIRED CONFIG COMPONENTS SDL2main)

add_library(mesha SHARED lib.cpp)

target_link_libraries(mesha PRIVATE blend2d::blend2d)

# if(TARGET SDL2::SDL2main)
#     # It has an implicit dependency on SDL2 functions, so it MUST be added before SDL2::SDL2 (or SDL2::SDL2-static)
#     target_link_libraries(mesha PRIVATE SDL2::SDL2main)
# endif()

# target_link_libraries(mesha PRIVATE SDL2::SDL2)
