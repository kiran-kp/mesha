cmake_minimum_required(VERSION 3.28)
project(mesha)

find_package(SDL3 REQUIRED CONFIG REQUIRED COMPONENTS SDL3-shared)

add_executable(mesha engine/main.cpp
                     engine/ui.cpp
                     engine/vm.cpp
                     # imgui
                     extern/imgui/imgui.cpp
                     extern/imgui/imgui_draw.cpp
                     extern/imgui/imgui_tables.cpp
                     extern/imgui/imgui_widgets.cpp
                     extern/imgui/backends/imgui_impl_sdl3.cpp
                     extern/imgui/backends/imgui_impl_sdlrenderer3.cpp
                     # janet
                     extern/janet/build/c/janet.c)

set_property(TARGET mesha PROPERTY CXX_STANDARD 20)


target_include_directories(mesha PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/engine"
                                         "${CMAKE_CURRENT_SOURCE_DIR}/extern/imgui"
                                         "${CMAKE_CURRENT_SOURCE_DIR}/extern/imgui/backends"
                                         "${CMAKE_CURRENT_SOURCE_DIR}/extern/janet/build/"
                                         "${CMAKE_CURRENT_SOURCE_DIR}/extern/readerwriterqueue")

target_link_libraries(mesha PRIVATE SDL3::SDL3)
