cmake_minimum_required(VERSION 3.28)
project(mesha)

# Create an option to switch between a system sdl library and a vendored sdl library
option(MESHA_VENDORED "Use vendored libraries" OFF)

# SDL2
if(MESHA_VENDORED)
  add_subdirectory(extern/sdl EXCLUDE_FROM_ALL)
else()
  # 1. Look for a SDL2 package, 2. look for the SDL2 component and 3. fail if none can be found
  find_package(SDL2 REQUIRED CONFIG REQUIRED COMPONENTS SDL2)

  # 1. Look for a SDL2 package, 2. Look for the SDL2maincomponent and 3. DO NOT fail when SDL2main is not available
  find_package(SDL2 REQUIRED CONFIG COMPONENTS SDL2main)
endif()

add_executable(mesha engine/main.cpp
                     engine/ui.cpp
                     engine/vm.cpp
                     # imgui
                     extern/imgui/imgui.cpp
                     extern/imgui/imgui_draw.cpp
                     extern/imgui/imgui_tables.cpp
                     extern/imgui/imgui_widgets.cpp
                     extern/imgui/backends/imgui_impl_sdlrenderer2.cpp
                     extern/imgui/backends/imgui_impl_sdl2.cpp
                     # janet
                     extern/janet/build/c/janet.c)

set_property(TARGET mesha PROPERTY CXX_STANDARD 17)


# Dear ImGUI
target_include_directories(mesha PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/engine"
                                         "${CMAKE_CURRENT_SOURCE_DIR}/extern/imgui"
                                         "${CMAKE_CURRENT_SOURCE_DIR}/extern/imgui/backends"
                                         "${CMAKE_CURRENT_SOURCE_DIR}/extern/janet/build/"
                                         "${CMAKE_CURRENT_SOURCE_DIR}/extern/readerwriterqueue")

# SDL2::SDL2main may or may not be available. It is e.g. required by Windows GUI applications
if(TARGET SDL2::SDL2main)
  # It has an implicit dependency on SDL2 functions, so it MUST be added before SDL2::SDL2 (or SDL2::SDL2-static)
  target_link_libraries(mesha PRIVATE SDL2::SDL2main)
endif()

# Link to the actual SDL2 library. SDL2::SDL2 is the shared SDL library, SDL2::SDL2-static is the static SDL libarary.
target_link_libraries(mesha PRIVATE SDL2::SDL2)
