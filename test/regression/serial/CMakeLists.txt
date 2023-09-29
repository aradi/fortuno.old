cmake_minimum_required(VERSION 3.25)

project(test-serial LANGUAGES Fortran)

# Basic setup
if (NOT CMAKE_Fortran_MODULE_DIRECTORY)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fortran_mods)
endif ()
include(FetchContent)

# Get fortuno
FetchContent_Declare(Fortuno
        GIT_REPOSITORY https://github.com/aradi/fortuno
        GIT_TAG main
        FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(Fortuno)

# Create demo library
add_library(mylib)
add_subdirectory(src)

# Create test app
enable_testing()
add_subdirectory(test)
