cmake_minimum_required(VERSION 3.25)

project(test-coarray LANGUAGES Fortran)

# Basic setup
if (NOT CMAKE_Fortran_MODULE_DIRECTORY)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fortran_mods)
endif ()
include(FetchContent)

# Get fortuno
set(FORTUNO_COARRAY ON CACHE BOOL "test-mpi: Override" FORCE)
FetchContent_Declare(Fortuno
        GIT_REPOSITORY https://github.com/aradi/fortuno
        GIT_TAG main
        # TODO: Add COMPONENTS Coarray
        FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(Fortuno)

# Create demo library
add_library(mylib)
add_subdirectory(src)

# Create test app
enable_testing()
add_subdirectory(test)
