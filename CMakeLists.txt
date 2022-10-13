cmake_minimum_required(VERSION 3.16)

# Set basic project properties
project(
    Fortuno
    VERSION 0.1.0
    DESCRIPTION "Fortran unit test objects and drivers"
    LANGUAGES Fortran
)

# Set up install directories
include(GNUInstallDirs)
set(INSTALL_INCLUDEDIR "${CMAKE_INSTALL_INCLUDEDIR}/fortuno")
set(INSTALL_MODULEDIR "${INSTALL_INCLUDEDIR}/fmod")

# Include project specific CMake functions
include(cmake/Fortuno.cmake)

# Include user customizable config settings
include(config.cmake)

# Configure the build type (see cmake/Fortuno.cmake)
configure_build_type()

# Configure the compiler flags (see cmake/Fortuno.cmake)
configure_compiler_flags()

# Get MPI
if(WITH_MPI)
    find_package(MPI REQUIRED)
    if(NOT TARGET MPI::MPI_Fortran)
        message(FATAL_ERROR "Could not find MPI for Fortran compiler ${CMAKE_Fortran_COMPILER}")
  endif()
endif()

# Build library
add_subdirectory(src)

# Build tests
if(WITH_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()


#
# Install package config find, so that other CMake project can find this project
#
include(CMakePackageConfigHelpers)

# If project uses customized finders, they should be installed with it
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortuno
)
endif()

# Install project, add namespace
install(
    EXPORT fortuno-targets
    FILE "fortuno-targets.cmake"
    NAMESPACE "Fortuno::"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortuno"
)

# Create and install CMake package config file
configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/export/fortuno-config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake/fortuno-config.cmake"
    INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortuno"
)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/cmake/fortuno-config-version.cmake"
    VERSION "${PROJECT_VERSION}"
    COMPATIBILITY SameMajorVersion
)
install(
    FILES "${CMAKE_CURRENT_BINARY_DIR}/cmake/fortuno-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake/fortuno-config-version.cmake"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortuno"
)
