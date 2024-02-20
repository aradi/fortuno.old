cmake_minimum_required(VERSION 3.16)
# CMake version compatibility
# TODO: Remove when cmake 3.25 is commonly distributed
if (POLICY CMP0140)
	cmake_policy(SET CMP0140 NEW)
endif ()

#[==============================================================================================[
#                                    Basic project defintion                                    #
]==============================================================================================]
project(
		Fortuno
		VERSION 0.2.0
		DESCRIPTION "Fortran unit test objects and drivers"
		LANGUAGES Fortran
)
# Back-porting to PROJECT_IS_TOP_LEVEL to older cmake
# TODO: Remove when requiring cmake >= 3.21
if (NOT DEFINED Spglib_IS_TOP_LEVEL)
	if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
		set(PROJECT_IS_TOP_LEVEL ON)
	else ()
		set(PROJECT_IS_TOP_LEVEL OFF)
	endif ()
endif ()

if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif ()

#[==============================================================================================[
#                                            Options                                            #
]==============================================================================================]

option(FORTUNO_TESTS "Fortuno: Build unit tests of the Fortuno library" ${PROJECT_IS_TOP_LEVEL})
option(FORTUNO_SHARED_LIBS "Fortuno: Build as a shared library" ${PROJECT_IS_TOP_LEVEL})
option(FORTUNO_INSTALL "Fortuno: Install project" ${PROJECT_IS_TOP_LEVEL})
option(FORTUNO_MPI "Fortuno: Build with MPI support" OFF)
option(FORTUNO_COARRAY "Fortuno: Enable coarray support" OFF)
mark_as_advanced(FORTUNO_COARRAY)

#[==============================================================================================[
#                                     Project configuration                                     #
]==============================================================================================]

# Compatibilities
# TODO: Remove when requiring cmake >= 3.20
if (NOT COMMAND cmake_path)
	# Note: This is not an actual replacement of cmake_path, just a minimal implementation for
	# this project
	include(cmake/compat/cmake_path.cmake)
endif ()

set(BUILD_SHARED_LIBS ${FORTUNO_SHARED_LIBS})

if (NOT CMAKE_Fortran_MODULE_DIRECTORY)
	set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fortran_mods)
endif ()

if (FORTUNO_INSTALL)
	include(CMakePackageConfigHelpers)
	if (UNIX)
		include(GNUInstallDirs)
	endif ()
	# CMake does not properly support fortran module installation paths.
	# Adapting the standard from fortran-stdlib
	# https://gitlab.kitware.com/cmake/cmake/-/issues/19608
	# https://discourse.cmake.org/t/api-design-c-modules-source-listings-and-interface-properties/5389/14
	cmake_path(APPEND CMAKE_INSTALL_INCLUDEDIR  "${CMAKE_Fortran_COMPILER_ID}-${CMAKE_Fortran_COMPILER_VERSION}" ${PROJECT_NAME}
			OUTPUT_VARIABLE _DEFAULT_CMAKE_INSTALL_MODULEDIR)
	set(CMAKE_INSTALL_MODULEDIR ${_DEFAULT_CMAKE_INSTALL_MODULEDIR}
			CACHE STRING
			"Fortran module installation path (Not a cmake native variable)"
	)
	cmake_path(IS_ABSOLUTE CMAKE_INSTALL_MODULEDIR _is_absolute)
	if (_is_absolute)
		set(CMAKE_INSTALL_FULL_MODULEDIR ${CMAKE_INSTALL_MODULEDIR})
	else ()
		cmake_path(APPEND CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_MODULEDIR}
				OUTPUT_VARIABLE CMAKE_INSTALL_FULL_MODULEDIR)
	endif ()
endif ()

#[==============================================================================================[
#                                       External packages                                       #
]==============================================================================================]

if (FORTUNO_MPI)
	find_package(MPI REQUIRED COMPONENTS Fortran)
endif ()

#[==============================================================================================[
#                                        Main definition                                        #
]==============================================================================================]


add_library(Fortuno_Fortuno)
set_target_properties(Fortuno_Fortuno PROPERTIES
		VERSION ${PROJECT_VERSION}
		# TODO: When stable version is released switch this to ${PROJECT_VERSION_MAJOR}
		SOVERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
		OUTPUT_NAME fortuno
		EXPORT_NAME Fortuno
)
add_library(Fortuno::Fortuno ALIAS Fortuno_Fortuno)
if(FORTUNO_MPI)
	add_library(Fortuno_MPI)
	set_target_properties(Fortuno_MPI PROPERTIES
			OUTPUT_NAME fortuno-mpi
			EXPORT_NAME MPI
	)
	add_library(Fortuno::MPI ALIAS Fortuno_MPI)
endif()
if(FORTUNO_COARRAY)
	add_library(Fortuno_Coarray)
	set_target_properties(Fortuno_Coarray PROPERTIES
			OUTPUT_NAME fortuno-coarray
			EXPORT_NAME Coarray
	)
	add_library(Fortuno::Coarray ALIAS Fortuno_Coarray)
endif()

# Main project
add_subdirectory(src)

# Testing
if (FORTUNO_TESTS)
	enable_testing()
	add_subdirectory(test)
endif ()

#[==============================================================================================[
#                                       Install or Export                                       #
]==============================================================================================]

if (FORTUNO_INSTALL)
	# Create and install CMake package config file
	write_basic_package_version_file(
			${CMAKE_CURRENT_BINARY_DIR}/FortunoConfigVersion.cmake
			VERSION ${PROJECT_VERSION}
			# TODO: Change to SameMajorVersion when stable release
			COMPATIBILITY SameMinorVersion
	)
	configure_package_config_file(
			cmake/FortunoConfig.cmake.in
			${CMAKE_CURRENT_BINARY_DIR}/FortunoConfig.cmake
			INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Fortuno
	)
	install(FILES
			${CMAKE_CURRENT_BINARY_DIR}/FortunoConfig.cmake
			${CMAKE_CURRENT_BINARY_DIR}/FortunoConfigVersion.cmake
			DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Fortuno
			COMPONENT Fortuno_Development
	)

	# Install main targets
	install(EXPORT FortunoTargets
			FILE FortunoTargets.cmake
			NAMESPACE Fortuno::
			DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Fortuno
			COMPONENT Fortuno_Development
	)
	export(EXPORT FortunoTargets
			FILE FortunoTargets.cmake
			NAMESPACE Fortuno::
	)
endif ()

# Make project available for FetchContent
if (NOT PROJECT_IS_TOP_LEVEL)
	# Set variables for FetchContent
	# All variables have to be consistent with FortunoConfig.cmake
	set(Fortuno_MPI ${FORTUNO_MPI})
	set(Fortuno_Coarray ${FORTUNO_COARRAY})
	# Propagate variables
	if (CMAKE_VERSION VERSION_LESS 3.25)
		# TODO: Remove when cmake 3.25 is commonly distributed
		set(Fortuno_VERSION ${Fortuno_VERSION} PARENT_SCOPE)
		set(Fortuno_VERSION_MAJOR ${Fortuno_VERSION_MAJOR} PARENT_SCOPE)
		set(Fortuno_VERSION_MINOR ${Fortuno_VERSION_MINOR} PARENT_SCOPE)
		set(Fortuno_VERSION_PATCH ${Fortuno_VERSION_PATCH} PARENT_SCOPE)
		set(Fortuno_VERSION_TWEAK ${Fortuno_VERSION_TWEAK} PARENT_SCOPE)
		set(Fortuno_MPI ${Fortuno_MPI} PARENT_SCOPE)
		set(Fortuno_Coarray ${Fortuno_coarray} PARENT_SCOPE)
	else ()
		return(PROPAGATE
				Fortuno_VERSION
				Fortuno_VERSION_MAJOR
				Fortuno_VERSION_MINOR
				Fortuno_VERSION_PATCH
				Fortuno_VERSION_TWEAK
				Fortuno_MPI
				Fortuno_Coarray
		)
	endif ()
endif ()
