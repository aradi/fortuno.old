#
# Demonstrates and tests how other projects can use Fortuno
#
cmake_minimum_required(VERSION 3.16)

project(TestBuild LANGUAGES Fortran)

find_package(Fortuno REQUIRED)

add_executable(test_build test_build.f90)
target_link_libraries(test_build Fortuno::Fortuno)
