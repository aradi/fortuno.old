#
# Demonstrates and tests how other projects can use Fortuno
#
cmake_minimum_required(VERSION 3.16)

project(TestBuild LANGUAGES Fortran)

find_package(Fortuno REQUIRED)

add_executable(printversion printversion.f90)
target_link_libraries(printversion Fortuno::Fortuno)
