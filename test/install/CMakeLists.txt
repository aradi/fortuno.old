cmake_minimum_required(VERSION 3.25)

project(find-project LANGUAGES Fortran)

include(FetchContent)
FetchContent_Declare(Fortuno
		GIT_REPOSITORY https://github.com/aradi/fortuno
		GIT_TAG main
		FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(Fortuno)

add_executable(printversion printversion.f90)
target_link_libraries(printversion Fortuno::Fortuno)
