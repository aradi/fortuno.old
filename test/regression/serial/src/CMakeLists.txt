set(moddir "${CMAKE_CURRENT_BINARY_DIR}/modfiles")

add_library(${targetprefix}-mylib mylib.f90)

set_target_properties(
    ${targetprefix}-mylib
    PROPERTIES Fortran_MODULE_DIRECTORY "${moddir}"
)
target_include_directories(
    ${targetprefix}-mylib
    PUBLIC $<BUILD_INTERFACE:${moddir}> $<INSTALL_INTERFACE:${INSTALL_MODULEDIR}>
)
