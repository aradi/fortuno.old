# Source files of the serial part of the library
set(f90-sources)

# Source files of the MPI-part of the library
set(mpi-f90-sources)

# Source files of the coarray-part of the library
set(coa-f90-sources)

# Collect source files from subdirectories
add_subdirectory(fortuno)

# Build library
add_library(Fortuno ${f90-sources})
set_target_properties(Fortuno PROPERTIES OUTPUT_NAME fortuno)
if(WITH_MPI)
    add_library(Fortuno_MPI ${mpi-f90-sources})
    set_target_properties(Fortuno_MPI PROPERTIES OUTPUT_NAME fortuno-mpi)
    target_compile_definitions(Fortuno_MPI PRIVATE WITH_MPI)
    target_link_libraries(Fortuno_MPI Fortuno MPI::MPI_Fortran)
endif()
if(WITH_COARRAY)
    add_library(Fortuno_Coarray ${coa-f90-sources})
    set_target_properties(Fortuno_Coarray PROPERTIES OUTPUT_NAME fortuno-coarray)
    target_compile_definitions(Fortuno_Coarray PRIVATE WITH_COARRAY)
    target_compile_options(Fortuno_Coarray PUBLIC ${Fortran_COARRAY_FLAG})
    target_link_options(Fortuno_Coarray PUBLIC ${Fortran_COARRAY_FLAG})
    target_link_libraries(Fortuno_Coarray Fortuno)
endif()

# Store generated mod-files in a separate folder
set(moddir "${CMAKE_CURRENT_BINARY_DIR}/modfiles")
set_target_properties(
    Fortuno
    PROPERTIES Fortran_MODULE_DIRECTORY "${moddir}"
)
if(WITH_MPI)
    set_target_properties(
        Fortuno_MPI
        PROPERTIES Fortran_MODULE_DIRECTORY "${moddir}"
    )
endif()
if(WITH_COARRAY)
    set_target_properties(
        Fortuno_Coarray
        PROPERTIES Fortran_MODULE_DIRECTORY "${moddir}"
    )
endif()


# Add mod-file directory to include search path for targets depending on the library
target_include_directories(
    Fortuno PUBLIC
    $<BUILD_INTERFACE:${moddir}>
    $<INSTALL_INTERFACE:${INSTALL_MODULEDIR}>
)
if(WITH_MPI)
    target_include_directories(
        Fortuno_MPI PUBLIC
        $<BUILD_INTERFACE:${moddir}>
        $<INSTALL_INTERFACE:${INSTALL_MODULEDIR}>
    )
endif()
if(WITH_COARRAY)
    target_include_directories(
        Fortuno_Coarray PUBLIC
        $<BUILD_INTERFACE:${moddir}>
        $<INSTALL_INTERFACE:${INSTALL_MODULEDIR}>
    )
endif()


# Install the library
install(
    TARGETS Fortuno
    EXPORT fortuno-targets
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_INCLUDEDIR}"
)
if(WITH_MPI)
    install(
        TARGETS Fortuno_MPI
        EXPORT fortuno-targets
        LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
        PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_INCLUDEDIR}"
    )
endif()
if(WITH_COARRAY)
    install(
        TARGETS Fortuno_Coarray
        EXPORT fortuno-targets
        LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
        PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${INSTALL_INCLUDEDIR}"
    )
endif()

# Install the folder with the modfiles (needed by projects using the library)
install(DIRECTORY "${moddir}/" DESTINATION "${INSTALL_MODULEDIR}")
