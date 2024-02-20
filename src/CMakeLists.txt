add_subdirectory(fortuno)

target_include_directories(Fortuno_Fortuno PUBLIC
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
)
if(FORTUNO_MPI)
    target_compile_definitions(Fortuno_MPI PRIVATE WITH_MPI)
    target_link_libraries(Fortuno_MPI PRIVATE Fortuno_Fortuno MPI::MPI_Fortran)
endif()
if(FORTUNO_COARRAY)
    target_compile_definitions(Fortuno_Coarray PRIVATE WITH_COARRAY)
    target_compile_options(Fortuno_Coarray PUBLIC
            $<$<Fortran_COMPILER_ID:GNU>:-fcoarray=lib>
            $<$<Fortran_COMPILER_ID:Intel,IntelLLVM,NAG>:-coarray>
    )
    target_link_options(Fortuno_Coarray PUBLIC
            $<$<Fortran_COMPILER_ID:GNU>:-fcoarray=lib>
            $<$<Fortran_COMPILER_ID:Intel,IntelLLVM,NAG>:-coarray>
    )
    target_link_libraries(Fortuno_Coarray PRIVATE Fortuno_Fortuno)
endif()

# Install instructions
if (FORTUNO_INSTALL)
    target_include_directories(Fortuno_Fortuno PUBLIC
            $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>
    )

    # Install the module files
    install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
            DESTINATION ${CMAKE_INSTALL_MODULEDIR}
            COMPONENT Fortuno_Development
    )

    # Install the Targets
    install(TARGETS Fortuno_Fortuno
            EXPORT FortunoTargets
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Fortuno_Runtime NAMELINK_COMPONENT Fortuno_Development
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Fortuno_Development
            PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT Fortuno_Development
    )
    # TODO: These need to be separate components
    if(FORTUNO_MPI)
        install(TARGETS Fortuno_MPI
                EXPORT FortunoTargets
                LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Fortuno_Runtime NAMELINK_COMPONENT Fortuno_Development
                ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Fortuno_Development
                PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT Fortuno_Development
        )
    endif()
    if(FORTUNO_COARRAY)
        install(TARGETS Fortuno_Coarray
                EXPORT fortuno-targets
                LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Fortuno_Runtime NAMELINK_COMPONENT Fortuno_Development
                ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT Fortuno_Development
                PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT Fortuno_Development
        )
    endif()
endif ()
