#
# Global config options
#

# MPI support
option(WITH_MPI "Whether MPI support should be enabled" FALSE)

# Coarray support
option(WITH_COARRAY "Whether coarray support should be enabled" FALSE)

# Type of the built libraries
option(BUILD_SHARED_LIBS "Whether libraries built should be shared" FALSE)

# Building tests
option(WITH_TESTS "Whether tests should be built" TRUE)


#
# Compiler dependent config options
#

if("GNU" STREQUAL "${CMAKE_Fortran_COMPILER_ID}")

    # Specific settings for the GNU compiler

    set(Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2018"
        CACHE STRING "General Fortran compiler flags")

    set(Fortran_FLAGS_RELEASE "-O3 -funroll-all-loops"
        CACHE STRING "Extra Fortran compiler flags for Release build")

    set(Fortran_FLAGS_DEBUG "-g -Wall -pedantic -fbounds-check"
        CACHE STRING "Extra Fortran compiler flags for Debug build")

    set(Fortran_COARRAY_FLAG "-fcoarray=lib" CACHE STRING "Coarray flag of the Fortran compiler")

elseif("Intel" STREQUAL "${CMAKE_Fortran_COMPILER_ID}")

    # Specific settings for the Intel compiler

    set(Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f18 -diag-error-limit 1"
        CACHE STRING "General Fortran compiler flags")

    set(Fortran_FLAGS_RELEASE "-O3 -ip"
        CACHE STRING "Extra Fortran compiler flags for Release build")

    set(Fortran_FLAGS_DEBUG "-g -warn all -check -traceback"
        CACHE STRING "Extra Fortran compiler flags for Debug build")

    set(Fortran_COARRAY_FLAG "-coarray" CACHE STRING "Coarray flag of the Fortran compiler")

elseif("NAG" STREQUAL "${CMAKE_Fortran_COMPILER_ID}")

    # Specific settings for the NAG compiler

    set(Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -f2018"
        CACHE STRING "General Fortran compiler flags")

    set(Fortran_FLAGS_RELEASE "-O3"
        CACHE STRING "Extra Fortran compiler flags for Release build")

    set(Fortran_FLAGS_DEBUG "-g -nan -C=all"
        CACHE STRING "Extra Fortran compiler flags for Debug build")

    set(Fortran_COARRAY_FLAG "-coarray" CACHE STRING "Coarray flag of the Fortran compiler")

else()

    # Generic compiler settings (using CMake's default values)

    set(Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
        CACHE STRING "General Fortran compiler flags")

    set(Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}"
        CACHE STRING "Extra Fortran compiler flags for Release build")

    set(Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}"
        CACHE STRING "Extra compiler flags for Debug build")

    set(Fortran_COARRAY_FLAG "" CACHE STRING "Coarray flag of the Fortran compiler")

endif()
