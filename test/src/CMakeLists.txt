add_library(mylib mylib.f90)

set(
    serial-tests
    fixtured
    multiple
    parametrized
    parametrized2
    selftest
    simple
)

foreach (test IN LISTS serial-tests)
    add_executable(test_${test} test_${test}.f90)
    target_link_libraries(test_${test} mylib Fortuno)
    add_test(
        NAME src/test_${test}
        COMMAND test_${test}
    )
endforeach()


set(
    mpi-tests
    mpi_simple
)

if(WITH_MPI)
    foreach(test IN LISTS mpi-tests)
        add_executable(test_${test} test_${test}.f90)
        target_link_libraries(test_${test} mylib Fortuno_MPI)
    endforeach()
endif()


set(
    coa-tests
    coa_simple
)

if(WITH_COARRAY)
    foreach(test IN LISTS coa-tests)
        add_executable(test_${test} test_${test}.f90)
        target_link_libraries(test_${test} mylib Fortuno_Coarray)
    endforeach()
endif()
