add_library(mylib mylib.f90)

set(
    serial-positive-tests
    fixtured
    parametrized
    parametrized2
    simple
)
set(
    serial-negative-tests
    multiple
    selftest
)
foreach(test IN LISTS serial-positive-tests serial-negative-tests)
    add_executable(test_${test} test_${test}.f90)
    target_link_libraries(test_${test} mylib Fortuno)
    add_test(
        NAME src/test_${test}
        COMMAND test_${test}
    )
endforeach()
foreach(test IN LISTS serial-negative-tests)
    set_tests_properties(src/test_${test} PROPERTIES WILL_FAIL True)
endforeach()


set(mpi-positive-tests)
set(
    mpi-negative-tests
    mpi_simple
)
if(WITH_MPI)
    foreach(test IN LISTS mpi-positive-tests mpi-negative-tests)
        add_executable(test_${test} test_${test}.f90)
        target_link_libraries(test_${test} mylib Fortuno_MPI)
        add_test(
            NAME src/test_${test}
            COMMAND test_${test}
        )
        endforeach()
    foreach(test IN LISTS mpi-negative-tests)
        set_tests_properties(src/test_${test} PROPERTIES WILL_FAIL True)
    endforeach()
endif()


set(coa-positive-tests)
set(
    coa-negative-tests
    coa_simple
)
if(WITH_COARRAY)
    foreach(test IN LISTS coa-positive-tests coa-negative-tests)
        add_executable(test_${test} test_${test}.f90)
        target_link_libraries(test_${test} mylib Fortuno_Coarray)
        add_test(
            NAME src/test_${test}
            COMMAND test_${test}
        )
        endforeach()
    foreach(test IN LISTS coa-negative-tests)
        set_tests_properties(src/test_${test} PROPERTIES WILL_FAIL True)
    endforeach()
endif()
