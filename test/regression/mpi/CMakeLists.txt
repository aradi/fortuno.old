# Prefix to use for the test names
set(testprefix "reg/mpi")

# Prefix to use for target names
set(targetprefix "test-reg-mpi")

# create demo library test-mylib
add_subdirectory(src)

# Set up all test suites to consider (contained in files named test_${suite})
set(
    suites
    simple
)
set(
    failing-suites
    simple
)

# Create a library out of all test suites
add_library(${targetprefix}-suites)
foreach(suite IN LISTS suites)
    target_sources(${targetprefix}-suites PRIVATE test_${suite}.f90)
endforeach()
target_link_libraries(${targetprefix}-suites Fortuno_MPI)
target_link_libraries(${targetprefix}-suites ${targetprefix}-mylib)

# Create test app
add_executable(${targetprefix}-app testapp.f90)
set_target_properties(${targetprefix}-app PROPERTIES OUTPUT_NAME "testapp")
target_link_libraries(${targetprefix}-app ${targetprefix}-suites)

# Run test app separately for each test suite
foreach(test IN LISTS suites)
    add_test(
        NAME ${testprefix}/${test}
        COMMAND ${CMAKE_CURRENT_BINARY_DIR}/testapp ${test}
    )

    # Set WILL_FAIL flag if suite is expected to fail
    set(will-fail False)
    foreach(failing-suite IN LISTS failing-suites)
        if(failing-suite STREQUAL test)
            set(will-fail True)
            break()
        endif()
    endforeach()
    set_tests_properties(${testprefix}/${test} PROPERTIES WILL_FAIL ${will-fail})
endforeach()
