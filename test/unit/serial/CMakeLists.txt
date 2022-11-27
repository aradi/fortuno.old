# Prefix to use for the test names
set(testprefix "unit/serial")

# Prefix to use for target names
set(targetprefix "test-unit-serial")

# Set up all test suites to consider (contained in files named test_${suite})
set(
    suites
    selftest
)

# Create library from all test suites
add_library(${targetprefix}-suites)
foreach(suite IN LISTS suites)
    target_sources(${targetprefix}-suites PRIVATE test_${suite}.f90)
endforeach()
target_link_libraries(${targetprefix}-suites Fortuno)

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
endforeach()
