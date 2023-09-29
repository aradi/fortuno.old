set(test_suites
		selftest
)
set_property(DIRECTORY APPEND
		PROPERTY LABELS unit
)

# Use the same framework as in regression tests
# TODO: The test-app should be common and test-suites should be added as needed
add_executable(test-app)
target_sources(test-app PRIVATE
		testapp.f90
)
target_link_libraries(test-app PRIVATE Fortuno_Fortuno)

# Run test app separately for each test suite
foreach (test IN LISTS test_suites)
	target_sources(test-app PRIVATE test_${test}.f90)
	add_test(NAME ${test}
			COMMAND $<TARGET_FILE:test-app> ${test}
	)
endforeach ()
