set(test_suites
		simple
)
add_executable(test-app)
target_sources(test-app PRIVATE
		testapp.f90
)
target_link_libraries(test-app PRIVATE mylib Fortuno::Coarray)

foreach(test IN LISTS test_suites)
	target_sources(test-app PRIVATE test_${test}.f90)
	add_test(NAME ${test}
			COMMAND $<TARGET_FILE:test-app> ${test}
	)
endforeach()
