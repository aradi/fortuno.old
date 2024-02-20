set_property(DIRECTORY APPEND
		PROPERTY LABELS regression
)

function(Fortuno_Regression_add_test test)
	#[===[.md
	# Fortuno_Regression_add_test

	Internal helper for adding Fortuno regressions tests

	## Synopsis
	```cmake
	Fortuno_Regression_add_test(<name>
			[TEST_NAME <test_name>]
			[TARGET <target>]
			[LABELS <label1> <label2>])
	```

	## Options

	`<name>`
	  Path to the CMake project to be executed relative to `${CMAKE_CURRENT_SOURCE_DIR}`

	`TEST_NAME` [Default: `regression-<name>`]
	  Name for the test to be used as the ctest name

	`LABELS`
	  Additional labels to be added

	]===]

	set(ARGS_Options)
	set(ARGS_OneValue
			TEST_NAME
	)
	set(ARGS_MultiValue
			LABELS
	)
	cmake_parse_arguments(PARSE_ARGV 1 ARGS "${ARGS_Options}" "${ARGS_OneValue}" "${ARGS_MultiValue}")
	# Check required/optional arguments
	if (ARGC LESS 1)
		message(FATAL_ERROR
				"Fortuno: Missing test name in Octopus_add_test call")
	endif ()
	if (NOT DEFINED ARGS_TEST_NAME)
		set(ARGS_TEST_NAME regression-${test})
	endif ()

	add_test(NAME ${ARGS_TEST_NAME}
			COMMAND ${CMAKE_CTEST_COMMAND} --build-and-test ${CMAKE_CURRENT_SOURCE_DIR}/${test}
			${CMAKE_CURRENT_BINARY_DIR}/${test}
			--build-generator "${CMAKE_GENERATOR}"
			--build-options -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
			# Generated Config file point to binary targets until it is installed
			-DFETCHCONTENT_TRY_FIND_PACKAGE_MODE=ALWAYS
			-DFortuno_ROOT=${PROJECT_BINARY_DIR}
			--test-command ${CMAKE_CTEST_COMMAND} --test-dir ${CMAKE_CURRENT_BINARY_DIR}/${test}
	)
	set_tests_properties(${ARGS_TEST_NAME} PROPERTIES
			LABELS "${ARGS_LABELS}"
	)
endfunction()

Fortuno_Regression_add_test(serial)
if (FORTUNO_MPI)
	Fortuno_Regression_add_test(mpi
			LABELS mpi
			TARGET Fortuno_MPI
	)
endif ()
if (FORTUNO_COARRAY)
	Fortuno_Regression_add_test(coarray
			LABELS coarray
			TARGET Fortuno_Coarray
	)
endif ()
