target_sources(Fortuno_Fortuno PRIVATE
		argumentparser.f90
		checkers.f90
		checkresult.f90
		common.f90
		contextfactory.f90
		failuredetails.f90
		failureinfo.f90
		genericcontext.f90
		genericdriver.f90
		genericsuite.f90
		generictest.f90
		serial.f90
		testerror.f90
		testlogger.f90
		teststatus.f90
		utils.f90
		version.f90
)

add_subdirectory(serial)
if (FORTUNO_MPI)
	add_subdirectory(mpi)
endif ()
if (FORTUNO_COARRAY)
	add_subdirectory(coarray)
endif ()
