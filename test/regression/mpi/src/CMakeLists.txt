target_sources(mylib PRIVATE
		mylib.f90
)
target_link_libraries(mylib PRIVATE MPI::MPI_Fortran)
