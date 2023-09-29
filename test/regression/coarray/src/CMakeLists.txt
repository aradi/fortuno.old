target_sources(mylib PRIVATE
		mylib.f90
)

target_compile_options(mylib PUBLIC
		$<$<Fortran_COMPILER_ID:GNU>:-fcoarray=lib>
		$<$<Fortran_COMPILER_ID:Intel,IntelLLVM,NAG>:-coarray>
)
target_link_options(mylib PUBLIC
		$<$<Fortran_COMPILER_ID:GNU>:-fcoarray=lib>
		$<$<Fortran_COMPILER_ID:Intel,IntelLLVM,NAG>:-coarray>
)

