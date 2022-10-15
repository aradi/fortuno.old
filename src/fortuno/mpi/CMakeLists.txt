set(curdir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
    APPEND mpi-f90-sources
    "${curdir}/mpicontext.f90"
    "${curdir}/mpidriver.f90"
    "${curdir}/mpifailureinfo.f90"
    "${curdir}/mpilogger.f90"
    "${curdir}/mpitest.f90"
)

set(mpi-f90-sources "${mpi-f90-sources}" PARENT_SCOPE)
