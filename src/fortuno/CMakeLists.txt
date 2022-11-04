set(curdir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
    APPEND f90-sources
    "${curdir}/argumentparser.f90"
    "${curdir}/basetypes.f90"
    "${curdir}/checkers.f90"
    "${curdir}/checkresult.f90"
    "${curdir}/contextfactory.f90"
    "${curdir}/failuredetails.f90"
    "${curdir}/failureinfo.f90"
    "${curdir}/genericdriver.f90"
    "${curdir}/testerror.f90"
    "${curdir}/testlogger.f90"
    "${curdir}/utils.f90"
    "${curdir}/version.f90"
)
add_subdirectory(serial)

list(
    APPEND mpi-f90-sources
    "${curdir}/mpi.f90"
)
add_subdirectory(mpi)

list(
    APPEND coa-f90-sources
    "${curdir}/coarray.f90"
)
add_subdirectory(coarray)

set(f90-sources "${f90-sources}" PARENT_SCOPE)
set(mpi-f90-sources "${mpi-f90-sources}" PARENT_SCOPE)
set(coa-f90-sources "${coa-f90-sources}" PARENT_SCOPE)