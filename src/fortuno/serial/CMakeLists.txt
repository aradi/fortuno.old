set(curdir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
    APPEND f90-sources
    "${curdir}/serialapp.f90"
    "${curdir}/serialcontext.f90"
    "${curdir}/serialdriver.f90"
    "${curdir}/seriallogger.f90"
    "${curdir}/serialsuite.f90"
    "${curdir}/serialtest.f90"
)

set(f90-sources "${f90-sources}" PARENT_SCOPE)
