set(curdir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
    APPEND coa-f90-sources
    "${curdir}/coacmdapp.f90"
    "${curdir}/coacontext.f90"
    "${curdir}/coadriver.f90"
    "${curdir}/coafailureinfo.f90"
    "${curdir}/coagctx.f90"
    "${curdir}/coalogger.f90"
    "${curdir}/coasuite.f90"
    "${curdir}/coatest.f90"
    "${curdir}/simplified.f90"
)

set(coa-f90-sources "${coa-f90-sources}" PARENT_SCOPE)
