module fortuno_coarray
  use fortuno_coarray_coaapp, only : coa_app
  use fortuno_coarray_coacontext, only : coa_context
  use fortuno_coarray_coadriver, only : coa_driver
  use fortuno_coarray_coasuite, only : coa_suite, coa_suite_base
  use fortuno_coarray_coatest, only : coa_test, coa_test_base
  implicit none

  private
  public :: coa_app, coa_context, coa_driver, coa_suite, coa_suite_base, coa_test, coa_test_base

end module fortuno_coarray
