module fortuno_coarray
  use fortuno_coarray_coacontext, only : coa_context
  use fortuno_coarray_coadriver, only : coa_driver, coa_test_base
  use fortuno_coarray_coatest, only : coa_test
  implicit none

  private
  public :: coa_context, coa_driver, coa_test, coa_test_base

end module fortuno_coarray
