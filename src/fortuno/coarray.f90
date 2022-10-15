module fortuno_coarray
  use fortuno_coarray_coacontext, only : coa_context, coa_context_ptr
  use fortuno_coarray_coadriver, only : coa_driver
  use fortuno_coarray_coatest, only : coa_test
  implicit none

  private
  public :: coa_context, coa_context_ptr, coa_driver, coa_test

end module fortuno_coarray
