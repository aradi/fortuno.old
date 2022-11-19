module fortuno_coa_simplified
  use fortuno_coarray_coagctx, only : check, check_failed, failed, globalctx => coagctx, skip,&
      & suite_ptr
  use fortuno_coarray_coaapp, only : test_app => coa_app
  use fortuno_coarray_coatest, only : fixtured_test => coa_fixtured_test, test => coa_test
  use fortuno_coarray_coadriver, only : test_driver => coa_driver
  use fortuno_coarray_coasuite, only : test_suite => coa_suite
  implicit none

end module fortuno_coa_simplified
