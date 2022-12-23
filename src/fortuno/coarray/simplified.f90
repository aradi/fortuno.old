module fortuno_coa_simplified
  use fortuno_coarray_coagctx, only : check, check_failed, failed, globalctx => coagctx, skip,&
      & suite_ptr
  use fortuno_coarray_coacmdapp, only : cmd_app => coa_cmd_app
  use fortuno_coarray_coatest, only : fixtured_test => coa_fixtured_test, test => coa_test,&
      & test_base_cls => coa_test_base_cls
  use fortuno_coarray_coadriver, only : test_driver => coa_driver
  use fortuno_coarray_coasuite, only : test_suite => coa_suite, suite_base_cls => coa_suite_base_cls
  implicit none

end module fortuno_coa_simplified
