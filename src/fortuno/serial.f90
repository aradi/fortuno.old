module fortuno_serial
  use fortuno_common, only : context_base, test_base, suite_base, init_suite_base, teststatus,&
      & is_equal, test_name, test_error, driver_result, get_version
  use fortuno_serial_simplified, only : check, check_failed, cmd_app, failed, globalctx, skip,&
      & suite_base_cls, suite_ptr, test, fixtured_test, test_base_cls, test_suite, test_driver
  implicit none

end module fortuno_serial