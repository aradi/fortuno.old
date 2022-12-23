module fortuno_serial_simplified
  use fortuno_serial_serialgctx, only : check, check_failed, failed, globalctx => serialgctx, skip,&
      & suite_ptr
  use fortuno_serial_serialcmdapp, only : cmd_app => serial_cmd_app
  use fortuno_serial_serialtest, only : fixtured_test => serial_fixtured_test, test => serial_test,&
      & test_base_cls => serial_test_base_cls
  use fortuno_serial_serialdriver, only : test_driver => serial_driver
  use fortuno_serial_serialsuite, only : test_suite => serial_suite,&
      & suite_base_cls => serial_suite_base_cls
  implicit none

end module fortuno_serial_simplified
