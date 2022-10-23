module fortuno
  use fortuno_argumentparser, only : argument_parser
  use fortuno_basetypes, only : test_context, test_case, test_suite
  use fortuno_checkers, only : is_equal
  use fortuno_genericdriver, only : test_name
  use fortuno_testerror, only : test_error
  use fortuno_serial_serialcontext, only : serial_context
  use fortuno_serial_serialtest, only : serial_test
  use fortuno_serial_serialdriver, only : serial_driver, serial_test_case
  use fortuno_testlogger, only : driver_result
  use fortuno_version, only : get_version
  implicit none

  private
  public :: argument_parser
  public :: driver_result
  public :: get_version
  public :: is_equal
  public :: serial_context, serial_driver, serial_test_case
  public :: serial_test
  public :: test_context, test_case, test_suite
  public :: test_error
  public :: test_name

end module fortuno
