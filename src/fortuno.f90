module fortuno
  use fortuno_argumentparser, only : argument_parser
  use fortuno_basetypes, only : test_context, test_case, test_options, test_suite, testroutine_ifc
  use fortuno_checkers, only : is_equal
  use fortuno_genericdriver, only : test_name
  use fortuno_testerror, only : test_error
  use fortuno_serialdriver, only : serial_driver
  use fortuno_testlogger, only : driver_result
  use fortuno_version, only : get_version
#ifdef WITH_COARRAY
  use fortuno_coarraycontext, only : coa_context, coa_context_ptr
  use fortuno_coarraydriver, only : coa_driver
#endif
  implicit none

  private
  public :: argument_parser
  public :: driver_result
  public :: is_equal
  public :: test_context, test_case, test_options, test_suite, testroutine_ifc
  public :: test_error
  public :: serial_driver
  public :: test_name
  public :: get_version
#ifdef WITH_COARRAY
  public :: coa_context, coa_context_ptr, coa_driver
#endif

end module fortuno
