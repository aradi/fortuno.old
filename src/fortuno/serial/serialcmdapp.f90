module fortuno_serial_serialcmdapp
  use fortuno_argumentparser, only : argument_parser
  use fortuno_serial_serialdriver, only : serial_driver
  use fortuno_serial_serialsuite, only : serial_suite_base, serial_suite_base_cls
  implicit none

  private
  public :: serial_cmd_app


  type :: serial_cmd_app
  private
    type(serial_driver) :: driver
  contains
    procedure :: run
    procedure :: add_suite_scalar
    procedure :: add_suite_array
    generic :: add_suite => add_suite_scalar, add_suite_array
  end type serial_cmd_app


  interface serial_cmd_app
    module procedure new_serial_cmd_app_suite, new_serial_cmd_app_suite_cls
  end interface serial_cmd_app

contains


  function new_serial_cmd_app_suite(testsuites) result(this)
    class(serial_suite_base), optional, intent(in) :: testsuites(:)
    type(serial_cmd_app) :: this

    this%driver = serial_driver(testsuites)

  end function new_serial_cmd_app_suite


  function new_serial_cmd_app_suite_cls(testsuites) result(this)
    type(serial_suite_base_cls), intent(in) :: testsuites(:)
    type(serial_cmd_app) :: this

    this%driver = serial_driver(testsuites)

  end function new_serial_cmd_app_suite_cls


  subroutine run(this)
    class(serial_cmd_app), intent(inout) :: this

    type(argument_parser), allocatable :: argparser

    argparser = argument_parser()
    call this%driver%run(argparser%get_test_names())

  end subroutine run


  subroutine add_suite_scalar(this, testsuite)
    class(serial_cmd_app), intent(inout) :: this
    class(serial_suite_base), intent(in) :: testsuite

    call this%driver%add_suite_base(testsuite)

  end subroutine add_suite_scalar


  subroutine add_suite_array(this, testsuites)
    class(serial_cmd_app), intent(inout) :: this
    class(serial_suite_base), intent(in) :: testsuites(:)

    call this%driver%add_suite_base(testsuites)

  end subroutine add_suite_array

end module fortuno_serial_serialcmdapp
