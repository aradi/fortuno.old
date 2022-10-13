module fortuno_serialdriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_basetypes, only : test_suite, test_suite_cls, test_context, test_case
  use fortuno_contextfactory, only : context_factory
  use fortuno_genericdriver, only : generic_driver
  use fortuno_seriallogger, only : serial_logger
  use fortuno_testlogger, only : driver_result, test_logger, test_status, init_test_status
  use fortuno_testerror, only : test_error
  use fortuno_utils, only : dyn_char
  implicit none

  private
  public :: serial_driver


  type, extends(generic_driver) :: serial_driver
  contains
    procedure :: create_context_factory
    procedure :: create_logger
    procedure :: stop_on_error
  end type serial_driver


  interface serial_driver
    module procedure new_serial_driver_single, new_serial_driver_array
  end interface


  type, extends(context_factory) :: serial_context_factory
  contains
    procedure :: create_context => create_context
  end type serial_context_factory


  type, extends(test_context) :: serial_context
  end type serial_context


contains


  function new_serial_driver_single(testsuite) result(this)
    class(test_suite), optional, intent(in) :: testsuite
    type(serial_driver) :: this

    if (present(testsuite)) call this%add_test_suite(testsuite)

  end function new_serial_driver_single


  function new_serial_driver_array(testsuites) result(this)
    class(test_suite), intent(in) :: testsuites(:)
    type(serial_driver) :: this

    call this%add_test_suite(testsuites)

  end function new_serial_driver_array


  subroutine stop_on_error(this, error)
    class(serial_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    write(stderr, "(a, dt)") "Error: ", error
    error stop 1, quiet = .true.

  end subroutine stop_on_error


  subroutine create_context_factory(this, ctxfact)
    class(serial_driver), intent(in) :: this
    class(context_factory), allocatable, intent(out) :: ctxfact

    allocate(serial_context_factory :: ctxfact)

  end subroutine create_context_factory


  subroutine create_logger(this, logger)
    class(serial_driver), intent(in) :: this
    class(test_logger), allocatable, intent(out) :: logger

    allocate(serial_logger :: logger)

  end subroutine create_logger


  subroutine create_context(this, testsuite, testcase, ctx)
    class(serial_context_factory), intent(in) :: this
    class(test_suite), pointer, intent(in) :: testsuite
    class(test_case), pointer, intent(in) :: testcase
    class(test_context), allocatable, intent(out) :: ctx

    allocate(serial_context :: ctx)
    ctx%testsuite => testsuite
    ctx%testcase => testcase

  end subroutine create_context


end module fortuno_serialdriver