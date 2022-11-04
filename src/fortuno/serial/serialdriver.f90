module fortuno_serial_serialdriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_basetypes, only : suite_base, context_base, test_base
  use fortuno_contextfactory, only : context_factory
  use fortuno_genericdriver, only : generic_driver, test_base_runner
  use fortuno_serial_serialcontext, only : serial_context
  use fortuno_serial_seriallogger, only : serial_logger
  use fortuno_testlogger, only : test_logger
  use fortuno_testerror, only : test_error
  implicit none

  private
  public :: serial_test_base, serial_driver


  type, extends(test_base_runner) :: serial_context_runner
  contains
    procedure :: run_test_base
  end type serial_context_runner


  type, extends(generic_driver) :: serial_driver
  contains
    procedure :: create_test_base_runner
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


  type, extends(test_base), abstract :: serial_test_base
  contains
    procedure(serial_test_base_run_iface), deferred :: run
  end type serial_test_base


  abstract interface
    subroutine serial_test_base_run_iface(this, ctx)
      import :: serial_test_base, serial_context
      class(serial_test_base), intent(inout) :: this
      class(serial_context), intent(inout) :: ctx
    end subroutine serial_test_base_run_iface
  end interface

contains


  function new_serial_driver_single(testsuite) result(this)
    class(suite_base), optional, intent(in) :: testsuite
    type(serial_driver) :: this

    if (present(testsuite)) call this%add_suite_base(testsuite)

  end function new_serial_driver_single


  function new_serial_driver_array(testsuites) result(this)
    class(suite_base), intent(in) :: testsuites(:)
    type(serial_driver) :: this

    call this%add_suite_base(testsuites)

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


  subroutine create_test_base_runner(this, runner)
    class(serial_driver), intent(in) :: this
    class(test_base_runner), allocatable, intent(out) :: runner

    allocate(serial_context_runner :: runner)

  end subroutine create_test_base_runner


  subroutine create_context(this, testsuite, testcase, ctx)
    class(serial_context_factory), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(test_base), pointer, intent(in) :: testcase
    class(context_base), allocatable, intent(out) :: ctx

    allocate(serial_context :: ctx)
    ctx%testsuite => testsuite
    ctx%testcase => testcase

  end subroutine create_context


  subroutine run_test_base(this, testcase, ctx)
    class(serial_context_runner), intent(in) :: this
    class(test_base), pointer, intent(in) :: testcase
    class(context_base), pointer, intent(in) :: ctx

    class(serial_context), pointer :: myctx
    class(serial_test_base), pointer :: mycase

    select type(ctx)
    class is (serial_context)
      myctx => ctx
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    select type(testcase)
    class is (serial_test_base)
      mycase => testcase
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    call mycase%run(myctx)

  end subroutine run_test_base

end module fortuno_serial_serialdriver