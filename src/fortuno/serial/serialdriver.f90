module fortuno_serial_serialdriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_basetypes, only : suite_base, context_base, test_base
  use fortuno_contextfactory, only : context_factory
  use fortuno_genericdriver, only : generic_driver, test_runner
  use fortuno_serial_serialcontext, only : serial_context
  use fortuno_serial_seriallogger, only : serial_logger
  use fortuno_serial_serialsuite, only : serial_suite_base
  use fortuno_serial_serialtest, only : serial_test_base
  use fortuno_testlogger, only : test_logger
  use fortuno_testerror, only : test_error
  implicit none

  private
  public :: serial_driver


  type, extends(generic_driver) :: serial_driver
  contains
    procedure :: create_test_runner
    procedure :: create_context_factory
    procedure :: create_logger
    procedure :: stop_on_error
  end type serial_driver


  interface serial_driver
    module procedure new_serial_driver
  end interface


  type, extends(test_runner) :: serial_runner
  contains
    procedure :: set_up_suite
    procedure :: tear_down_suite
    procedure :: run_test
  end type serial_runner


  type, extends(context_factory) :: serial_context_factory
  contains
    procedure :: create_context
  end type serial_context_factory

contains


  function new_serial_driver(testsuites) result(this)
    class(suite_base), optional, intent(in) :: testsuites(:)
    type(serial_driver) :: this

    if (present(testsuites)) call this%add_suite_base(testsuites)

  end function new_serial_driver


  subroutine stop_on_error(this, error)
    class(serial_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    if (allocated(error%message)) write(stderr, "(a)") error%message
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


  subroutine create_test_runner(this, runner)
    class(serial_driver), intent(in) :: this
    class(test_runner), allocatable, intent(out) :: runner

    allocate(serial_runner :: runner)

  end subroutine create_test_runner


  subroutine create_context(this, testsuite, testcase, ctx)
    class(serial_context_factory), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(test_base), pointer, intent(in) :: testcase
    class(context_base), allocatable, intent(out) :: ctx

    allocate(serial_context :: ctx)
    ctx%testsuite => testsuite
    ctx%testcase => testcase

  end subroutine create_context


  subroutine set_up_suite(this, testsuite, ctx)
    class(serial_runner), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(context_base), pointer, intent(in) :: ctx

    class(serial_context), pointer :: myctx
    class(serial_suite_base), pointer :: mysuite

    select type(ctx)
    class is (serial_context)
      myctx => ctx
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    select type(testsuite)
    class is (serial_suite_base)
      mysuite => testsuite
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    call mysuite%set_up(myctx)

  end subroutine set_up_suite


  subroutine tear_down_suite(this, testsuite, ctx)
    class(serial_runner), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(context_base), pointer, intent(in) :: ctx

    class(serial_context), pointer :: myctx
    class(serial_suite_base), pointer :: mysuite

    select type(ctx)
    class is (serial_context)
      myctx => ctx
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    select type(testsuite)
    class is (serial_suite_base)
      mysuite => testsuite
    class default
      error stop "Internal error, expected serial_suite_base, obtained something else"
    end select

    call mysuite%tear_down(myctx)

  end subroutine tear_down_suite


  subroutine run_test(this, testcase, ctx)
    class(serial_runner), intent(in) :: this
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

  end subroutine run_test

end module fortuno_serial_serialdriver