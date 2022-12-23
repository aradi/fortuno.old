module fortuno_serial_serialdriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_contextbase, only: context_base
  use fortuno_contextfactory, only : context_factory
  use fortuno_genericdriver, only : generic_driver, test_runner
  use fortuno_serial_serialgctx, only : set_global_context, restore_global_context
  use fortuno_serial_serialcontext, only : serial_context
  use fortuno_serial_seriallogger, only : serial_logger
  use fortuno_serial_serialsuite, only : serial_suite_base, serial_suite_base_cls
  use fortuno_serial_serialtest, only : serial_test_base
  use fortuno_suitebase, only : suite_base, suite_base_cls
  use fortuno_testbase, only : test_base
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
    module procedure new_serial_driver_suite, new_serial_driver_suite_cls
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


  function new_serial_driver_suite(testsuites) result(this)
    class(suite_base), optional, intent(in) :: testsuites(:)
    type(serial_driver) :: this

    if (present(testsuites)) call this%add_suite_base(testsuites)

  end function new_serial_driver_suite


  function new_serial_driver_suite_cls(testsuites) result(this)
    type(serial_suite_base_cls), intent(in) :: testsuites(:)
    type(serial_driver) :: this

    type(suite_base_cls), allocatable :: sbc(:)
    integer :: isuite

    allocate(sbc(size(testsuites)))
    do isuite = 1, size(testsuites)
      sbc(isuite)%instance = testsuites(isuite)%instance
    end do
    call this%add_suite_base(sbc)

  end function new_serial_driver_suite_cls


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


  subroutine create_context(this, testsuite, ctx)
    class(serial_context_factory), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(context_base), allocatable, intent(out) :: ctx

    allocate(serial_context :: ctx)
    ctx%suite => testsuite

  end subroutine create_context


  subroutine set_up_suite(this, testsuite, ctx)
    class(serial_runner), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(context_base), pointer, intent(in) :: ctx

    class(serial_context), pointer :: myctx, oldctx
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

    call set_global_context(myctx, oldctx)
    call mysuite%set_up()
    call restore_global_context(oldctx)

  end subroutine set_up_suite


  subroutine tear_down_suite(this, testsuite, ctx)
    class(serial_runner), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(context_base), pointer, intent(in) :: ctx

    class(serial_context), pointer :: myctx, oldctx
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

    call set_global_context(myctx, oldctx)
    call mysuite%tear_down()
    call restore_global_context(oldctx)

  end subroutine tear_down_suite


  subroutine run_test(this, test, ctx)
    class(serial_runner), intent(in) :: this
    class(test_base), pointer, intent(in) :: test
    class(context_base), pointer, intent(in) :: ctx

    class(serial_context), pointer :: myctx, oldctx
    class(serial_test_base), pointer :: mytest

    select type(ctx)
    class is (serial_context)
      myctx => ctx
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    select type(test)
    class is (serial_test_base)
      mytest => test
    class default
      error stop "Internal error, expected serial_test_base, obtained something else"
    end select

    call set_global_context(myctx, oldctx)
    call mytest%run()
    call restore_global_context(oldctx)

  end subroutine run_test

end module fortuno_serial_serialdriver