module fortuno_coarray_coadriver
  use iso_fortran_env, only: stderr => error_unit
  use fortuno_genericcontext, only: generic_context
  use fortuno_contextfactory, only: context_factory
  use fortuno_coarray_coacontext, only: coa_context, coa_context_factory
  use fortuno_coarray_coagctx, only: set_global_context, restore_global_context
  use fortuno_coarray_coalogger, only: coa_logger
  use fortuno_coarray_coasuite, only: coa_suite_base, coa_suite_base_cls
  use fortuno_coarray_coatest, only: coa_test_base
  use fortuno_genericdriver, only: generic_driver, test_runner
  use fortuno_genericsuite, only: generic_suite, generic_suite_cls
  use fortuno_generictest, only: generic_test
  use fortuno_testerror, only: test_error
  use fortuno_testlogger, only: test_logger
  implicit none

  private
  public :: coa_driver, coa_test_base

  type, extends(test_runner) :: coa_runner
  contains
    procedure :: set_up_suite
    procedure :: tear_down_suite
    procedure :: run_test
  end type coa_runner

  type, extends(generic_driver) :: coa_driver
  contains
    procedure :: create_context_factory
    procedure :: create_logger
    procedure :: create_test_runner
    procedure :: stop_on_error
  end type

  interface coa_driver
    module procedure new_coa_driver_suite, new_coa_driver_suite_cls
  end interface

contains

  function new_coa_driver_suite(testsuites) result(this)
    class(generic_suite), optional, intent(in) :: testsuites(:)
    type(coa_driver) :: this

    if (present(testsuites)) call this%add_generic_suite(testsuites)

  end function new_coa_driver_suite

  function new_coa_driver_suite_cls(testsuites) result(this)
    type(coa_suite_base_cls), intent(in) :: testsuites(:)
    type(coa_driver) :: this

    type(generic_suite_cls), allocatable :: sbc(:)
    integer :: isuite

    allocate (sbc(size(testsuites)))
    do isuite = 1, size(testsuites)
      sbc(isuite)%instance = testsuites(isuite)%instance
    end do
    call this%add_generic_suite(sbc)

  end function new_coa_driver_suite_cls

  subroutine create_context_factory(this, ctxfact)
    class(coa_driver), intent(in) :: this
    class(context_factory), allocatable, intent(out) :: ctxfact

    ctxfact = coa_context_factory()

  end subroutine create_context_factory

  subroutine create_logger(this, logger)
    class(coa_driver), intent(in) :: this
    class(test_logger), allocatable, intent(out) :: logger

    logger = coa_logger()

  end subroutine create_logger

  subroutine create_test_runner(this, runner)
    class(coa_driver), intent(in) :: this
    class(test_runner), allocatable, intent(out) :: runner

    allocate (coa_runner :: runner)

  end subroutine create_test_runner

  subroutine stop_on_error(this, error)
    class(coa_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    if (allocated(error%message) .and. this_image() == 1) write (stderr, "(a)") error%message
    error stop 1, quiet = .true.

  end subroutine stop_on_error

  subroutine set_up_suite(this, testsuite, ctx)
    class(coa_runner), intent(in) :: this
    class(generic_suite), pointer, intent(in) :: testsuite
    class(generic_context), pointer, intent(in) :: ctx

    class(coa_context), pointer :: myctx, oldctx
    class(coa_suite_base), pointer :: mysuite

    select type (ctx)
    class is (coa_context)
      myctx => ctx
    class default
      error stop "Internal error, expected coa_context, obtained something else"
    end select

    select type (testsuite)
    class is (coa_suite_base)
      mysuite => testsuite
    class default
      error stop "Internal error, expected coa_context, obtained something else"
    end select

    call set_global_context(myctx, oldctx)
    call mysuite%set_up()
    call restore_global_context(oldctx)

  end subroutine set_up_suite

  subroutine tear_down_suite(this, testsuite, ctx)
    class(coa_runner), intent(in) :: this
    class(generic_suite), pointer, intent(in) :: testsuite
    class(generic_context), pointer, intent(in) :: ctx

    class(coa_context), pointer :: myctx, oldctx
    class(coa_suite_base), pointer :: mysuite

    select type (ctx)
    class is (coa_context)
      myctx => ctx
    class default
      error stop "Internal error, expected coa_context, obtained something else"
    end select

    select type (testsuite)
    class is (coa_suite_base)
      mysuite => testsuite
    class default
      error stop "Internal error, expected coa_suite_base, obtained something else"
    end select

    call set_global_context(myctx, oldctx)
    call mysuite%tear_down()
    call restore_global_context(oldctx)

  end subroutine tear_down_suite

  subroutine run_test(this, test, ctx)
    class(coa_runner), intent(in) :: this
    class(generic_test), pointer, intent(in) :: test
    class(generic_context), pointer, intent(in) :: ctx

    class(coa_context), pointer :: myctx, oldctx
    class(coa_test_base), pointer :: mytest

    select type (ctx)
    class is (coa_context)
      myctx => ctx
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    select type (test)
    class is (coa_test_base)
      mytest => test
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    call set_global_context(myctx, oldctx)
    call mytest%run()
    call restore_global_context(oldctx)

  end subroutine run_test

end module fortuno_coarray_coadriver
