module fortuno_coarray_coadriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_basetypes, only : test_base, context_base, suite_base
  use fortuno_contextfactory, only : context_factory
  use fortuno_coarray_coacontext, only : coa_context, coa_context_factory
  use fortuno_coarray_coalogger, only : coa_logger
  use fortuno_coarray_coasuite, only : coa_suite_base
  use fortuno_coarray_coatest, only : coa_test_base
  use fortuno_genericdriver, only : generic_driver, test_runner
  use fortuno_testerror, only : test_error
  use fortuno_testlogger, only : test_logger
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
    module procedure new_coa_driver
  end interface

contains


  function new_coa_driver(testsuites) result(this)
    class(suite_base), optional, intent(in) :: testsuites(:)
    type(coa_driver) :: this

    if (present(testsuites)) call this%add_suite_base(testsuites)

  end function new_coa_driver


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

    allocate(coa_runner :: runner)

  end subroutine create_test_runner


  subroutine stop_on_error(this, error)
    class(coa_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    if (this_image() == 1) write(stderr, "(a, dt)") "Error: ", error
    error stop 1, quiet = .true.

  end subroutine stop_on_error


  subroutine set_up_suite(this, testsuite, ctx)
    class(coa_runner), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(context_base), pointer, intent(in) :: ctx

    class(coa_context), pointer :: myctx
    class(coa_suite_base), pointer :: mysuite

    select type(ctx)
    class is (coa_context)
      myctx => ctx
    class default
      error stop "Internal error, expected coa_context, obtained something else"
    end select

    select type(testsuite)
    class is (coa_suite_base)
      mysuite => testsuite
    class default
      error stop "Internal error, expected coa_context, obtained something else"
    end select

    call mysuite%set_up(myctx)

  end subroutine set_up_suite


  subroutine tear_down_suite(this, testsuite, ctx)
    class(coa_runner), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(context_base), pointer, intent(in) :: ctx

    class(coa_context), pointer :: myctx
    class(coa_suite_base), pointer :: mysuite

    select type(ctx)
    class is (coa_context)
      myctx => ctx
    class default
      error stop "Internal error, expected coa_context, obtained something else"
    end select

    select type(testsuite)
    class is (coa_suite_base)
      mysuite => testsuite
    class default
      error stop "Internal error, expected coa_suite_base, obtained something else"
    end select

    call mysuite%tear_down(myctx)

  end subroutine tear_down_suite


  subroutine run_test(this, testcase, ctx)
    class(coa_runner), intent(in) :: this
    class(test_base), pointer, intent(in) :: testcase
    class(context_base), pointer, intent(in) :: ctx

    class(coa_context), pointer :: myctx
    class(coa_test_base), pointer :: mycase

    select type(ctx)
    class is (coa_context)
      myctx => ctx
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    select type(testcase)
    class is (coa_test_base)
      mycase => testcase
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    call mycase%run(myctx)

  end subroutine run_test

end module fortuno_coarray_coadriver