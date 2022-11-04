module fortuno_coarray_coadriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_basetypes, only : test_base, context_base, suite_base
  use fortuno_contextfactory, only : context_factory
  use fortuno_coarray_coacontext, only : coa_context, coa_context_factory
  use fortuno_coarray_coalogger, only : coa_logger
  use fortuno_genericdriver, only : generic_driver, test_base_runner
  use fortuno_testerror, only : test_error
  use fortuno_testlogger, only : test_logger
  implicit none

  private
  public :: coa_driver, coa_test_base


  type, extends(test_base_runner) :: coa_context_runner
  contains
    procedure :: run_test_base
  end type coa_context_runner


  type, extends(generic_driver) :: coa_driver
  contains
    procedure :: create_context_factory
    procedure :: create_logger
    procedure :: create_test_base_runner
    procedure :: stop_on_error
  end type


  interface coa_driver
    module procedure new_coa_driver
  end interface


  type, extends(test_base), abstract :: coa_test_base
  contains
    procedure(coa_test_base_run_iface), deferred :: run
  end type coa_test_base


  abstract interface
    subroutine coa_test_base_run_iface(this, ctx)
      import :: coa_test_base, coa_context
      class(coa_test_base), intent(inout) :: this
      class(coa_context), intent(inout) :: ctx
    end subroutine coa_test_base_run_iface
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


  subroutine create_test_base_runner(this, runner)
    class(coa_driver), intent(in) :: this
    class(test_base_runner), allocatable, intent(out) :: runner

    allocate(coa_context_runner :: runner)

  end subroutine create_test_base_runner


  subroutine stop_on_error(this, error)
    class(coa_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    if (this_image() == 1) write(stderr, "(a, dt)") "Error: ", error
    error stop 1, quiet = .true.

  end subroutine stop_on_error


  subroutine run_test_base(this, testcase, ctx)
    class(coa_context_runner), intent(in) :: this
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

  end subroutine run_test_base

end module fortuno_coarray_coadriver