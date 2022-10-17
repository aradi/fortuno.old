module fortuno_coarray_coadriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_basetypes, only : test_case, test_context, test_suite, test_suite_cls
  use fortuno_contextfactory, only : context_factory
  use fortuno_coarray_coacontext, only : coa_context, coa_context_factory
  use fortuno_coarray_coalogger, only : coa_logger
  use fortuno_genericdriver, only : test_case_runner
  use fortuno_serial_serialdriver, only : serial_driver
  use fortuno_testerror, only : test_error
  use fortuno_testlogger, only : test_logger
  use fortuno_utils, only : string
  implicit none

  private
  public :: coa_driver, coa_test_case


  type, extends(test_case_runner) :: coa_context_runner
  contains
    procedure :: run_test_case
  end type coa_context_runner


  type, extends(serial_driver) :: coa_driver
  contains
    procedure :: create_context_factory
    procedure :: create_logger
    procedure :: create_test_case_runner
    procedure :: stop_on_error
  end type


  interface coa_driver
    module procedure new_coa_driver
  end interface


  type, extends(test_case), abstract :: coa_test_case
  contains
    procedure(coa_test_case_run_iface), deferred :: run
  end type coa_test_case


  abstract interface
    subroutine coa_test_case_run_iface(this, ctx)
      import :: coa_test_case, coa_context
      class(coa_test_case), intent(inout) :: this
      class(coa_context), pointer, intent(in) :: ctx
    end subroutine coa_test_case_run_iface
  end interface


contains


  function new_coa_driver(testsuites) result(this)
    class(test_suite), optional, intent(in) :: testsuites(:)
    type(coa_driver) :: this

    if (present(testsuites)) call this%add_test_suite(testsuites)

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


  subroutine create_test_case_runner(this, runner)
    class(coa_driver), intent(in) :: this
    class(test_case_runner), allocatable, intent(out) :: runner

    allocate(coa_context_runner :: runner)

  end subroutine create_test_case_runner


  subroutine stop_on_error(this, error)
    class(coa_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    if (this_image() == 1) write(stderr, "(a, dt)") "Error: ", error
    error stop 1, quiet = .true.

  end subroutine stop_on_error


  subroutine run_test_case(this, testcase, ctx)
    class(coa_context_runner), intent(in) :: this
    class(test_case), pointer, intent(in) :: testcase
    class(test_context), pointer, intent(in) :: ctx

    class(coa_context), pointer :: myctx
    class(coa_test_case), pointer :: mycase

    select type(ctx)
    class is (coa_context)
      myctx => ctx
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    select type(testcase)
    class is (coa_test_case)
      mycase => testcase
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    call mycase%run(myctx)

  end subroutine run_test_case

end module fortuno_coarray_coadriver