module fortuno_coarray_coadriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_basetypes, only : test_context, test_options, test_suite, test_suite_cls
  use fortuno_contextfactory, only : context_factory
  use fortuno_coarray_coacontext, only : coa_context, coa_context_factory
  use fortuno_coarray_coalogger, only : coa_logger
  use fortuno_serialdriver, only : serial_driver
  use fortuno_testerror, only : test_error
  use fortuno_testlogger, only : test_logger
  use fortuno_utils, only : string
  implicit none

  private
  public :: coa_driver


  type, extends(serial_driver) :: coa_driver
  contains
    procedure :: create_context_factory
    procedure :: create_logger
    procedure :: stop_on_error
  end type


  interface coa_driver
    module procedure new_coa_driver
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


  subroutine stop_on_error(this, error)
    class(coa_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    if (this_image() == 1) write(stderr, "(a, dt)") "Error: ", error
    error stop 1, quiet = .true.

  end subroutine stop_on_error


end module fortuno_coarray_coadriver