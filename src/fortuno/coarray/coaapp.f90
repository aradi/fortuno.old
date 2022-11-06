module fortuno_coarray_coaapp
  use fortuno_argumentparser, only : argument_parser
  use fortuno_coarray_coadriver, only : coa_driver
  use fortuno_coarray_coasuite, only : coa_suite_base
  implicit none

  private
  public :: coa_app


  type :: coa_app
  private
    type(coa_driver) :: driver
  contains
    procedure :: run
    procedure :: add_suite_scalar
    procedure :: add_suite_array
    generic :: add_suite => add_suite_scalar, add_suite_array
  end type coa_app


  interface coa_app
    module procedure new_coa_app
  end interface coa_app

contains


  function new_coa_app(testsuites) result(this)
    class(coa_suite_base), optional, intent(in) :: testsuites(:)
    type(coa_app) :: this

    this%driver = coa_driver(testsuites)

  end function new_coa_app


  subroutine run(this)
    class(coa_app), intent(inout) :: this

    type(argument_parser), allocatable :: argparser

    argparser = argument_parser()
    call this%driver%run(argparser%get_test_names())

  end subroutine run


  subroutine add_suite_scalar(this, testsuite)
    class(coa_app), intent(inout) :: this
    class(coa_suite_base), intent(in) :: testsuite

    call this%driver%add_suite_base(testsuite)

  end subroutine add_suite_scalar


  subroutine add_suite_array(this, testsuites)
    class(coa_app), intent(inout) :: this
    class(coa_suite_base), intent(in) :: testsuites(:)

    call this%driver%add_suite_base(testsuites)

  end subroutine add_suite_array

end module fortuno_coarray_coaapp
