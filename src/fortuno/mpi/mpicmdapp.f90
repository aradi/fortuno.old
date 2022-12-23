module fortuno_mpi_mpicmdapp
  use fortuno_argumentparser, only : argument_parser
  use fortuno_mpi_mpidriver, only : mpi_driver
  use fortuno_mpi_mpisuite, only : mpi_suite_base, mpi_suite_base_cls
  implicit none

  private
  public :: mpi_cmd_app


  type :: mpi_cmd_app
  private
    type(mpi_driver) :: driver
  contains
    procedure :: run
    procedure :: add_suite_scalar
    procedure :: add_suite_array
    generic :: add_suite => add_suite_scalar, add_suite_array
  end type mpi_cmd_app


  interface mpi_cmd_app
    module procedure new_mpi_cmd_app_suite, new_mpi_cmd_app_suite_cls
  end interface mpi_cmd_app

contains


  function new_mpi_cmd_app_suite(testsuites) result(this)
    class(mpi_suite_base), optional, intent(in) :: testsuites(:)
    type(mpi_cmd_app) :: this

    this%driver = mpi_driver(testsuites)

  end function new_mpi_cmd_app_suite


  function new_mpi_cmd_app_suite_cls(testsuites) result(this)
    type(mpi_suite_base_cls), intent(in) :: testsuites(:)
    type(mpi_cmd_app) :: this

    this%driver = mpi_driver(testsuites)

  end function new_mpi_cmd_app_suite_cls


  subroutine run(this)
    class(mpi_cmd_app), intent(inout) :: this

    type(argument_parser), allocatable :: argparser

    argparser = argument_parser()
    call this%driver%run(argparser%get_test_names())

  end subroutine run


  subroutine add_suite_scalar(this, testsuite)
    class(mpi_cmd_app), intent(inout) :: this
    class(mpi_suite_base), intent(in) :: testsuite

    call this%driver%add_suite_base(testsuite)

  end subroutine add_suite_scalar


  subroutine add_suite_array(this, testsuites)
    class(mpi_cmd_app), intent(inout) :: this
    class(mpi_suite_base), intent(in) :: testsuites(:)

    call this%driver%add_suite_base(testsuites)

  end subroutine add_suite_array

end module fortuno_mpi_mpicmdapp
