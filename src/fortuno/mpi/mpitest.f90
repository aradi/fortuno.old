module fortuno_mpi_mpitest
  use fortuno_basetypes, only : test_base
  use fortuno_mpi_mpicontext, only : mpi_context
  implicit none

  private
  public :: mpi_fixtured_test, mpi_test, mpi_test_base, mpi_test_base_cls


  type, extends(test_base), abstract :: mpi_test_base
  contains
    procedure(mpi_test_base_run_i), deferred :: run
  end type mpi_test_base


  abstract interface
    subroutine mpi_test_base_run_i(this)
      import :: mpi_test_base
      implicit none
      class(mpi_test_base), intent(inout) :: this
    end subroutine mpi_test_base_run_i
  end interface


  type :: mpi_test_base_cls
    class(mpi_test_base), allocatable :: instance
  end type mpi_test_base_cls


  type, extends(mpi_test_base) :: mpi_test
    procedure(mpi_test_testroutine_i), nopass, pointer :: testroutine
  contains
    procedure :: run => mpi_test_run
  end type mpi_test


  abstract interface
    subroutine mpi_test_testroutine_i()
    end subroutine mpi_test_testroutine_i
  end interface


  type, extends(mpi_test_base) :: mpi_fixtured_test
    procedure(mpi_fixtured_test_testroutine_i), pointer :: testroutine
  contains
    procedure :: run => mpi_fixtured_test_run
    procedure :: set_up => mpi_fixtured_test_set_up
    procedure :: tear_down => mpi_fixtured_test_tear_down
  end type mpi_fixtured_test


  abstract interface
    subroutine mpi_fixtured_test_testroutine_i(this)
      import :: mpi_fixtured_test
      implicit none
      class(mpi_fixtured_test), intent(in) :: this
    end subroutine mpi_fixtured_test_testroutine_i
  end interface

contains


  subroutine mpi_test_run(this)
    class(mpi_test), intent(inout) :: this

    call this%testroutine()

  end subroutine mpi_test_run


  subroutine mpi_fixtured_test_run(this)
    class(mpi_fixtured_test), intent(inout) :: this

    call this%set_up()
    call this%testroutine()
    call this%tear_down()

  end subroutine mpi_fixtured_test_run


  subroutine mpi_fixtured_test_set_up(this)
    class(mpi_fixtured_test), intent(inout) :: this
  end subroutine mpi_fixtured_test_set_up


  subroutine mpi_fixtured_test_tear_down(this)
    class(mpi_fixtured_test), intent(inout) :: this
  end subroutine mpi_fixtured_test_tear_down

end module fortuno_mpi_mpitest
