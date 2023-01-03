module fortuno_mpi_mpitest
  use fortuno_mpi_mpicontext, only : mpi_context
  use fortuno_testbase, only : test_base
  implicit none

  private
  public :: mpi_test, mpi_test_base, mpi_test_base_cls


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
    procedure(mpi_test_proc_i), nopass, pointer :: proc
  contains
    procedure :: run => mpi_test_run
  end type mpi_test


  abstract interface
    subroutine mpi_test_proc_i()
    end subroutine mpi_test_proc_i
  end interface

contains


  subroutine mpi_test_run(this)
    class(mpi_test), intent(inout) :: this

    call this%proc()

  end subroutine mpi_test_run

end module fortuno_mpi_mpitest
