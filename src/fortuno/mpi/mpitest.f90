module fortuno_mpi_mpitest
  use fortuno_basetypes, only : test_base
  use fortuno_mpi_mpicontext, only : mpi_context
  implicit none

  private
  public :: mpi_test, mpi_test_base


  type, extends(test_base), abstract :: mpi_test_base
  contains
    procedure(mpi_test_base_run_i), deferred :: run
  end type mpi_test_base


  abstract interface
    subroutine mpi_test_base_run_i(this, ctx)
      import :: mpi_test_base, mpi_context
      class(mpi_test_base), intent(inout) :: this
      class(mpi_context), intent(inout) :: ctx
    end subroutine mpi_test_base_run_i
  end interface


  type, extends(mpi_test_base) :: mpi_test
    procedure(test_routine_i), nopass, pointer :: testroutine
  contains
    procedure :: run
  end type mpi_test


  abstract interface

    subroutine test_routine_i(ctx)
      import :: mpi_context
      class(mpi_context), intent(inout) :: ctx
    end subroutine test_routine_i

  end interface

contains


  subroutine run(this, ctx)
    class(mpi_test), intent(inout) :: this
    class(mpi_context), intent(inout) :: ctx

    call this%testroutine(ctx)

  end subroutine run

end module fortuno_mpi_mpitest
