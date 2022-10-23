module fortuno_mpi_mpitest
  use fortuno_mpi_mpicontext, only : mpi_context
  use fortuno_mpi_mpidriver, only : mpi_test_case
  implicit none

  private
  public :: mpi_test


  type, extends(mpi_test_case) :: mpi_test
    procedure(test_routine_iface), nopass, pointer :: testroutine
  contains
    procedure :: run
  end type mpi_test


  abstract interface

    subroutine test_routine_iface(ctx)
      import :: mpi_context
      class(mpi_context), intent(inout) :: ctx
    end subroutine test_routine_iface

  end interface

contains


  subroutine run(this, ctx)
    class(mpi_test), intent(inout) :: this
    class(mpi_context), intent(inout) :: ctx

    call this%testroutine(ctx)

  end subroutine run

end module fortuno_mpi_mpitest
