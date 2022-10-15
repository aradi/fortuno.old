module fortuno_mpi_mpitest
  use fortuno_basetypes, only : test_case, test_context
  use fortuno_mpi_mpicontext, only : mpi_context, mpi_context_ptr
  implicit none

  private
  public :: mpi_test


  type, extends(test_case) :: mpi_test
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
    class(test_context), pointer, intent(in) :: ctx

    class(mpi_context), pointer :: mpictx

    mpictx => mpi_context_ptr(ctx)
    call this%testroutine(mpictx)

  end subroutine run

end module fortuno_mpi_mpitest
