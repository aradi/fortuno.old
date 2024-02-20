!> Demonstration library offering various objects to test
module mylib
  use mpi_f08, only: MPI_Allreduce, MPI_Bcast, MPI_Comm, MPI_IN_PLACE, MPI_INTEGER, MPI_SUM
  implicit none

  private
  public :: allreduce_sum, broadcast

contains

  !> Broadcasts a scalar integer.
  subroutine broadcast(buffer, source, comm)

    !> Buffer to broadcast
    integer, intent(inout) :: buffer

    !> Source rank
    integer, intent(in) :: source

    !> MPI communicator
    type(MPI_Comm), intent(in) :: comm

    call MPI_Bcast(buffer, 1, MPI_INTEGER, source, comm)

  end subroutine broadcast

  !> Reduces a scalar integer by summation on all ranks.
  subroutine allreduce_sum(val, comm)

    !> Value to reduce by summation
    integer, intent(inout) :: val

    !> MPI communicator
    type(MPI_Comm), intent(in) :: comm

    call MPI_Allreduce(MPI_IN_PLACE, val, 1, MPI_INTEGER, MPI_SUM, comm)

  end subroutine allreduce_sum

end module mylib
