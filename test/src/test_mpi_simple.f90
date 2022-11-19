module testmod_mpi_simple
  use mpi_f08, only : MPI_Allreduce, MPI_Bcast, MPI_INTEGER, MPI_SUM
  use fortuno_mpi, only : comm_handle_f08, comm_rank, comm_size, check, fixtured_test, skip, test,&
      & test_suite
  implicit none


  type, extends(fixtured_test) :: div_n_failure
    integer :: div, rem
  end type

contains


  function mpi_simple_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("mpi_simple", [&
        & test("broadcast", test_broadcast),&
        & test("allreduce", test_allreduce),&
        & test("procs_lt_4", test_procs_lt_4),&
        & test("procs_ge_4", test_procs_ge_4)&
        & ])
    call suite%add_test(&
        & div_n_failure("divnfailure_3_0", test_divnfailure, div=3, rem=0))

  end function mpi_simple_suite


  ! Given: rank 0 contains a different integer value as all other ranks
  ! When: rank 0 broadcasts its value
  ! Then: all ranks contain rank 0's value
  subroutine test_broadcast()

    integer, parameter :: value_rank0 = 1, value_otherranks = -1
    integer :: buffer

    if (comm_rank() == 0) then
      buffer = value_rank0
    else
      buffer = value_otherranks
    end if

    call MPI_Bcast(buffer, 1, MPI_INTEGER, 0, comm_handle_f08())

    call check(buffer == value_rank0)

  end subroutine test_broadcast


  ! Given: all ranks contain an integer with value (rank + 1)
  ! When: allreduce() is invoked with summation
  ! Then: all ranks contain the sum N * (N + 1) / 2, where N = nr. of processes.
  subroutine test_allreduce()

    integer :: send, recv, expected

    send = comm_rank() + 1

    call MPI_Allreduce(send, recv, 1, MPI_INTEGER, MPI_SUM, comm_handle_f08())

    expected = comm_size() * (comm_size() + 1) / 2
    call check(recv == expected)

  end subroutine test_allreduce


  ! Empty test executed only when nr. of processes < 4.
  subroutine test_procs_lt_4()

    if (comm_size() >= 4) then
      call skip()
      return
    end if
    ! Here you can put tests, which work only up to 3 processes

  end subroutine test_procs_lt_4


  ! Empty test executed only when nr. of processes >= 4.
  subroutine test_procs_ge_4()

    if (comm_size() < 4) then
      call skip()
      return
    end if
    ! Here you can put tests, which work only for 4 processes or more

  end subroutine test_procs_ge_4


  ! When: rank, rank -1 or rank - 2 divided by `div` have a remainder of `rem`
  ! Then: fail with customized error message
  subroutine test_divnfailure(this)
    class(div_n_failure), intent(in) :: this

    character(100) :: msg

    if (mod(comm_rank(), this%div) == this%rem) then
      write(msg, "(a, i0)") "This has failed on purpose on rank ", comm_rank()
      call check(.false., msg=trim(msg))
    else
      call check(.true.)
    end if

    if (mod(comm_rank() - 1, this%div) == this%rem) then
      write(msg, "(a, i0)") "This has failed on purpose (2nd time) on rank ", comm_rank()
      call check(.false., msg=trim(msg))
    else
      call check(.true.)
    end if

    if (mod(comm_rank() - 2, this%div) == this%rem) then
      write(msg, "(a, i0)") "This has failed on purpose (3rd time) on rank ", comm_rank()
      call check(.false., msg=trim(msg))
    else
      call check(.true.)
    end if

  end subroutine test_divnfailure

end module testmod_mpi_simple


program testapp_mpi_simple
  use fortuno_mpi, only : test_app
  use testmod_mpi_simple, only : mpi_simple_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app([mpi_simple_suite()])
  call app%run()

end program testapp_mpi_simple
