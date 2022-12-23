module testmod_simple
  use mylib, only : allreduce_sum, broadcast
  use fortuno_mpi, only : comm_handle_f08, comm_rank, comm_size, check, fixtured_test, skip, test,&
      & test_suite, tbc => test_base_cls
  implicit none


  type, extends(fixtured_test) :: div_n_failure
    integer :: div, rem
  end type

contains


  function simple_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("simple", [&
        & tbc(test("broadcast", test_broadcast)),&
        & tbc(test("allreduce", test_allreduce)),&
        & tbc(test("procs_lt_4", test_procs_lt_4)),&
        & tbc(test("procs_ge_4", test_procs_ge_4)),&
        & tbc(div_n_failure("divnfailure_3_0", test_divnfailure, div=3, rem=0))&
        & ])

  end function simple_suite


  ! Given: source rank contains a different integer value as all other ranks
  ! When: source rank broadcasts its value
  ! Then: all ranks contain source rank's value
  subroutine test_broadcast()

    integer, parameter :: source_rank = 0, source_rank_value = 1, other_rank_value = -1
    integer :: buffer

    ! buffer = (comm_rank() == 0 ? value_rank0 : value_otherranks)
    if (comm_rank() == source_rank) then
      buffer = source_rank_value
    else
      buffer = other_rank_value
    end if

    call broadcast(buffer, source_rank, comm_handle_f08())

    call check(buffer == source_rank_value)

  end subroutine test_broadcast


  ! Given: all ranks contain integer with value (rank + 1)
  ! When: allreduce_sum() is invoked
  ! Then: all ranks contain the sum N * (N + 1) / 2, where N = nr. of ranks
  subroutine test_allreduce()

    integer :: val, expected

    val = comm_rank() + 1

    call allreduce_sum(val, comm_handle_f08())

    expected = comm_size() * (comm_size() + 1) / 2
    call check(val == expected)

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

end module testmod_simple
