module testsuite_mpi_simple
  use mpi_f08, only : MPI_Allreduce, MPI_Bcast, MPI_INTEGER, MPI_SUM
  use fortuno, only : suite_base, test_base, context_base
  use fortuno_mpi, only : mpi_context, mpi_test, mpi_test_base
  implicit none


  type, extends(mpi_test_base) :: div_n_failure
    procedure(test_divnfailure), nopass, pointer :: testproc
    integer :: div, rem
  contains
    procedure :: run => div_n_failure_run
  end type

contains

  function new_suite_base() result(testsuite)
    type(suite_base) :: testsuite

    testsuite = suite_base("mpi_simple", [&
        & mpi_test("broadcast", test_broadcast),&
        & mpi_test("allreduce", test_allreduce)&
        & ])
    call testsuite%add_test(&
        & div_n_failure("divnfailure(3, 0)", test_divnfailure, div=3, rem=0))

  end function new_suite_base


  subroutine test_broadcast(ctx)
    class(mpi_context), intent(inout) :: ctx

    integer :: buffer

    if (ctx%mpi%rank == 0) then
      buffer = 42
    else
      buffer = -1
    end if

    if (ctx%mpi%rank == 0) then
      call ctx%check(buffer == 42)
    else
      call ctx%check(buffer == -1)
    end if
    if (ctx%failed()) return

    call MPI_Bcast(buffer, 1, MPI_INTEGER, 0, ctx%mpi%comm)
    call ctx%check(buffer == 42)

  end subroutine test_broadcast


  subroutine test_allreduce(ctx)
    class(mpi_context),intent(inout) :: ctx

    integer :: send, recv, expected

    send = ctx%mpi%rank + 1
    call MPI_Allreduce(send, recv, 1, MPI_INTEGER, MPI_SUM, ctx%mpi%comm)
    call ctx%check(send == ctx%mpi%rank + 1)
    expected = ctx%mpi%commsize * (ctx%mpi%commsize + 1) / 2
    call ctx%check(recv == expected)

  end subroutine test_allreduce


  subroutine test_divnfailure(ctx, mycase)
    class(mpi_context), intent(inout) :: ctx
    class(div_n_failure), intent(in) :: mycase

    character(100) :: msg

    if (mod(ctx%mpi%rank, mycase%div) == mycase%rem) then
      write(msg, "(a, i0)") "This has failed on purpose on rank ", ctx%mpi%rank
      call ctx%check(.false., msg=trim(msg))
    else
      call ctx%check(.true.)
    end if

    if (mod(ctx%mpi%rank - 1, mycase%div) == mycase%rem) then
      write(msg, "(a, i0)") "This has failed on purpose (2nd time) on rank ", ctx%mpi%rank
      call ctx%check(.false., msg=trim(msg))
    else
      call ctx%check(.true.)
    end if

    if (mod(ctx%mpi%rank - 2, mycase%div) == mycase%rem) then
      write(msg, "(a, i0)") "This has failed on purpose (3rd time) on rank ", ctx%mpi%rank
      call ctx%check(.false., msg=trim(msg))
    else
      call ctx%check(.true.)
    end if

  end subroutine test_divnfailure


  subroutine div_n_failure_run(this, ctx)
    class(div_n_failure), intent(inout) :: this
    class(mpi_context), intent(inout) :: ctx

    call this%testproc(ctx, this)

  end subroutine div_n_failure_run

end module testsuite_mpi_simple


program testdriver_mpi_simple
  use fortuno_mpi, only : mpi_driver
  use testsuite_mpi_simple, only : new_suite_base
  implicit none

  type(mpi_driver), allocatable :: driver

  driver = mpi_driver([new_suite_base()])
  call driver%run()

end program testdriver_mpi_simple
