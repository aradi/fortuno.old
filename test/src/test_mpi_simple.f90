module testmod_mpi_simple
  use mpi_f08, only : MPI_Allreduce, MPI_Bcast, MPI_INTEGER, MPI_SUM
  use fortuno_mpi, only : context => mpi_context, suite => mpi_suite, test => mpi_test,&
      & mpi_test_base
  implicit none


  type, extends(mpi_test_base) :: div_n_failure
    procedure(test_divnfailure), nopass, pointer :: testproc
    integer :: div, rem
  contains
    procedure :: run => div_n_failure_run
  end type

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    testsuite = suite("mpi_simple", [&
        & test("broadcast", test_broadcast),&
        & test("allreduce", test_allreduce)&
        & ])
    call testsuite%add_test(&
        & div_n_failure("divnfailure(3, 0)", test_divnfailure, div=3, rem=0))

  end function test_suite


  subroutine test_broadcast(ctx)
    class(context), intent(inout) :: ctx

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
    class(context),intent(inout) :: ctx

    integer :: send, recv, expected

    send = ctx%mpi%rank + 1
    call MPI_Allreduce(send, recv, 1, MPI_INTEGER, MPI_SUM, ctx%mpi%comm)
    call ctx%check(send == ctx%mpi%rank + 1)
    expected = ctx%mpi%commsize * (ctx%mpi%commsize + 1) / 2
    call ctx%check(recv == expected)

  end subroutine test_allreduce


  subroutine test_divnfailure(ctx, mycase)
    class(context), intent(inout) :: ctx
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
    class(context), intent(inout) :: ctx

    call this%testproc(ctx, this)

  end subroutine div_n_failure_run

end module testmod_mpi_simple


program testapp_mpi_simple
  use fortuno_mpi, only : mpi_app
  use testmod_mpi_simple, only : test_suite
  implicit none

  type(mpi_app), allocatable :: app

  app = mpi_app([test_suite()])
  call app%run()

end program testapp_mpi_simple
