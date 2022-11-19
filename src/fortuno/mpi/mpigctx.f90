module fortuno_mpi_mpigctx
  use mpi_f08, only : MPI_Comm
  use fortuno_basetypes, only : suite_base
  use fortuno_checkresult, only : check_result
  use fortuno_mpi_mpicontext, only : mpi_context
  implicit none

  private
  public :: mpigctx
  public :: suite_ptr
  public :: set_global_context, restore_global_context
  public :: check, check_failed, failed, skip
  public :: comm_handle_f, comm_handle_f08, comm_rank, comm_size


  interface check
    module procedure check_logical, check_detailed
  end interface check

  class(mpi_context), pointer, protected :: mpigctx => null()


contains


  subroutine set_global_context(newctx, oldctx)
    class(mpi_context), pointer, intent(in) :: newctx
    class(mpi_context), pointer, intent(out) :: oldctx

    oldctx => mpigctx
    mpigctx => newctx

  end subroutine set_global_context


  subroutine restore_global_context(oldctx)
    class(mpi_context), pointer, intent(in) :: oldctx

    mpigctx => oldctx

  end subroutine restore_global_context


  function suite_ptr() result(suiteptr)
    class(suite_base), pointer :: suiteptr

    suiteptr => mpigctx%suite

  end function suite_ptr


  subroutine check_logical(cond, msg, file, line)
    logical, intent(in) :: cond
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call mpigctx%check_logical(cond, msg, file, line)

  end subroutine check_logical


  subroutine check_detailed(checkresult, msg, file, line)
    type(check_result), intent(in) :: checkresult
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call mpigctx%check_detailed(checkresult, msg, file, line)

  end subroutine check_detailed


  function check_failed() result(checkfailed)
    logical :: checkfailed

    checkfailed = mpigctx%check_failed()

  end function check_failed


  function failed()
    logical :: failed

    failed = mpigctx%failed()

  end function failed


  subroutine skip()

    call mpigctx%skip()

  end subroutine skip


  pure function comm_handle_f08() result(handle)
    type(MPI_Comm) :: handle

    handle = mpigctx%mpi%comm

  end function comm_handle_f08


  pure function comm_handle_f() result(handle)
    integer :: handle

    handle = mpigctx%mpi%comm%MPI_VAL

  end function comm_handle_f


  pure function comm_rank() result(rank)
    integer :: rank

    rank = mpigctx%mpi%rank

  end function comm_rank


  pure function comm_size() result(commsize)
    integer :: commsize

    commsize = mpigctx%mpi%commsize

  end function comm_size

end module fortuno_mpi_mpigctx