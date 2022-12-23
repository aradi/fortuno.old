module fortuno_serial_serialgctx
  use fortuno_checkresult, only : check_result
  use fortuno_serial_serialcontext, only : serial_context
  use fortuno_suitebase, only : suite_base

  private
  public :: serialgctx
  public :: suite_ptr
  public :: set_global_context, restore_global_context
  public :: check, check_failed, failed, skip


  interface check
    module procedure check_logical, check_detailed
  end interface check

  class(serial_context), pointer, protected :: serialgctx => null()


contains


  subroutine set_global_context(newctx, oldctx)
    class(serial_context), pointer, intent(in) :: newctx
    class(serial_context), pointer, intent(out) :: oldctx

    oldctx => serialgctx
    serialgctx => newctx

  end subroutine set_global_context


  subroutine restore_global_context(oldctx)
    class(serial_context), pointer, intent(in) :: oldctx

    serialgctx => oldctx

  end subroutine restore_global_context


  function suite_ptr() result(suiteptr)
    class(suite_base), pointer :: suiteptr

    suiteptr => serialgctx%suite

  end function suite_ptr


  subroutine check_logical(cond, msg, file, line)
    logical, intent(in) :: cond
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call serialgctx%check_logical(cond, msg, file, line)

  end subroutine check_logical


  subroutine check_detailed(checkresult, msg, file, line)
    type(check_result), intent(in) :: checkresult
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call serialgctx%check_detailed(checkresult, msg, file, line)

  end subroutine check_detailed


  function check_failed() result(checkfailed)
    logical :: checkfailed

    checkfailed = serialgctx%check_failed()

  end function check_failed


  function failed()
    logical :: failed

    failed = serialgctx%failed()

  end function failed


  subroutine skip()

    call serialgctx%skip()

  end subroutine skip

end module fortuno_serial_serialgctx