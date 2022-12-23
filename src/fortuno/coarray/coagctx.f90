module fortuno_coarray_coagctx
  use fortuno_checkresult, only : check_result
  use fortuno_coarray_coacontext, only : coa_context
  use fortuno_suitebase, only : suite_base
  implicit none

  private
  public :: coagctx
  public :: suite_ptr
  public :: set_global_context, restore_global_context
  public :: check, check_failed, failed, skip


  interface check
    module procedure check_logical, check_detailed
  end interface check

  class(coa_context), pointer, protected :: coagctx => null()


contains


  subroutine set_global_context(newctx, oldctx)
    class(coa_context), pointer, intent(in) :: newctx
    class(coa_context), pointer, intent(out) :: oldctx

    oldctx => coagctx
    coagctx => newctx

  end subroutine set_global_context


  subroutine restore_global_context(oldctx)
    class(coa_context), pointer, intent(in) :: oldctx

    coagctx => oldctx

  end subroutine restore_global_context


  function suite_ptr() result(suiteptr)
    class(suite_base), pointer :: suiteptr

    suiteptr => coagctx%suite

  end function suite_ptr


  subroutine check_logical(cond, msg, file, line)
    logical, intent(in) :: cond
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call coagctx%check_logical(cond, msg, file, line)

  end subroutine check_logical


  subroutine check_detailed(checkresult, msg, file, line)
    type(check_result), intent(in) :: checkresult
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call coagctx%check_detailed(checkresult, msg, file, line)

  end subroutine check_detailed


  function check_failed() result(checkfailed)
    logical :: checkfailed

    checkfailed = coagctx%check_failed()

  end function check_failed


  function failed()
    logical :: failed

    failed = coagctx%failed()

  end function failed


  subroutine skip()

    call coagctx%skip()

  end subroutine skip

end module fortuno_coarray_coagctx