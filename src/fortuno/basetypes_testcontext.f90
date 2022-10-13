submodule (fortuno_basetypes) testcontext
  implicit none

contains

  module subroutine test_context_check_logical(this, cond, msg, file, line)
    class(test_context), intent(inout) :: this
    logical, intent(in) :: cond
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    type(failure_info), allocatable :: failureinfo

    this%nchecks = this%nchecks + 1
    if (cond) return
    call this%mark_as_failed()
    allocate(failureinfo)
    failureinfo%checknr = this%nchecks
    if (present(msg)) failureinfo%message = msg
    if (present(file)) failureinfo%file = file
    if (present(line)) failureinfo%line = line
    call move_alloc(failureinfo, this%failureinfo)

  end subroutine test_context_check_logical


  module subroutine test_context_check_detailed(this, checkresult, msg, file, line)
    class(test_context), intent(inout) :: this
    type(check_result), intent(in) :: checkresult
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call this%check_logical(checkresult%success, msg, file, line)
    if (.not. this%failed()) return
    if (allocated(checkresult%failuredet)) this%failureinfo%failuredet = checkresult%failuredet

  end subroutine test_context_check_detailed


  module function test_context_failed(this) result(failed)
    class(test_context), intent(in) :: this
    logical :: failed

    failed = this%failure

  end function test_context_failed


  module subroutine test_context_mark_as_failed(this)
    class(test_context), intent(inout) :: this

    this%failure = .true.

  end subroutine test_context_mark_as_failed


end submodule testcontext