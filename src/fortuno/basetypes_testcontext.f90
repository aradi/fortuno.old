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

    call this%register_check(cond)
    if (cond) return
    allocate(failureinfo)
    failureinfo%checknr = this%nchecks
    if (present(msg)) failureinfo%message = msg
    if (present(file)) failureinfo%file = file
    if (present(line)) failureinfo%line = line
    if (allocated(this%failureinfo)) call move_alloc(this%failureinfo, failureinfo%previous)
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


  module subroutine test_context_register_check(this, succeeded)
    class(test_context), intent(inout) :: this
    logical, intent(in) :: succeeded

    this%nchecks = this%nchecks + 1
    this%check_failure = .not. succeeded
    this%failure = this%failure .or. this%check_failure

  end subroutine test_context_register_check


  module function test_context_failed(this) result(failed)
    class(test_context), intent(in) :: this
    logical :: failed

    failed = this%failure

  end function test_context_failed


  module function test_context_check_failed(this) result(failed)
    class(test_context), intent(in) :: this
    logical :: failed

    failed = this%check_failure

  end function test_context_check_failed


end submodule testcontext