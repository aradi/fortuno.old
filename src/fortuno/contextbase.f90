module fortuno_contextbase
  use fortuno_checkresult, only : check_result
  use fortuno_failureinfo, only : failure_info
  use fortuno_suitebase, only : suite_base
  use fortuno_teststatus, only : teststatus
  implicit none

  private
  public :: context_base


  type, abstract :: context_base
    class(suite_base), pointer :: suite => null()
    integer, private :: status_ = teststatus%ok
    logical, private :: check_failure = .false.
    integer :: nchecks = 0
    class(failure_info), allocatable :: failureinfo
  contains
    procedure :: check_logical
    procedure :: check_detailed
    generic :: check => check_logical, check_detailed
    procedure :: register_check
    procedure :: failed
    procedure :: check_failed
    procedure :: skip
    procedure :: status
  end type context_base

contains


  subroutine check_logical(this, cond, msg, file, line)
    class(context_base), intent(inout) :: this
    logical, intent(in) :: cond
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    type(failure_info), allocatable :: failureinfo

    call this%register_check(cond)
    if (.not. (this%failed() .and. this%check_failed())) return
    allocate(failureinfo)
    failureinfo%checknr = this%nchecks
    if (present(msg)) failureinfo%message = msg
    if (present(file)) failureinfo%file = file
    if (present(line)) failureinfo%line = line
    if (allocated(this%failureinfo)) call move_alloc(this%failureinfo, failureinfo%previous)
    call move_alloc(failureinfo, this%failureinfo)

  end subroutine check_logical


  subroutine check_detailed(this, checkresult, msg, file, line)
    class(context_base), intent(inout) :: this
    type(check_result), intent(in) :: checkresult
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call this%check_logical(checkresult%success, msg, file, line)
    if (.not. (this%failed() .and. this%check_failed())) return
    if (allocated(checkresult%failuredet)) this%failureinfo%failuredet = checkresult%failuredet

  end subroutine check_detailed


  subroutine register_check(this, succeeded)
    class(context_base), intent(inout) :: this
    logical, intent(in) :: succeeded

    this%nchecks = this%nchecks + 1
    this%check_failure = .not. succeeded
    if (this%status_ == teststatus%ok .and. .not. succeeded) this%status_ = teststatus%failed

  end subroutine register_check


  function failed(this)
    class(context_base), intent(in) :: this
    logical :: failed

    failed = this%status_ == teststatus%failed

  end function failed


  function check_failed(this) result(checkfailed)
    class(context_base), intent(in) :: this
    logical :: checkfailed

    checkfailed = this%check_failure

  end function check_failed


  subroutine skip(this)
    class(context_base), intent(inout) :: this

    if (this%status_ == teststatus%ok) this%status_ = teststatus%skipped

  end subroutine skip


  function status(this)
    class(context_base), intent(in) :: this
    integer :: status

    status = this%status_

  end function status

end module fortuno_contextbase