module fortuno_basetypes
  use fortuno_checkresult, only : check_result
  use fortuno_failureinfo, only : failure_info
  implicit none

  private
  public :: teststatus
  public :: test_base, test_base_cls
  public :: context_base
  public :: init_suite_base, suite_base, suite_base_cls


  type :: test_status_enum_
    integer :: ok = 0
    integer :: failed = 1
    integer :: skipped = 2
  end type test_status_enum_

  type(test_status_enum_), parameter :: teststatus = test_status_enum_()


  type, abstract :: test_base
    character(:), allocatable :: name
  contains
    procedure :: get_char_repr => test_base_get_char_repr
  end type test_base


  type :: test_base_cls
    class(test_base), allocatable :: instance
  end type test_base_cls


  type, abstract :: suite_base
    character(:), allocatable :: name
    type(test_base_cls), allocatable :: tests(:)
  contains
    procedure :: set_name => suite_base_set_name
    procedure, private :: add_test_scalar => suite_base_add_test_scalar
    procedure, private :: add_test_array => suite_base_add_test_array
    generic :: add_test => add_test_scalar, add_test_array
    procedure :: get_char_repr => suite_base_get_char_repr
  end type suite_base


  type :: suite_base_cls
    class(suite_base), allocatable :: instance
  end type suite_base_cls


  type, abstract :: context_base
    class(suite_base), pointer :: suite => null()
    integer, private :: status_ = teststatus%ok
    logical, private :: check_failure = .false.
    integer :: nchecks = 0
    class(failure_info), allocatable :: failureinfo
  contains
    procedure :: check_logical => context_base_check_logical
    procedure :: check_detailed => context_base_check_detailed
    generic :: check => check_logical, check_detailed
    procedure :: register_check => context_base_register_check
    procedure :: failed => context_base_failed
    procedure :: check_failed => context_base_check_failed
    procedure :: skip => context_base_skip
    procedure :: status => context_base_status
  end type context_base

contains

  subroutine test_base_get_char_repr(this, repr)
    class(test_base), intent(in) :: this
    character(:), allocatable, intent(out) :: repr
  end subroutine test_base_get_char_repr


  subroutine context_base_check_logical(this, cond, msg, file, line)
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

  end subroutine context_base_check_logical


  subroutine context_base_check_detailed(this, checkresult, msg, file, line)
    class(context_base), intent(inout) :: this
    type(check_result), intent(in) :: checkresult
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call this%check_logical(checkresult%success, msg, file, line)
    if (.not. (this%failed() .and. this%check_failed())) return
    if (allocated(checkresult%failuredet)) this%failureinfo%failuredet = checkresult%failuredet

  end subroutine context_base_check_detailed


  subroutine context_base_register_check(this, succeeded)
    class(context_base), intent(inout) :: this
    logical, intent(in) :: succeeded

    this%nchecks = this%nchecks + 1
    this%check_failure = .not. succeeded
    if (this%status_ == teststatus%ok .and. .not. succeeded) this%status_ = teststatus%failed

  end subroutine context_base_register_check


  function context_base_failed(this) result(failed)
    class(context_base), intent(in) :: this
    logical :: failed

    failed = this%status_ == teststatus%failed

  end function context_base_failed


  function context_base_check_failed(this) result(checkfailed)
    class(context_base), intent(in) :: this
    logical :: checkfailed

    checkfailed = this%check_failure

  end function context_base_check_failed


  subroutine context_base_skip(this)
    class(context_base), intent(inout) :: this

    if (this%status_ == teststatus%ok) this%status_ = teststatus%skipped

  end subroutine context_base_skip


  function context_base_status(this) result(status)
    class(context_base), intent(in) :: this
    integer :: status

    status = this%status_

  end function context_base_status


  subroutine init_suite_base(this, name, tests)
    class(suite_base), intent(inout) :: this
    character(*), intent(in) :: name
    class(test_base), optional, intent(in) :: tests(:)

    this%name = name
    if (present(tests)) call this%add_test(tests)

  end subroutine init_suite_base


  subroutine suite_base_set_name(this, name)
    class(suite_base), intent(inout) :: this
    character(*), intent(in) :: name

    this%name = name

  end subroutine suite_base_set_name


  subroutine suite_base_add_test_scalar(this, test)
    class(suite_base), intent(inout) :: this
    class(test_base), intent(in) :: test

    call add_slots_(this%tests, 1)
    this%tests(size(this%tests))%instance = test

  end subroutine suite_base_add_test_scalar


  subroutine suite_base_add_test_array(this, tests)
    class(suite_base), intent(inout) :: this
    class(test_base), intent(in) :: tests(:)

    integer :: istart, itest

    call add_slots_(this%tests, size(tests))
    istart = size(this%tests) - size(tests)
    do itest = 1, size(tests)
      this%tests(istart + itest)%instance = tests(itest)
    end do

  end subroutine suite_base_add_test_array


  subroutine suite_base_get_char_repr(this, repr)
    class(suite_base), intent(in) :: this
    character(:), allocatable, intent(out) :: repr
  end subroutine suite_base_get_char_repr


  subroutine add_slots_(tests, newslots)
    type(test_base_cls), allocatable, intent(inout) :: tests(:)
    integer, intent(in) :: newslots

    type(test_base_cls), allocatable :: buffer(:)
    integer :: ii

    if (.not. allocated(tests)) then
      allocate(tests(newslots))
    else
      call move_alloc(tests, buffer)
      allocate(tests(size(buffer) + newslots))
      do ii = 1, size(buffer)
        call move_alloc(buffer(ii)%instance, tests(ii)%instance)
      end do
    end if

  end subroutine add_slots_

end module fortuno_basetypes
