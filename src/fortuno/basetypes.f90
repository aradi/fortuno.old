module fortuno_basetypes
  use fortuno_checkresult, only : check_result
  use fortuno_failureinfo, only : failure_info
  implicit none

  private
  public :: test_base, test_base_cls
  public :: context_base
  public :: init_suite_base, suite_base, suite_base_cls


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
    type(test_base_cls), allocatable :: testcases(:)
  contains
    procedure :: set_name => suite_base_set_name
    procedure :: add_test_single => suite_base_add_test_single
    procedure :: add_test_array => suite_base_add_test_array
    generic :: add_test => add_test_single, add_test_array
  end type suite_base


  type :: suite_base_cls
    class(suite_base), allocatable :: instance
  end type suite_base_cls


  type, abstract :: context_base
    class(test_base), pointer :: testcase => null()
    class(suite_base), pointer :: testsuite => null()
    logical, private :: failure = .false.
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
  end type context_base

contains

  subroutine test_base_get_char_repr(this, state)
    class(test_base), intent(in) :: this
    character(:), allocatable, intent(out) :: state
  end subroutine test_base_get_char_repr


  subroutine context_base_check_logical(this, cond, msg, file, line)
    class(context_base), intent(inout) :: this
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

  end subroutine context_base_check_logical


  subroutine context_base_check_detailed(this, checkresult, msg, file, line)
    class(context_base), intent(inout) :: this
    type(check_result), intent(in) :: checkresult
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    call this%check_logical(checkresult%success, msg, file, line)
    if (.not. this%failed()) return
    if (allocated(checkresult%failuredet)) this%failureinfo%failuredet = checkresult%failuredet

  end subroutine context_base_check_detailed


  subroutine context_base_register_check(this, succeeded)
    class(context_base), intent(inout) :: this
    logical, intent(in) :: succeeded

    this%nchecks = this%nchecks + 1
    this%check_failure = .not. succeeded
    this%failure = this%failure .or. this%check_failure

  end subroutine context_base_register_check


  function context_base_failed(this) result(failed)
    class(context_base), intent(in) :: this
    logical :: failed

    failed = this%failure

  end function context_base_failed


  function context_base_check_failed(this) result(failed)
    class(context_base), intent(in) :: this
    logical :: failed

    failed = this%check_failure

  end function context_base_check_failed


  subroutine init_suite_base(this, name, testcases)
    class(suite_base), intent(inout) :: this
    character(*), intent(in) :: name
    class(test_base), optional, intent(in) :: testcases(:)

    this%name = name
    if (present(testcases)) call this%add_test(testcases)

  end subroutine init_suite_base


  subroutine suite_base_set_name(this, name)
    class(suite_base), intent(inout) :: this
    character(*), intent(in) :: name

    this%name = name

  end subroutine suite_base_set_name

  subroutine suite_base_add_test_single(this, testcase)
    class(suite_base), intent(inout) :: this
    class(test_base), intent(in) :: testcase

    call add_slots_(this%testcases, 1)
    this%testcases(size(this%testcases))%instance = testcase

  end subroutine suite_base_add_test_single


  subroutine suite_base_add_test_array(this, testcases)
    class(suite_base), intent(inout) :: this
    class(test_base), intent(in) :: testcases(:)

    integer :: istart, ii

    call add_slots_(this%testcases, size(testcases))
    istart = size(this%testcases) - size(testcases)
    do ii = 1, size(testcases)
      this%testcases(istart + ii)%instance = testcases(ii)
    end do

  end subroutine suite_base_add_test_array


  subroutine add_slots_(testcases, newslots)
    type(test_base_cls), allocatable, intent(inout) :: testcases(:)
    integer, intent(in) :: newslots

    type(test_base_cls), allocatable :: buffer(:)
    integer :: ii

    if (.not. allocated(testcases)) then
      allocate(testcases(newslots))
    else
      call move_alloc(testcases, buffer)
      allocate(testcases(size(buffer) + newslots))
      do ii = 1, size(buffer)
        call move_alloc(buffer(ii)%instance, testcases(ii)%instance)
      end do
    end if

  end subroutine add_slots_

end module fortuno_basetypes
