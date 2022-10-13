module fortuno_basetypes
  use fortuno_checkresult, only : check_result
  use fortuno_failuredetails, only : failure_details
  use fortuno_failureinfo, only : failure_info
  implicit none

  private
  public :: check_result
  public :: test_case, test_case_cls
  public :: test_context
  public :: test_options
  public :: test_suite, test_suite_cls
  public :: testroutine_ifc


  type :: test_options
    logical :: should_fail = .false.
  end type test_options


  type :: test_case
    character(:), allocatable :: name
    procedure(testroutine_ifc), nopass, pointer :: testroutine
    class(test_options), allocatable :: options
  contains
    procedure, nopass :: set_up => test_case_set_up
    procedure, nopass :: run => test_case_run
    procedure, nopass :: tear_down => test_case_tear_down
    procedure :: get_status_str => test_case_get_status_str
  end type test_case


  interface test_case
    module procedure new_test_case
  end interface test_case


  type :: test_case_cls
    class(test_case), allocatable :: instance
  end type test_case_cls


  type :: test_suite
    character(:), allocatable :: name
    type(test_case_cls), allocatable :: testcases(:)
  contains
    procedure, private :: add_test_case_single => test_suite_add_test_case_single
    procedure, private :: add_test_case_array => test_suite_add_test_case_array
    generic :: add_test_case => add_test_case_single, add_test_case_array
    procedure :: set_up => test_suite_set_up
    procedure :: tear_down => test_suite_tear_down
  end type test_suite


  interface test_suite
    module procedure new_test_suite
  end interface


  type :: test_suite_cls
    class(test_suite), allocatable :: instance
  end type test_suite_cls


  type :: test_context
    class(test_case), pointer :: testcase => null()
    class(test_suite), pointer :: testsuite => null()
    logical, private :: failure = .false.
    integer :: nchecks = 0
    class(failure_info), allocatable :: failureinfo
  contains
    procedure :: check_logical => test_context_check_logical
    procedure :: check_detailed => test_context_check_detailed
    generic :: check => check_logical, check_detailed
    procedure :: failed => test_context_failed
    procedure :: mark_as_failed => test_context_mark_as_failed
  end type test_context


interface

    module function new_test_case(name, testroutine, options) result(this)
      implicit none
      character(*), intent(in) :: name
      procedure(testroutine_ifc), pointer, intent(in) :: testroutine
      type(test_options), optional, intent(in) :: options
      type(test_case) :: this
    end function new_test_case

    module subroutine test_case_set_up(ctx)
      implicit none
      class(test_context), pointer, intent(in) :: ctx
    end subroutine test_case_set_up

    module subroutine test_case_run(ctx)
      implicit none
      class(test_context), pointer, intent(in) :: ctx
    end subroutine test_case_run

    module subroutine test_case_tear_down(ctx)
      implicit none
      class(test_context), pointer, intent(in) :: ctx
    end subroutine test_case_tear_down

    module subroutine test_case_get_status_str(this, state)
      implicit none
      class(test_case), intent(in) :: this
      character(:), allocatable, intent(out) :: state
    end subroutine test_case_get_status_str

    module function new_test_suite(name, testcases) result(this)
      implicit none
      character(*), intent(in) :: name
      class(test_case), optional, intent(in) :: testcases(:)
      type(test_suite) :: this
    end function new_test_suite

    module subroutine test_suite_add_test_case_single(this, testcase)
      implicit none
      class(test_suite), intent(inout) :: this
      class(test_case), intent(in) :: testcase
    end subroutine test_suite_add_test_case_single

    module subroutine test_suite_add_test_case_array(this, testcases)
      implicit none
      class(test_suite), intent(inout) :: this
      class(test_case), intent(in) :: testcases(:)
    end subroutine test_suite_add_test_case_array

    module subroutine test_suite_set_up(this, ctx)
      implicit none
      class(test_suite), intent(inout) :: this
      class(test_context), pointer, intent(in) :: ctx
    end subroutine test_suite_set_up

    module subroutine test_suite_tear_down(this, ctx)
      implicit none
      class(test_suite), intent(inout) :: this
      class(test_context), pointer, intent(in) :: ctx
    end subroutine test_suite_tear_down

    module subroutine test_context_check_logical(this, cond, msg, file, line)
      implicit none
      class(test_context), intent(inout) :: this
      logical, intent(in) :: cond
      character(*), optional, intent(in) :: msg
      character(*), optional, intent(in) :: file
      integer, optional, intent(in) :: line
    end subroutine test_context_check_logical

    module subroutine test_context_check_detailed(this, checkresult, msg, file, line)
      implicit none
      class(test_context), intent(inout) :: this
      type(check_result), intent(in) :: checkresult
      character(*), optional, intent(in) :: msg
      character(*), optional, intent(in) :: file
      integer, optional, intent(in) :: line
    end subroutine test_context_check_detailed

    module function test_context_failed(this) result(failed)
      implicit none
      class(test_context), intent(in) :: this
      logical :: failed
    end function test_context_failed

    module subroutine test_context_mark_as_failed(this)
      implicit none
      class(test_context), intent(inout) :: this
    end subroutine test_context_mark_as_failed

  end interface


  abstract interface

    subroutine failure_details_write_formatted(this, unit)
      import :: failure_details
      implicit none
      class(failure_details), intent(in) :: this
      integer, intent(in) :: unit
    end subroutine failure_details_write_formatted

    subroutine testroutine_ifc(ctx)
      import :: test_context
      implicit none
      class(test_context), pointer, intent(in) :: ctx
    end subroutine testroutine_ifc

  end interface


end module fortuno_basetypes
