module fortuno_testlogger
  use fortuno_contextbase, only : context_base
  use fortuno_failureinfo, only : failure_info
  use fortuno_teststatus, only : teststatus
  implicit none

  private
  public :: driver_result
  public :: test_logger
  public :: test_name_str
  public :: test_result, init_test_result
  public :: testtypes


  type, abstract :: test_logger
  contains
    procedure(begin_short_log_i), deferred :: begin_short_log
    procedure(short_log_result_i), deferred :: short_log_result
    procedure(end_short_log_i), deferred :: end_short_log
    procedure(log_results_i), deferred :: log_results
  end type test_logger


  type :: test_result
    integer :: status = teststatus%failed
    character(:), allocatable :: name
    character(:), allocatable :: repr
    class(failure_info), allocatable :: failureinfo
  end type test_result


  type :: driver_result
    type(test_result), allocatable :: suiteresults(:,:)
    type(test_result), allocatable :: testresults(:)
    integer, allocatable :: suiteindex(:)
    logical :: failed = .false.
  end type driver_result


  type :: test_types_enum_
    integer :: suitesetup = 1
    integer :: suiteteardown = 2
    integer :: testrun = 3
  end type test_types_enum_

  type(test_types_enum_), parameter :: testtypes = test_types_enum_()


  abstract interface

    subroutine begin_short_log_i(this)
      import :: test_logger
      implicit none
      class(test_logger), intent(inout) :: this
    end subroutine begin_short_log_i

    subroutine end_short_log_i(this)
      import :: test_logger
      implicit none
      class(test_logger), intent(inout) :: this
    end subroutine end_short_log_i

    subroutine short_log_result_i(this, testtype, suiteresult, testresult)
      import :: test_logger, test_result
      implicit none
      class(test_logger), intent(inout) :: this
      integer, intent(in) :: testtype
      type(test_result), intent(in) :: suiteresult
      type(test_result), optional, intent(in) :: testresult
    end subroutine short_log_result_i

    subroutine log_results_i(this, driverresult)
      import :: test_logger, driver_result
      implicit none
      class(test_logger), intent(inout) :: this
      type(driver_result), intent(in) :: driverresult
    end subroutine log_results_i

  end interface

contains


  subroutine init_test_result(this, name, repr, ctx)
    type(test_result), intent(out) :: this
    character(*), intent(in) :: name
    character(:), allocatable, intent(in) :: repr
    class(context_base), intent(inout) :: ctx

    this%status = ctx%status()
    this%name = name
    if (allocated(repr)) this%repr = repr
    if (allocated(ctx%failureinfo)) call move_alloc(ctx%failureinfo, this%failureinfo)

  end subroutine init_test_result


  function test_name_str(testtype, suiteresult, testresult) result(testnamestr)
    integer, intent(in) :: testtype
    type(test_result), intent(in) :: suiteresult
    type(test_result), optional, intent(in) :: testresult
    character(:), allocatable :: testnamestr

    character(:), allocatable :: prefix, suitename, testname

    select case (testtype)
    case (testtypes%suitesetup)
      prefix = "+"
    case (testtypes%suiteteardown)
      prefix = "-"
    case default
      prefix = ""
    end select

    if (allocated(suiteresult%repr)) then
      suitename = prefix // suiteresult%name // "{" // suiteresult%repr // "}"
    else
      suitename = prefix // suiteresult%name
    end if
    if (.not. present(testresult)) then
      testnamestr = suitename
      return
    end if
    if (allocated(testresult%repr)) then
      testname = testresult%name // "{" // testresult%repr // "}"
    else
      testname = testresult%name
    end if
    testnamestr = suitename // "/" // testname

  end function test_name_str

end module fortuno_testlogger
