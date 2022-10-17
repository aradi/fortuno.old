module fortuno_testlogger
  use fortuno_basetypes, only : test_context
  use fortuno_failureinfo, only : failure_info
  implicit none

  private
  public :: test_logger
  public :: test_name_str
  public :: test_status, init_test_status
  public :: driver_result


  type, abstract :: test_logger
  contains
    procedure(begin_short_log_iface), deferred :: begin_short_log
    procedure(short_log_result_iface), deferred :: short_log_result
    procedure(end_short_log_iface), deferred :: end_short_log
    procedure(log_results_iface), deferred :: log_results
  end type test_logger


  type :: test_status
    logical :: success = .false.
    character(:), allocatable :: name
    character(:), allocatable :: repr
    class(failure_info), allocatable :: failureinfo
  end type test_status


  type :: driver_result
    type(test_status), allocatable :: suiteresults(:)
    type(test_status), allocatable :: caseresults(:)
    integer, allocatable :: casetosuite(:)
    logical :: failed = .false.
  end type driver_result


  interface test_status
    module procedure new_test_status
  end interface


  abstract interface

    subroutine begin_short_log_iface(this)
      import :: test_logger
      implicit none
      class(test_logger), intent(inout) :: this
    end subroutine begin_short_log_iface

    subroutine end_short_log_iface(this)
      import :: test_logger
      implicit none
      class(test_logger), intent(inout) :: this
    end subroutine end_short_log_iface

    subroutine short_log_result_iface(this, suitename, casename, success)
      import :: test_logger
      implicit none
      class(test_logger), intent(inout) :: this
      character(*), intent(in) :: suitename, casename
      logical, intent(in) :: success
    end subroutine short_log_result_iface

    subroutine log_results_iface(this, driverresult)
      import :: test_logger, driver_result
      implicit none
      class(test_logger), intent(inout) :: this
      type(driver_result), intent(in) :: driverresult
    end subroutine log_results_iface

  end interface

contains

  function new_test_status(success, name, repr, ctx) result(this)
    logical, intent(in) :: success
    character(*), intent(in) :: name
    character(:), allocatable, intent(in) :: repr
    class(test_context), intent(inout) :: ctx
    type(test_status) :: this

    call init_test_status(this, success, name, repr, ctx)

  end function new_test_status


  subroutine init_test_status(this, success, name, repr, ctx)
    type(test_status), intent(out) :: this
    logical, intent(in) :: success
    character(*), intent(in) :: name
    character(:), allocatable, intent(in) :: repr
    class(test_context), intent(inout) :: ctx

    this%success = success
    this%name = name
    if (allocated(repr)) this%repr = repr
    if (allocated(ctx%failureinfo)) call move_alloc(ctx%failureinfo, this%failureinfo)

  end subroutine init_test_status


  function test_name_str(suitestatus, casestatus) result(testname)
    type(test_status) :: suitestatus, casestatus
    character(:), allocatable :: testname

    testname = suitestatus%name // "/" // casestatus%name
    if (allocated(casestatus%repr)) testname = testname // "{" // casestatus%repr // "}"

  end function test_name_str


end module fortuno_testlogger
