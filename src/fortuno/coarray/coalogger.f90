module fortuno_coarray_coalogger
  use iso_fortran_env, only : stdout => output_unit
  use fortuno_coarray_coafailureinfo, only : coa_failure_info
  use fortuno_failureinfo, only : failure_info
  use fortuno_serial_seriallogger, only : serial_logger
  use fortuno_testlogger, only : test_result
  implicit none

  private
  public :: coa_logger


  type, extends(serial_logger) :: coa_logger
  contains
    procedure :: begin_short_log
    procedure :: short_log_result
    procedure :: end_short_log
    procedure :: begin_test_base_failure_log
    procedure :: log_test_base_failure
    procedure :: end_test_base_failure_log
    procedure :: log_summary
  end type coa_logger

contains


  subroutine begin_short_log(this)
    class(coa_logger), intent(inout) :: this

    if (this_image() /= 1) return
    call this%serial_logger%begin_short_log()

  end subroutine begin_short_log


  subroutine end_short_log(this)
    class(coa_logger), intent(inout) :: this

    if (this_image() /= 1) return
    call this%serial_logger%end_short_log()

  end subroutine end_short_log


  subroutine short_log_result(this, testtype, suiteresult, testresult)
    class(coa_logger), intent(inout) :: this
    integer, intent(in) :: testtype
    type(test_result), intent(in) :: suiteresult
    type(test_result), optional, intent(in) :: testresult

    if (this_image() /= 1) return
    call this%serial_logger%short_log_result(testtype, suiteresult, testresult)

  end subroutine short_log_result


  subroutine begin_test_base_failure_log(this, testtype, suiteresult, testresult)
    class(coa_logger), intent(inout) :: this
    integer, intent(in) :: testtype
    type(test_result), intent(in) :: suiteresult
    type(test_result), optional, intent(in) :: testresult

    if (this_image() /= 1) return
    call this%serial_logger%begin_test_base_failure_log(testtype, suiteresult, testresult)

  end subroutine begin_test_base_failure_log


  subroutine end_test_base_failure_log(this)
    class(coa_logger), intent(inout) :: this

    sync all
    if (this_image() /= 1) return
    call this%serial_logger%end_test_base_failure_log()

  end subroutine end_test_base_failure_log


  recursive subroutine log_test_base_failure(this, failureinfo)
    class(coa_logger), intent(inout) :: this
    class(failure_info), intent(in) :: failureinfo

    integer :: firstfailing
    logical :: writesepline

    writesepline = .false.
    if (allocated(failureinfo%previous)) then
      call this%log_test_base_failure(failureinfo%previous)
      writesepline = .true.
    end if
    sync all
    select type (failureinfo)
    class is (coa_failure_info)
      firstfailing = findloc(failureinfo%failedimages, .true., dim=1)
      if (firstfailing == this_image()) then
        if (writesepline) write(stdout, "()")
        call failureinfo%write_formatted(stdout)
      end if
    end select

  end subroutine log_test_base_failure


  subroutine log_summary(this, suitestats, teststats)
    class(coa_logger), intent(inout) :: this
    integer, intent(in) :: suitestats(:), teststats(:)

    if (this_image() /= 1) return
    call this%serial_logger%log_summary(suitestats, teststats)

  end subroutine log_summary

end module fortuno_coarray_coalogger
