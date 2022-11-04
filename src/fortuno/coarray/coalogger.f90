module fortuno_coarray_coalogger
  use iso_fortran_env, only : stdout => output_unit
  use fortuno_coarray_coafailureinfo, only : coa_failure_info
  use fortuno_failureinfo, only : failure_info
  use fortuno_serial_seriallogger, only : serial_logger
  use fortuno_testlogger, only : driver_result, test_status, test_name_str
  use fortuno_utils, only : findloc_logical, nr_digits
  implicit none

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


  subroutine short_log_result(this, suitename, casename, success)
    class(coa_logger), intent(inout) :: this
    character(*), intent(in) :: suitename, casename
    logical, intent(in) :: success

    if (this_image() /= 1) return
    call this%serial_logger%short_log_result(suitename, casename, success)

  end subroutine short_log_result


  subroutine begin_test_base_failure_log(this, suiteresult, caseresult)
    class(coa_logger), intent(inout) :: this
    type(test_status), intent(in) :: suiteresult, caseresult

    if (this_image() /= 1) return
    call this%serial_logger%begin_test_base_failure_log(suiteresult, caseresult)

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
      firstfailing = findloc_logical(failureinfo%failedimages, .true.)
      if (firstfailing == this_image()) then
        if (writesepline) write(stdout, "()")
        call failureinfo%write_formatted(stdout)
      end if
    end select

  end subroutine log_test_base_failure


  subroutine log_summary(this, nsucceeded, nfailed)
    class(coa_logger), intent(inout) :: this
    integer, intent(in) :: nsucceeded, nfailed

    if (this_image() /= 1) return
    call this%serial_logger%log_summary(nsucceeded, nfailed)

  end subroutine log_summary

end module fortuno_coarray_coalogger
