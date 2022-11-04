module fortuno_serial_seriallogger
  use iso_fortran_env, only : stdout => output_unit
  use fortuno_failureinfo, only : failure_info
  use fortuno_testlogger, only : driver_result, test_logger, test_name_str, test_status
  use fortuno_utils, only : nr_digits
  implicit none

  private
  public :: serial_logger


  type, extends(test_logger) :: serial_logger
  contains
    procedure :: begin_short_log
    procedure :: short_log_result
    procedure :: end_short_log
    procedure :: log_results
    procedure :: begin_test_base_failure_log
    procedure :: log_test_base_failure
    procedure :: end_test_base_failure_log
    procedure :: log_summary
  end type serial_logger

contains


  subroutine begin_short_log(this)
    class(serial_logger), intent(inout) :: this

  end subroutine begin_short_log


  subroutine end_short_log(this)
    class(serial_logger), intent(inout) :: this

    write(stdout, "(/)")

  end subroutine end_short_log


  subroutine short_log_result(this, suitename, casename, success)
    class(serial_logger), intent(inout) :: this
    character(*), intent(in) :: suitename, casename
    logical, intent(in) :: success

    if (success) then
      write(stdout, "(a)", advance="no") "[Passed] "
    else
      write(stdout, "(a)", advance="no") "[FAILED] "
    end if
    write(stdout, "(3a)") suitename, "/", casename

  end subroutine short_log_result


  subroutine log_results(this, driverresult)
    class(serial_logger), intent(inout) :: this
    type(driver_result), intent(in) :: driverresult

    integer :: nsucceeded, nfailed, icase

    nsucceeded = 0
    nfailed = 0

    do icase = 1, size(driverresult%caseresults)
      associate (&
          & suiteresult => driverresult%suiteresults(driverresult%casetosuite(icase)),&
          & caseresult => driverresult%caseresults(icase)&
          &)
        if (.not. caseresult%success) then
          nfailed = nfailed + 1
          call this%begin_test_base_failure_log(suiteresult, caseresult)
          if (allocated(caseresult%failureinfo)) then
            call this%log_test_base_failure(caseresult%failureinfo)
          end if
          call this%end_test_base_failure_log()
        else
          nsucceeded = nsucceeded + 1
        end if
      end associate
    end do

    call this%log_summary(nsucceeded, nfailed)

  end subroutine log_results


  subroutine begin_test_base_failure_log(this, suiteresult, caseresult)
    class(serial_logger), intent(inout) :: this
    type(test_status), intent(in) :: suiteresult, caseresult

    write(stdout, "(a, t12, a, /)") "# FAILED: ", test_name_str(suiteresult, caseresult)

  end subroutine begin_test_base_failure_log


  recursive subroutine log_test_base_failure(this, failureinfo)
    class(serial_logger), intent(inout) :: this
    class(failure_info), intent(in) :: failureinfo

    if (allocated(failureinfo%previous)) then
      call this%log_test_base_failure(failureinfo%previous)
      write(stdout, "()")
    end if
    call failureinfo%write_formatted(stdout)

  end subroutine log_test_base_failure


  subroutine end_test_base_failure_log(this)
    class(serial_logger), intent(inout) :: this

    write(stdout, "(/)")

  end subroutine end_test_base_failure_log


  subroutine log_summary(this, nsucceeded, nfailed)
    class(serial_logger), intent(inout) :: this
    integer, intent(in) :: nsucceeded, nfailed

    character(100) :: formstr
    integer :: ntests, fieldwidth

    ntests = nfailed + nsucceeded
    fieldwidth = nr_digits(ntests)

    write(formstr, "(a, i0, a)") "('Total tests: ', i", fieldwidth, ")"
    write(stdout, formstr) ntests
    write(formstr, "(a, i0, a)") "('Succeeded:   ', i", fieldwidth, ", '  (', I3, '%)')"
    write(stdout, formstr) nsucceeded, int(real(nsucceeded) / real(ntests) * 100.0)
    write(formstr, "(a, i0, a)") "('Failed:      ', i", fieldwidth, ", '  (', I3, '%)')"
    write(stdout, formstr) nfailed, int(real(nfailed) / real(ntests) * 100.0)
    write(stdout, "()")

  end subroutine log_summary

end module fortuno_serial_seriallogger
