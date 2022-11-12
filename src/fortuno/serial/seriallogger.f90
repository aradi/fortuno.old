module fortuno_serial_seriallogger
  use iso_fortran_env, only : stdout => output_unit
  use fortuno_basetypes, only : teststatus
  use fortuno_failureinfo, only : failure_info
  use fortuno_testlogger, only : driver_result, test_logger, test_name_str, test_result, testtypes
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
    procedure, private :: log_case_results_
    procedure, private :: log_suite_results_
  end type serial_logger

contains


  subroutine begin_short_log(this)
    class(serial_logger), intent(inout) :: this

  end subroutine begin_short_log


  subroutine end_short_log(this)
    class(serial_logger), intent(inout) :: this

    write(stdout, "(/)")

  end subroutine end_short_log


  subroutine short_log_result(this, testtype, suiteresult, caseresult)
    class(serial_logger), intent(inout) :: this
    integer, intent(in) :: testtype
    type(test_result), intent(in) :: suiteresult
    type(test_result), optional, intent(in) :: caseresult

    integer :: status

    if (testtype == testtypes%caserun) then
      status = caseresult%status
    else
      status = suiteresult%status
    end if

    select case (status)
    case (teststatus%ok)
      write(stdout, "(a)", advance="no") "| OK      | "
    case (teststatus%failed)
      write(stdout, "(a)", advance="no") "| FAILED  | "
    case (teststatus%skipped)
      write(stdout, "(a)", advance="no") "| Skipped | "
    case default
      write(stdout, "(a)", advance="no") "??????? | "
    end select

    write(stdout, "(a)") test_name_str(testtype, suiteresult, caseresult)

  end subroutine short_log_result


  subroutine log_results(this, driverresult)
    class(serial_logger), intent(inout) :: this
    type(driver_result), intent(in) :: driverresult

    integer :: suitestats(3), casestats(3)

    call this%log_suite_results_(driverresult, suitestats)
    call this%log_case_results_(driverresult, casestats)
    call this%log_summary(suitestats, casestats)

  end subroutine log_results


  subroutine begin_test_base_failure_log(this, testtype, suiteresult, caseresult)
    class(serial_logger), intent(inout) :: this
    integer, intent(in) :: testtype
    type(test_result), intent(in) :: suiteresult
    type(test_result), optional, intent(in) :: caseresult

    write(stdout, "(a, t12, a, /)") "# FAILED:", test_name_str(testtype, suiteresult, caseresult)

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


  subroutine log_summary(this, suitestats, casestats)
    class(serial_logger), intent(inout) :: this
    integer, intent(in) :: suitestats(:), casestats(:)

    character(100) :: formstr
    integer :: ntests, fieldwidth

    ntests = sum(suitestats)
    fieldwidth = nr_digits(ntests)

    write(formstr, "(a, i0, a)") "('Total test suites: ', i", fieldwidth, ")"
    write(stdout, formstr) ntests
    write(formstr, "(a, i0, a)") "('Skipped:     ', i", fieldwidth, ", '  (', I3, '%)')"
    write(stdout, formstr) suitestats(2), int(real(suitestats(2)) / real(ntests) * 100.0)
    write(formstr, "(a, i0, a)") "('Passed:      ', i", fieldwidth, ", '  (', I3, '%)')"
    write(stdout, formstr) suitestats(1), int(real(suitestats(1)) / real(ntests) * 100.0)
    write(formstr, "(a, i0, a)") "('Failed:      ', i", fieldwidth, ", '  (', I3, '%)')"
    write(stdout, formstr) suitestats(3), int(real(suitestats(3)) / real(ntests) * 100.0)
    write(stdout, "()")

    ntests = sum(casestats)
    fieldwidth = nr_digits(ntests)

    write(formstr, "(a, i0, a)") "('Total test cases: ', i", fieldwidth, ")"
    write(stdout, formstr) ntests
    write(formstr, "(a, i0, a)") "('Skipped:     ', i", fieldwidth, ", '  (', I3, '%)')"
    write(stdout, formstr) casestats(2), int(real(casestats(2)) / real(ntests) * 100.0)
    write(formstr, "(a, i0, a)") "('Passed:      ', i", fieldwidth, ", '  (', I3, '%)')"
    write(stdout, formstr) casestats(1), int(real(casestats(1)) / real(ntests) * 100.0)
    write(formstr, "(a, i0, a)") "('Failed:      ', i", fieldwidth, ", '  (', I3, '%)')"
    write(stdout, formstr) casestats(3), int(real(casestats(3)) / real(ntests) * 100.0)
    write(stdout, "()")

  end subroutine log_summary


  subroutine log_case_results_(this, driverresult, stats)
    class(serial_logger), intent(inout) :: this
    type(driver_result), intent(in) :: driverresult
    integer, intent(out) :: stats(:)

    integer :: nsucceeded, nfailed, nskipped, icase

    nsucceeded = 0
    nfailed = 0
    nskipped = 0

    do icase = 1, size(driverresult%caseresults)
      associate (&
          & suiteresults => driverresult%suiteresults(:, driverresult%casetosuite(icase)),&
          & caseresult => driverresult%caseresults(icase)&
          &)
        select case (caseresult%status)
        case (teststatus%ok)
          nsucceeded = nsucceeded + 1
        case (teststatus%skipped)
          nskipped = nskipped + 1
        case (teststatus%failed)
          nfailed = nfailed + 1
          call this%begin_test_base_failure_log(testtypes%caserun, suiteresults(1), caseresult)
          if (allocated(caseresult%failureinfo)) then
            call this%log_test_base_failure(caseresult%failureinfo)
          end if
          call this%end_test_base_failure_log()
        end select
      end associate
    end do

    stats = [nsucceeded, nskipped, nfailed]

  end subroutine log_case_results_


  subroutine log_suite_results_(this, driverresult, stats)
    class(serial_logger), intent(inout) :: this
    type(driver_result), intent(in) :: driverresult
    integer, intent(out) :: stats(:)

    integer :: nsucceeded, nfailed, nskipped, isuite

    nsucceeded = 0
    nfailed = 0
    nskipped = 0

    do isuite = 1, size(driverresult%suiteresults, dim=2)
      associate (suiteresults => driverresult%suiteresults(:, isuite))
        select case (suiteresults(1)%status)
        case (teststatus%skipped)
          nskipped = nskipped + 1
          cycle
        case (teststatus%failed)
          nfailed = nfailed + 1
          call this%begin_test_base_failure_log(testtypes%suitesetup, suiteresults(1))
          if (allocated(suiteresults(1)%failureinfo)) then
            call this%log_test_base_failure(suiteresults(1)%failureinfo)
          end if
          call this%end_test_base_failure_log()
          cycle
        end select

        select case (suiteresults(2)%status)
        case (teststatus%ok, teststatus%skipped)
          nsucceeded = nsucceeded + 1
        case (teststatus%failed)
          nfailed = nfailed + 1
          call this%begin_test_base_failure_log(testtypes%suiteteardown, suiteresults(2))
          if (allocated(suiteresults(2)%failureinfo)) then
            call this%log_test_base_failure(suiteresults(2)%failureinfo)
          end if
          call this%end_test_base_failure_log()
          cycle
        end select

      end associate
    end do

    stats = [nsucceeded, nskipped, nfailed]

  end subroutine log_suite_results_

end module fortuno_serial_seriallogger
