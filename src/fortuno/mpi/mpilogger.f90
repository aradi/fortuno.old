module fortuno_mpi_mpilogger
  use iso_fortran_env, only : stdout => output_unit
  use mpi_f08, only : MPI_Barrier, MPI_Comm
  use fortuno_failureinfo, only : failure_info
  use fortuno_serial_seriallogger, only : serial_logger
  use fortuno_mpi_mpifailureinfo, only : mpi_failure_info
  use fortuno_testlogger, only : driver_result, test_status, test_name_str
  use fortuno_utils, only : findloc_logical, nr_digits
  implicit none

  type, extends(serial_logger) :: mpi_logger
    type(MPI_Comm) :: mpicomm
    integer :: myrank = 0
  contains
    procedure :: begin_short_log
    procedure :: short_log_result
    procedure :: end_short_log
    procedure :: begin_test_case_failure_log
    procedure :: log_test_case_failure
    procedure :: end_test_case_failure_log
    procedure :: log_summary
  end type mpi_logger


contains


  subroutine begin_short_log(this)
    class(mpi_logger), intent(inout) :: this

    if (this%myrank /= 0) return
    call this%serial_logger%begin_short_log()

  end subroutine begin_short_log


  subroutine end_short_log(this)
    class(mpi_logger), intent(inout) :: this

    if (this%myrank /= 0) return
    call this%serial_logger%end_short_log()

  end subroutine end_short_log


  subroutine short_log_result(this, suitename, casename, success)
    class(mpi_logger), intent(inout) :: this
    character(*), intent(in) :: suitename, casename
    logical, intent(in) :: success

    if (this%myrank /= 0) return
    call this%serial_logger%short_log_result(suitename, casename, success)

  end subroutine short_log_result


  subroutine begin_test_case_failure_log(this, suiteresult, caseresult)
    class(mpi_logger), intent(inout) :: this
    type(test_status), intent(in) :: suiteresult, caseresult

    if (this%myrank /= 0) return
    call this%serial_logger%begin_test_case_failure_log(suiteresult, caseresult)

  end subroutine begin_test_case_failure_log


  subroutine end_test_case_failure_log(this)
    class(mpi_logger), intent(inout) :: this

    call MPI_Barrier(this%mpicomm)
    if (this%myrank /= 0) return
    call this%serial_logger%end_test_case_failure_log()

  end subroutine end_test_case_failure_log


  recursive subroutine log_test_case_failure(this, failureinfo)
    class(mpi_logger), intent(inout) :: this
    class(failure_info), intent(in) :: failureinfo

    integer :: firstfailing
    logical :: writesepline

    writesepline = .false.
    if (allocated(failureinfo%previous)) then
      call this%log_test_case_failure(failureinfo%previous)
      writesepline = .true.
    end if
    call MPI_Barrier(this%mpicomm)
    select type (failureinfo)
    class is (mpi_failure_info)
      firstfailing = findloc_logical(failureinfo%failedranks, .true.) - 1
      if (firstfailing == this%myrank) then
        if (writesepline) write(stdout, "()")
        call failureinfo%write_formatted(stdout)
      end if
    end select

  end subroutine log_test_case_failure


  subroutine log_summary(this, nsucceeded, nfailed)
    class(mpi_logger), intent(inout) :: this
    integer, intent(in) :: nsucceeded, nfailed

    if (this%myrank /= 0) return
    call this%serial_logger%log_summary(nsucceeded, nfailed)

  end subroutine log_summary

end module fortuno_mpi_mpilogger
