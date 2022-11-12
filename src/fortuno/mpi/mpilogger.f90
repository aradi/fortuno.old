module fortuno_mpi_mpilogger
  use iso_fortran_env, only : stdout => output_unit
  use mpi_f08, only : MPI_Barrier, MPI_Comm
  use fortuno_failureinfo, only : failure_info
  use fortuno_serial_seriallogger, only : serial_logger
  use fortuno_mpi_mpifailureinfo, only : mpi_failure_info
  use fortuno_testlogger, only : test_result
  use fortuno_utils, only : findloc_logical
  implicit none

  private
  public :: mpi_logger


  type, extends(serial_logger) :: mpi_logger
    type(MPI_Comm) :: mpicomm
    integer :: myrank = 0
  contains
    procedure :: begin_short_log
    procedure :: short_log_result
    procedure :: end_short_log
    procedure :: begin_test_base_failure_log
    procedure :: log_test_base_failure
    procedure :: end_test_base_failure_log
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


  subroutine short_log_result(this, testtype, suiteresult, caseresult)
    class(mpi_logger), intent(inout) :: this
    integer, intent(in) :: testtype
    type(test_result), intent(in) :: suiteresult
    type(test_result), optional, intent(in) :: caseresult

    if (this%myrank /= 0) return
    call this%serial_logger%short_log_result(testtype, suiteresult, caseresult)

  end subroutine short_log_result


  subroutine begin_test_base_failure_log(this, testtype, suiteresult, caseresult)
    class(mpi_logger), intent(inout) :: this
    integer, intent(in) :: testtype
    type(test_result), intent(in) :: suiteresult
    type(test_result), optional, intent(in) :: caseresult

    if (this%myrank /= 0) return
    call this%serial_logger%begin_test_base_failure_log(testtype, suiteresult, caseresult)

  end subroutine begin_test_base_failure_log


  subroutine end_test_base_failure_log(this)
    class(mpi_logger), intent(inout) :: this

    call MPI_Barrier(this%mpicomm)
    if (this%myrank /= 0) return
    call this%serial_logger%end_test_base_failure_log()

  end subroutine end_test_base_failure_log


  recursive subroutine log_test_base_failure(this, failureinfo)
    class(mpi_logger), intent(inout) :: this
    class(failure_info), intent(in) :: failureinfo

    integer :: firstfailing
    logical :: writesepline

    writesepline = .false.
    if (allocated(failureinfo%previous)) then
      call this%log_test_base_failure(failureinfo%previous)
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

  end subroutine log_test_base_failure

  subroutine log_summary(this, suitestats, casestats)
    class(mpi_logger), intent(inout) :: this
    integer, intent(in) :: suitestats(:), casestats(:)

    if (this%myrank /= 0) return
    call this%serial_logger%log_summary(suitestats, casestats)

  end subroutine log_summary

end module fortuno_mpi_mpilogger
