module fortuno_mpi_mpilogger
    use iso_fortran_env, only : stdout => output_unit
    use mpi_f08, only : MPI_Barrier, MPI_Comm
    use fortuno_seriallogger, only : serial_logger
    use fortuno_mpi_mpifailureinfo, only : mpi_failure_info
    use fortuno_testlogger, only : driver_result, test_status, test_name_str
    use fortuno_utils, only : nr_digits
    implicit none

    type, extends(serial_logger) :: mpi_logger
      type(MPI_Comm) :: mpicomm
      integer :: myrank = 0
    contains
      procedure :: begin_short_log
      procedure :: end_short_log
      procedure :: short_log_result
      procedure :: log_results
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


    subroutine log_results(this, driverresult)
      class(mpi_logger), intent(inout) :: this
      type(driver_result), intent(in) :: driverresult

      integer :: nsucceeded, nfailed, icase, first_failing

      nsucceeded = 0
      nfailed = 0

      do icase = 1, size(driverresult%caseresults)
        associate (&
            & suiteresult => driverresult%suiteresults(driverresult%casetosuite(icase)),&
            & caseresult => driverresult%caseresults(icase)&
            &)
          if (.not. caseresult%success) then
            nfailed = nfailed + 1
            if (allocated(caseresult%failureinfo)) then
              select type (failureinfo => caseresult%failureinfo)
              class is (mpi_failure_info)
                first_failing = findloc(failureinfo%failedranks, .true., dim=1) - 1
                if (first_failing == this%myrank) call this%log_failure(suiteresult, caseresult)
              end select
            ! If failureinfo is not allocated, first failing node can not be recovered
            ! Rank 0 should try to output something useful.
            else if (this%myrank == 0) then
              call this%log_failure(suiteresult, caseresult)
            end if
            call MPI_Barrier(this%mpicomm)
          else
            nsucceeded = nsucceeded + 1
          end if
        end associate
      end do

      if (this%myrank == 0) call this%log_summary(nsucceeded, nfailed)

    end subroutine log_results


  end module fortuno_mpi_mpilogger
