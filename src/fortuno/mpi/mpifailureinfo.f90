module fortuno_mpi_mpifailureinfo
  use fortuno_failureinfo, only : failure_info
  use fortuno_utils, only : findloc_logical
  implicit none

  private
  public :: mpi_failure_info


  type, extends(failure_info) :: mpi_failure_info
    logical, allocatable :: failedranks(:)
  contains
    procedure :: write_formatted
  end type mpi_failure_info


contains


  subroutine write_formatted(this, unit)
    class(mpi_failure_info),intent(in) :: this
    integer, intent(in) :: unit

    integer :: firstfailed, totalfailed

    if (allocated(this%failedranks)) then
      firstfailed = findloc_logical(this%failedranks, .true.) - 1
      totalfailed = count(this%failedranks, dim=1)
      if (totalfailed > 1) then
        write(unit, "(a, t12, i0, a, i0, a)") "Rank:", firstfailed, "  (+ ", totalfailed - 1,&
            & " more)"
      else
        write(unit, "(a, t12, i0)") "Rank:", firstfailed
      end if
    end if
    call this%failure_info%write_formatted(unit)

  end subroutine write_formatted

end module fortuno_mpi_mpifailureinfo
