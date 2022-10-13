module fortuno_coarray_coafailureinfo
  use fortuno_failureinfo, only : failure_info
  implicit none

  private
  public :: coa_failure_info


  type, extends(failure_info) :: coa_failure_info
    logical, allocatable :: failedimages(:)
  contains
    procedure :: write_formatted
  end type coa_failure_info


contains


  subroutine write_formatted(this, unit)
    class(coa_failure_info),intent(in) :: this
    integer, intent(in) :: unit

    integer :: firstfailed, totalfailed

    if (allocated(this%failedimages)) then
      firstfailed = findloc(this%failedimages, .true., dim=1)
      totalfailed = count(this%failedimages, dim=1)
      if (totalfailed > 1) then
        write(unit, "(a, t12, i0, a, i0, a)") "Image:", firstfailed, "  (+ ", totalfailed - 1,&
            & " others)"
      else
        write(unit, "(a, t12, i0)") "Image:", firstfailed
      end if
    end if
    call this%failure_info%write_formatted(unit)

  end subroutine write_formatted

end module fortuno_coarray_coafailureinfo
