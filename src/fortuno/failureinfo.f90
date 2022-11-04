module fortuno_failureinfo
  use fortuno_failuredetails, only : failure_details
  implicit none

  private
  public :: failure_info


  type :: failure_info
    character(:), allocatable :: message
    character(:), allocatable :: file
    integer :: line = 0
    integer :: checknr = 0
    class(failure_details), allocatable :: failuredet
    class(failure_info), allocatable :: previous
  contains
    procedure :: write_formatted
  end type failure_info

contains


  subroutine write_formatted(this, unit)
    class(failure_info), intent(in) :: this
    integer, intent(in) :: unit

    if (allocated(this%file)) then
      write(unit, "(a, t12, 2a, i0)") "Location:", this%file, ":", this%line
    else if (this%checknr /= 0) then
      write(unit, "(a, t12, i0)") "Check:", this%checknr
    end if
    if (allocated(this%message)) then
      write(unit, "(a, t12, a)") "Message: ", this%message
    end if
    if (allocated(this%failuredet)) then
      write(unit, "(a)") "::"
      call this%failuredet%write_formatted(unit)
    end if

  end subroutine write_formatted

end module fortuno_failureinfo