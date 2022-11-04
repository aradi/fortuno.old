module fortuno_testerror
  implicit none

  private
  public :: test_error

  type :: test_error
    integer :: code = 0
    character(:), allocatable :: message
  contains
    procedure :: write_formatted
    generic :: write(formatted) => write_formatted
  end type test_error

contains


  subroutine write_formatted(this, unit, iotype, vlist, iostat, iomsg)
    class(test_error), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated(this%message)) then
      write(unit, "(a)", iostat=iostat, iomsg=iomsg) this%message
    else
      iostat = 0
      iomsg = ""
    end if

  end subroutine write_formatted

end module fortuno_testerror
