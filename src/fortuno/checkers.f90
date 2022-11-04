module fortuno_checkers
  use fortuno_checkresult, only : check_result
  use fortuno_failuredetails, only : failure_details
  implicit none

  private
  public :: is_equal


  interface is_equal
    module procedure is_equal_i0_i0
  end interface


  type, extends(failure_details) :: i0_i0_details
    integer :: obtained
    integer :: expected
  contains
    procedure :: write_formatted => i0_i0_details_write_formatted
  end type i0_i0_details

contains


  subroutine i0_i0_details_write_formatted(this, unit)
    class(i0_i0_details), intent(in) :: this
    integer, intent(in) :: unit

    write(unit, "(a)") "Mismatching integer values"
    write(unit, "(a, i0)") "Obtained: ", this%obtained
    write(unit, "(a, i0)") "Expected: ", this%expected

  end subroutine i0_i0_details_write_formatted


  function is_equal_i0_i0(obtained, expected) result(checkresult)
    integer, intent(in) :: obtained, expected
    type(check_result) :: checkresult

    checkresult%success = obtained == expected
    if (checkresult%success) return
    checkresult%failuredet = i0_i0_details(obtained, expected)

  end function is_equal_i0_i0

end module  fortuno_checkers
