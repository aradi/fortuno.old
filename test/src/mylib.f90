module mylib
  implicit none

  private
  public :: factorial

contains


  function factorial(nn) result(fact)
    integer, intent(in) :: nn
    integer :: fact

    integer :: ii

    fact = 1
    do ii = 2, nn
      fact = fact * ii
    end do

  end function factorial

end module mylib
