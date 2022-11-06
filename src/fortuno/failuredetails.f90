module fortuno_failuredetails
  implicit none

  private
  public :: failure_details


  type, abstract :: failure_details
  contains
    procedure(write_formatted_i), deferred :: write_formatted
  end type failure_details


  abstract interface

    subroutine write_formatted_i(this, unit)
      import :: failure_details
      implicit none
      class(failure_details), intent(in) :: this
      integer, intent(in) :: unit
    end subroutine write_formatted_i

  end interface

end module fortuno_failuredetails
