module fortuno_checkresult
  use fortuno_failuredetails, only : failure_details
  implicit none

  private
  public :: check_result


  type :: check_result
    logical :: success = .false.
    class(failure_details), allocatable :: failuredet
  end type check_result

end module fortuno_checkresult
