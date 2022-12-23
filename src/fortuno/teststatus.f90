module fortuno_teststatus
  implicit none

  private
  public :: teststatus


  type :: test_status_enum_
    integer :: ok = 0
    integer :: failed = 1
    integer :: skipped = 2
  end type test_status_enum_

  type(test_status_enum_), parameter :: teststatus = test_status_enum_()

end module fortuno_teststatus