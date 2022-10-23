module fortuno_serial_serialcontext
  use fortuno_basetypes, only : test_context
  implicit none

  private
  public :: serial_context


  type, extends(test_context) :: serial_context
  end type serial_context

end module fortuno_serial_serialcontext
