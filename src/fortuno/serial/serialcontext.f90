module fortuno_serial_serialcontext
  use fortuno_genericcontext, only : generic_context
  implicit none

  private
  public :: serial_context


  type, extends(generic_context) :: serial_context
  end type serial_context

end module fortuno_serial_serialcontext
