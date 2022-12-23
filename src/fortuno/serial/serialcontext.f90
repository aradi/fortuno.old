module fortuno_serial_serialcontext
  use fortuno_contextbase, only : context_base
  implicit none

  private
  public :: serial_context


  type, extends(context_base) :: serial_context
  end type serial_context

end module fortuno_serial_serialcontext
