module fortuno_serial_serialcontext
  use fortuno_basetypes, only : test_context
  implicit none

  private
  public :: serial_context, serial_context_ptr


  type, extends(test_context) :: serial_context
  end type serial_context

contains

  function serial_context_ptr(ctx)
    class(test_context), pointer, intent(in) :: ctx
    class(serial_context), pointer :: serial_context_ptr

    select type (ctx)
    class is (serial_context)
      serial_context_ptr => ctx
    class default
      error stop "serialtest.f90::expected serial_context class pointer"
    end select

  end function serial_context_ptr

end module fortuno_serial_serialcontext
