module fortuno_serial_serialtest
  use fortuno_serial_serialcontext, only : serial_context
  use fortuno_serial_serialdriver, only : serial_test_case
  implicit none

  private
  public :: serial_test


  type, extends(serial_test_case) :: serial_test
    procedure(test_routine_iface), nopass, pointer :: testroutine
  contains
    procedure :: run
  end type serial_test


  abstract interface

    subroutine test_routine_iface(ctx)
      import :: serial_context
      class(serial_context), intent(inout) :: ctx
    end subroutine test_routine_iface

  end interface

contains


  subroutine run(this, ctx)
    class(serial_test), intent(inout) :: this
    class(serial_context), intent(inout) :: ctx

    call this%testroutine(ctx)

  end subroutine run


end module fortuno_serial_serialtest
