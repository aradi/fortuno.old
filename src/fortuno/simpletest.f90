module fortuno_simpletest
  use fortuno_serialcontext, only : serial_context
  use fortuno_serialdriver, only : serial_test_case
  implicit none

  private
  public :: simple_test


  type, extends(serial_test_case) :: simple_test
    procedure(test_routine_iface), nopass, pointer :: testroutine
  contains
    procedure :: run
  end type simple_test


  abstract interface

    subroutine test_routine_iface(ctx)
      import :: serial_context
      class(serial_context), intent(inout) :: ctx
    end subroutine test_routine_iface

  end interface

contains


  subroutine run(this, ctx)
    class(simple_test), intent(inout) :: this
    class(serial_context), pointer, intent(in) :: ctx

    call this%testroutine(ctx)

  end subroutine run


end module fortuno_simpletest
