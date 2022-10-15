module fortuno_simpletest
  use fortuno_basetypes, only : test_case, test_context
  use fortuno_serialcontext, only : serial_context, serial_context_ptr
  implicit none

  private
  public :: simple_test


  type, extends(test_case) :: simple_test
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
    class(test_context), pointer, intent(in) :: ctx

    class(serial_context), pointer :: serialctx

    serialctx => serial_context_ptr(ctx)
    call this%testroutine(serialctx)

  end subroutine run


end module fortuno_simpletest
