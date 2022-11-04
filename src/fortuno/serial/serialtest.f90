module fortuno_serial_serialtest
  use fortuno_basetypes, only : test_base
  use fortuno_serial_serialcontext, only : serial_context
  implicit none

  private
  public :: serial_test_base, serial_test


  type, extends(test_base), abstract :: serial_test_base
  contains
    procedure(serial_test_base_run_iface), deferred :: run
  end type serial_test_base


  abstract interface
    subroutine serial_test_base_run_iface(this, ctx)
      import :: serial_test_base, serial_context
      class(serial_test_base), intent(inout) :: this
      class(serial_context), intent(inout) :: ctx
    end subroutine serial_test_base_run_iface
  end interface


  type, extends(serial_test_base) :: serial_test
    procedure(serial_test_testroutine_iface), nopass, pointer :: testroutine
  contains
    procedure :: run
  end type serial_test


  abstract interface
    subroutine serial_test_testroutine_iface(ctx)
      import :: serial_context
      class(serial_context), intent(inout) :: ctx
    end subroutine serial_test_testroutine_iface
  end interface

contains


  subroutine run(this, ctx)
    class(serial_test), intent(inout) :: this
    class(serial_context), intent(inout) :: ctx

    call this%testroutine(ctx)

  end subroutine run

end module fortuno_serial_serialtest
