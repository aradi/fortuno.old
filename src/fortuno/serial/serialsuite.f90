module fortuno_serial_serialsuite
  use fortuno_basetypes, only : init_suite_base, suite_base
  use fortuno_serial_serialcontext, only : serial_context
  use fortuno_serial_serialtest, only : serial_test_base
  implicit none

  private
  public :: serial_suite, serial_suite_base


  type, extends(suite_base) :: serial_suite_base
  contains
    procedure :: set_up => serial_suite_base_set_up
    procedure :: tear_down => serial_suite_base_tear_down
  end type serial_suite_base


  type, extends(serial_suite_base) :: serial_suite
  end type serial_suite


  interface serial_suite
    module procedure new_serial_suite
  end interface serial_suite

contains


  subroutine serial_suite_base_set_up(this, ctx)
    class(serial_suite_base), intent(inout) :: this
    class(serial_context), intent(inout) :: ctx

  end subroutine serial_suite_base_set_up


  subroutine serial_suite_base_tear_down(this, ctx)
    class(serial_suite_base), intent(inout) :: this
    class(serial_context), intent(inout) :: ctx

  end subroutine serial_suite_base_tear_down


  function new_serial_suite(name, tests) result(this)
    character(*), intent(in) :: name
    class(serial_test_base), optional, intent(in) :: tests(:)
    type(serial_suite) :: this

    call init_suite_base(this, name, tests)

  end function new_serial_suite

end module fortuno_serial_serialsuite