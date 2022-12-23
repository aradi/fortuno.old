module fortuno_serial_serialsuite
  use fortuno_serial_serialtest, only : serial_test_base, serial_test_base_cls
  use fortuno_suitebase, only : init_suite_base, suite_base
  use fortuno_testbase, only : test_base_cls
  implicit none

  private
  public :: serial_suite, serial_suite_base, serial_suite_base_cls


  type, extends(suite_base) :: serial_suite_base
  contains
    procedure :: set_up => serial_suite_base_set_up
    procedure :: tear_down => serial_suite_base_tear_down
  end type serial_suite_base


  type :: serial_suite_base_cls
    class(serial_suite_base), allocatable :: instance
  end type serial_suite_base_cls


  type, extends(serial_suite_base) :: serial_suite
  end type serial_suite


  interface serial_suite
    module procedure new_serial_suite_test, new_serial_suite_test_cls
  end interface serial_suite

contains


  subroutine serial_suite_base_set_up(this)
    class(serial_suite_base), intent(inout) :: this

  end subroutine serial_suite_base_set_up


  subroutine serial_suite_base_tear_down(this)
    class(serial_suite_base), intent(inout) :: this

  end subroutine serial_suite_base_tear_down


  function new_serial_suite_test(name, tests) result(this)
    character(*), intent(in) :: name
    class(serial_test_base), optional, intent(in) :: tests(:)
    type(serial_suite) :: this

    call init_suite_base(this, name, tests)

  end function new_serial_suite_test


  function new_serial_suite_test_cls(name, tests) result(this)
    character(*), intent(in) :: name
    type(serial_test_base_cls), intent(in) :: tests(:)
    type(serial_suite) :: this

    type(test_base_cls), allocatable :: stbc(:)
    integer :: itest

    call init_suite_base(this, name)
    allocate(stbc(size(tests)))
    do itest = 1, size(tests)
      stbc(itest)%instance = tests(itest)%instance
    end do
    call this%add_test(stbc)

  end function new_serial_suite_test_cls

end module fortuno_serial_serialsuite