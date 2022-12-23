module fortuno_serial_serialtest
  use fortuno_testbase, only : test_base
  use fortuno_utils, only : keyword_arg_enforcer_
  implicit none

  private
  public :: serial_test_base, serial_test_base_cls, serial_test, serial_fixtured_test


  type, extends(test_base), abstract :: serial_test_base
  contains
    procedure(serial_test_base_run_i), deferred :: run
  end type serial_test_base


  type :: serial_test_base_cls
    class(serial_test_base), allocatable :: instance
  end type serial_test_base_cls


  abstract interface
    subroutine serial_test_base_run_i(this)
      import :: serial_test_base
      implicit none
      class(serial_test_base), intent(inout) :: this
    end subroutine serial_test_base_run_i
  end interface


  type, extends(serial_test_base) :: serial_test
    procedure(serial_test_testroutine_i), nopass, pointer :: testroutine
    type(keyword_arg_enforcer_) :: kwargsonly = keyword_arg_enforcer_()
  contains
    procedure :: run => serial_test_run
  end type serial_test


  abstract interface
    subroutine serial_test_testroutine_i()
    end subroutine serial_test_testroutine_i
  end interface


  type, extends(serial_test_base) :: serial_fixtured_test
    procedure(serial_fixtured_test_testroutine_i), pointer :: testroutine
    type(keyword_arg_enforcer_) :: kwargsonly = keyword_arg_enforcer_()
  contains
    procedure :: run => serial_fixtured_test_run
    procedure :: set_up => serial_fixtured_test_set_up
    procedure :: tear_down => serial_fixtured_test_tear_down
  end type serial_fixtured_test


  abstract interface
    subroutine serial_fixtured_test_testroutine_i(this)
      import :: serial_fixtured_test
      implicit none
      class(serial_fixtured_test), intent(in) :: this
    end subroutine serial_fixtured_test_testroutine_i
  end interface


contains


  subroutine serial_test_run(this)
    class(serial_test), intent(inout) :: this

    call this%testroutine()

  end subroutine serial_test_run


  subroutine serial_fixtured_test_run(this)
    class(serial_fixtured_test), intent(inout) :: this

    call this%set_up()
    call this%testroutine()
    call this%tear_down()

  end subroutine serial_fixtured_test_run


  subroutine serial_fixtured_test_set_up(this)
    class(serial_fixtured_test), intent(inout) :: this

  end subroutine serial_fixtured_test_set_up


  subroutine serial_fixtured_test_tear_down(this)
    class(serial_fixtured_test), intent(inout) :: this

  end subroutine serial_fixtured_test_tear_down

end module fortuno_serial_serialtest
