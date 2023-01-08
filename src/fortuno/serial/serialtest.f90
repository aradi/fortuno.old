module fortuno_serial_serialtest
  use fortuno_generictest, only : generic_test
  use fortuno_utils, only : keyword_arg_enforcer_
  implicit none

  private
  public :: serial_test_base, serial_test_base_cls, serial_test


  type, extends(generic_test), abstract :: serial_test_base
  contains
    procedure(serial_test_base_run_i), deferred :: run
  end type serial_test_base


  abstract interface
    subroutine serial_test_base_run_i(this)
      import :: serial_test_base
      implicit none
      class(serial_test_base), intent(inout) :: this
    end subroutine serial_test_base_run_i
  end interface


  type :: serial_test_base_cls
    class(serial_test_base), allocatable :: instance
  end type serial_test_base_cls


  type, extends(serial_test_base) :: serial_test
    procedure(serial_test_testroutine_i), nopass, pointer :: proc
  contains
    procedure :: run => serial_test_run
  end type serial_test


  abstract interface
    subroutine serial_test_testroutine_i()
    end subroutine serial_test_testroutine_i
  end interface

contains


  subroutine serial_test_run(this)
    class(serial_test), intent(inout) :: this

    call this%proc()

  end subroutine serial_test_run

end module fortuno_serial_serialtest
