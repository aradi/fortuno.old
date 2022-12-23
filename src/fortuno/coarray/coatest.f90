module fortuno_coarray_coatest
  use fortuno_coarray_coacontext, only : coa_context
  use fortuno_testbase, only : test_base
  implicit none

  private
  public :: coa_fixtured_test, coa_test, coa_test_base, coa_test_base_cls


  type, extends(test_base), abstract :: coa_test_base
  contains
    procedure(coa_test_base_run_i), deferred :: run
  end type coa_test_base


  abstract interface
    subroutine coa_test_base_run_i(this)
      import :: coa_test_base, coa_context
      class(coa_test_base), intent(inout) :: this
    end subroutine coa_test_base_run_i
  end interface


  type :: coa_test_base_cls
    class(coa_test_base), allocatable :: instance
  end type coa_test_base_cls


  type, extends(coa_test_base) :: coa_test
    procedure(coa_test_testroutine_i), nopass, pointer :: testroutine
  contains
    procedure :: run => coa_test_run
  end type coa_test


  abstract interface
    subroutine coa_test_testroutine_i()
    end subroutine coa_test_testroutine_i
  end interface


  type, extends(coa_test_base) :: coa_fixtured_test
    procedure(coa_fixtured_test_testroutine_i), pointer :: testroutine
  contains
    procedure :: run => coa_fixtured_test_run
    procedure :: set_up => coa_fixtured_test_set_up
    procedure :: tear_down => coa_fixtured_test_tear_down
  end type coa_fixtured_test


  abstract interface
    subroutine coa_fixtured_test_testroutine_i(this)
      import :: coa_fixtured_test
      implicit none
      class(coa_fixtured_test), intent(in) :: this
    end subroutine coa_fixtured_test_testroutine_i
  end interface

contains


  subroutine coa_test_run(this)
    class(coa_test), intent(inout) :: this

    call this%testroutine()

  end subroutine coa_test_run


  subroutine coa_fixtured_test_run(this)
    class(coa_fixtured_test), intent(inout) :: this

    call this%set_up()
    call this%testroutine()
    call this%tear_down()

  end subroutine coa_fixtured_test_run


  subroutine coa_fixtured_test_set_up(this)
    class(coa_fixtured_test), intent(inout) :: this

  end subroutine coa_fixtured_test_set_up


  subroutine coa_fixtured_test_tear_down(this)
    class(coa_fixtured_test), intent(inout) :: this

  end subroutine coa_fixtured_test_tear_down

end module fortuno_coarray_coatest
