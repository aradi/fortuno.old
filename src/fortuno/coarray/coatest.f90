module fortuno_coarray_coatest
  use fortuno_coarray_coacontext, only : coa_context
  use fortuno_testbase, only : test_base
  implicit none

  private
  public :: coa_test, coa_test_base, coa_test_base_cls


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
    procedure(coa_test_proc_i), nopass, pointer :: proc
  contains
    procedure :: run => coa_test_run
  end type coa_test


  abstract interface
    subroutine coa_test_proc_i()
    end subroutine coa_test_proc_i
  end interface

contains


  subroutine coa_test_run(this)
    class(coa_test), intent(inout) :: this

    call this%proc()

  end subroutine coa_test_run

end module fortuno_coarray_coatest
