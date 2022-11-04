module fortuno_coarray_coatest
  use fortuno_basetypes, only : test_base
  use fortuno_coarray_coacontext, only : coa_context
  implicit none

  private
  public :: coa_test, coa_test_base


  type, extends(test_base), abstract :: coa_test_base
  contains
    procedure(coa_test_base_run_iface), deferred :: run
  end type coa_test_base


  abstract interface
    subroutine coa_test_base_run_iface(this, ctx)
      import :: coa_test_base, coa_context
      class(coa_test_base), intent(inout) :: this
      class(coa_context), intent(inout) :: ctx
    end subroutine coa_test_base_run_iface
  end interface


  type, extends(coa_test_base) :: coa_test
    procedure(test_routine_iface), nopass, pointer :: testroutine
  contains
    procedure :: run
  end type coa_test


  abstract interface
    subroutine test_routine_iface(ctx)
      import :: coa_context
      class(coa_context), intent(inout) :: ctx
    end subroutine test_routine_iface
  end interface

contains


  subroutine run(this, ctx)
    class(coa_test), intent(inout) :: this
    class(coa_context), intent(inout) :: ctx

    call this%testroutine(ctx)

  end subroutine run

end module fortuno_coarray_coatest
