module fortuno_coarray_coatest
  use fortuno_basetypes, only : test_case, test_context
  use fortuno_coarray_coacontext, only : coa_context, coa_context_ptr
  implicit none

  private
  public :: coa_test


  type, extends(test_case) :: coa_test
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
    class(test_context), pointer, intent(in) :: ctx

    class(coa_context), pointer :: coactx

    coactx => coa_context_ptr(ctx)
    call this%testroutine(coactx)

  end subroutine run

end module fortuno_coarray_coatest
