module fortuno_coarray_coatest
  use fortuno_coarray_coacontext, only : coa_context
  use fortuno_coarray_coadriver, only : coa_test_case
  implicit none

  private
  public :: coa_test


  type, extends(coa_test_case) :: coa_test
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
