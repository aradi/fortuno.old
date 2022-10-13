module fortuno_contextfactory
  use fortuno_basetypes, only : test_context, test_suite, test_case
  implicit none

  private
  public :: context_factory


  type, abstract :: context_factory
  contains
    procedure(create_context_iface), deferred :: create_context
  end type context_factory


  abstract interface

    subroutine create_context_iface(this, testsuite, testcase, ctx)
      import :: context_factory, test_case, test_context, test_suite
      implicit none
      class(context_factory), intent(in) :: this
      class(test_suite), pointer, intent(in) :: testsuite
      class(test_case), pointer, intent(in) :: testcase
      class(test_context), allocatable, intent(out) :: ctx
    end subroutine create_context_iface

  end interface


end module fortuno_contextfactory