module fortuno_contextfactory
  use fortuno_contextbase, only : context_base
  use fortuno_suitebase, only : suite_base
  use fortuno_testbase, only : test_base
  implicit none

  private
  public :: context_factory


  type, abstract :: context_factory
  contains
    procedure(create_context_i), deferred :: create_context
  end type context_factory


  abstract interface

    subroutine create_context_i(this, testsuite, ctx)
      import :: context_factory, test_base, context_base, suite_base
      implicit none
      class(context_factory), intent(in) :: this
      class(suite_base), pointer, intent(in) :: testsuite
      class(context_base), allocatable, intent(out) :: ctx
    end subroutine create_context_i

  end interface

end module fortuno_contextfactory