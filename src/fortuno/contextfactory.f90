module fortuno_contextfactory
  use fortuno_genericcontext, only: generic_context
  use fortuno_genericsuite, only: generic_suite
  use fortuno_generictest, only: generic_test
  implicit none

  private
  public :: context_factory

  type, abstract :: context_factory
  contains
    procedure(create_context_i), deferred :: create_context
  end type context_factory

  abstract interface

    subroutine create_context_i(this, testsuite, ctx)
      import :: context_factory, generic_test, generic_context, generic_suite
      implicit none
      class(context_factory), intent(in) :: this
      class(generic_suite), pointer, intent(in) :: testsuite
      class(generic_context), allocatable, intent(out) :: ctx
    end subroutine create_context_i

  end interface

end module fortuno_contextfactory
