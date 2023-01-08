module fortuno_generictest
  use fortuno_utils, only : keyword_arg_enforcer_
  implicit none

  private
  public :: generic_test, generic_test_cls


  type, abstract :: generic_test
    character(:), allocatable :: name
    type(keyword_arg_enforcer_), private :: kwargsonly_ = keyword_arg_enforcer_()
  contains
    procedure :: get_char_repr
  end type generic_test


  type :: generic_test_cls
    class(generic_test), allocatable :: instance
  end type generic_test_cls

contains


  subroutine get_char_repr(this, repr)
    class(generic_test), intent(in) :: this
    character(:), allocatable, intent(out) :: repr
  end subroutine get_char_repr

end module fortuno_generictest
