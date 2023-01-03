module fortuno_testbase
  use fortuno_utils, only : keyword_arg_enforcer_
  implicit none

  private
  public :: test_base, test_base_cls


  type, abstract :: test_base
    character(:), allocatable :: name
    type(keyword_arg_enforcer_), private :: kwargsonly_ = keyword_arg_enforcer_()
  contains
    procedure :: get_char_repr
  end type test_base


  type :: test_base_cls
    class(test_base), allocatable :: instance
  end type test_base_cls

contains


  subroutine get_char_repr(this, repr)
    class(test_base), intent(in) :: this
    character(:), allocatable, intent(out) :: repr
  end subroutine get_char_repr

end module fortuno_testbase
