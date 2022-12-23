module fortuno_testbase
  implicit none

  private
  public :: test_base, test_base_cls


  type, abstract :: test_base
    character(:), allocatable :: name
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
