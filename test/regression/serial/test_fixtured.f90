module testmod_fixtured
  use mylib, only : factorial
  use fortuno_serial, only : check, test_suite, fixtured_test
  implicit none


  type, extends(fixtured_test) :: random_test
    integer :: nn = -1
  contains
    procedure:: set_up
    procedure :: get_char_repr
  end type

contains


  function fixtured_suite() result(suite)
    type(test_suite) :: suite

    integer :: ii

    call random_seed()

    suite = test_suite("fixtured", [&
        & [(random_test("recursion_down", test_recursion_down), ii = 1, 10)],&
        & [(random_test("recursion_up", test_recursion_up), ii = 1, 10)]&
        & ])

  end function fixtured_suite


  subroutine test_recursion_down(this)
    class(random_test), intent(in) :: this

    call check(factorial(this%nn) == this%nn * factorial(this%nn - 1))

  end subroutine test_recursion_down


  subroutine test_recursion_up(this)
    class(random_test), intent(in) :: this

    call check(factorial(this%nn + 1) == (this%nn + 1) * factorial(this%nn))

  end subroutine test_recursion_up


  subroutine set_up(this)
    class(random_test), intent(inout) :: this

    real :: rand

    call random_number(rand)
    this%nn = int(20.0 * rand) + 1

  end subroutine set_up


  subroutine get_char_repr(this, repr)
    class(random_test), intent(in) :: this
    character(:), allocatable, intent(out) :: repr

    character(5) :: buffer

    write(buffer, "(a, i2.2)") "n=", this%nn
    repr = trim(buffer)

  end subroutine get_char_repr

end module testmod_fixtured
