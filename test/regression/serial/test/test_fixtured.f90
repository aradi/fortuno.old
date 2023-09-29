module testmod_fixtured
  use mylib, only : factorial
  use fortuno_serial, only : check, test_base, test_suite, test_suite_base_cls
  implicit none


  type, extends(test_base) :: random_test
    procedure(test_recursion_down), pointer :: proc
    integer :: nn = -1
  contains
    procedure :: run
    procedure :: get_char_repr
  end type

contains


  function new_suite() result(suite)
    type(test_suite_base_cls) :: suite

    integer :: ii

    call random_seed()

    suite%instance =&
        & test_suite("fixtured", [&
        & [(random_test("recursion_down", proc=test_recursion_down), ii = 1, 10)],&
        & [(random_test("recursion_up", proc=test_recursion_up), ii = 1, 10)]&
        & ])

  end function new_suite


  subroutine test_recursion_down(this)
    class(random_test), intent(in) :: this

    call check(factorial(this%nn) == this%nn * factorial(this%nn - 1))

  end subroutine test_recursion_down


  subroutine test_recursion_up(this)
    class(random_test), intent(in) :: this

    call check(factorial(this%nn + 1) == (this%nn + 1) * factorial(this%nn))

  end subroutine test_recursion_up


  subroutine run(this)
    class(random_test), intent(inout) :: this

    real :: rand

    call random_number(rand)
    this%nn = int(20.0 * rand) + 1
    call this%proc()

  end subroutine run


  subroutine get_char_repr(this, repr)
    class(random_test), intent(in) :: this
    character(:), allocatable, intent(out) :: repr

    character(5) :: buffer

    write(buffer, "(a, i2.2)") "n=", this%nn
    repr = trim(buffer)

  end subroutine get_char_repr

end module testmod_fixtured
