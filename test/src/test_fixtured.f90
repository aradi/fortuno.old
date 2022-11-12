module testmod_fixtured
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, serial_test_base
  implicit none


  type, extends(serial_test_base) :: random_test
    procedure(test_recursion_down), nopass, pointer :: testroutine
    integer :: nn = -1
  contains
    procedure:: set_up
    procedure :: run
    procedure :: get_char_repr
  end type

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    integer :: ii

    call random_seed()

    testsuite = suite("fixtured", [&
        & [(random_test("recursion_down", test_recursion_down), ii = 1, 10)],&
        & [(random_test("recursion_up", test_recursion_up), ii = 1, 10)]&
        & ])

  end function test_suite


  subroutine test_recursion_down(ctx, mytest)
    class(context), intent(inout) :: ctx
    class(random_test), intent(in) :: mytest

    call ctx%check(factorial(mytest%nn) == mytest%nn * factorial(mytest%nn - 1))

  end subroutine test_recursion_down


  subroutine test_recursion_up(ctx, mytest)
    class(context), intent(inout) :: ctx
    class(random_test), intent(in) :: mytest

    call ctx%check(factorial(mytest%nn + 1) == (mytest%nn + 1) * factorial(mytest%nn))

  end subroutine test_recursion_up


  subroutine set_up(this)
    class(random_test), intent(inout) :: this

    real :: rand

    call random_number(rand)
    this%nn = int(20.0 * rand) + 1

  end subroutine set_up


  subroutine run(this, ctx)
    class(random_test), intent(inout) :: this
    class(context), intent(inout) :: ctx

    call this%set_up()
    call this%testroutine(ctx, this)

  end subroutine run


  subroutine get_char_repr(this, repr)
    class(random_test), intent(in) :: this
    character(:), allocatable, intent(out) :: repr

    character(5) :: buffer

    write(buffer, "(a, i2.2)") "n=", this%nn
    repr = trim(buffer)

  end subroutine get_char_repr

end module testmod_fixtured


program testapp_fixtured
  use fortuno, only : serial_app
  use testmod_fixtured, only : test_suite
  implicit none

  type(serial_app), allocatable :: app

  app = serial_app([test_suite()])
  call app%run()

end program testapp_fixtured
