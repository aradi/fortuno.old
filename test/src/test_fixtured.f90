module testsuite_fixtured
  use mylib, only : factorial
  use fortuno, only : serial_test_base, serial_context, suite_base
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


  function new_suite_base() result(testsuite)
    type(suite_base) :: testsuite

    integer :: ii

    call random_seed()

    testsuite = suite_base("fixtured", [&
        & [(random_test("recursion_down", test_recursion_down), ii = 1, 10)],&
        & [(random_test("recursion_up", test_recursion_down), ii = 1, 10)]&
        & ])

  end function new_suite_base


  subroutine test_recursion_down(ctx, mycase)
    class(serial_context), intent(inout) :: ctx
    class(random_test), intent(in) :: mycase

    call ctx%check(factorial(mycase%nn) == mycase%nn * factorial(mycase%nn - 1))

  end subroutine test_recursion_down


  subroutine test_recursion_up(ctx, mycase)
    class(serial_context), intent(inout) :: ctx
    class(random_test), intent(in) :: mycase

    call ctx%check(factorial(mycase%nn + 1) == (mycase%nn + 1) * factorial(mycase%nn))

  end subroutine test_recursion_up


  subroutine set_up(this)
    class(random_test), intent(inout) :: this

    real :: rand

    call random_number(rand)
    this%nn = int(20.0 * rand) + 1

  end subroutine set_up


  subroutine run(this, ctx)
    class(random_test), intent(inout) :: this
    class(serial_context), intent(inout) :: ctx

    call this%set_up()
    call this%testroutine(ctx, this)

  end subroutine run


  subroutine get_char_repr(this, state)
    class(random_test), intent(in) :: this
    character(:), allocatable, intent(out) :: state

    character(5) :: buffer

    write(buffer, "(a, i2.2)") "n=", this%nn
    state = trim(buffer)

  end subroutine get_char_repr


end module testsuite_fixtured


program testdriver_fixtured
  use fortuno, only : serial_driver
  use testsuite_fixtured, only : new_suite_base
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([new_suite_base()])
  call driver%run()

end program testdriver_fixtured
