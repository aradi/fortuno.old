module testmod_fixturedsuite
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, serial_suite_base,&
      & serial_test_base
  implicit none


  type, extends(serial_suite_base) :: random_suite
    integer :: nn = -1
  contains
    procedure :: set_up => random_suite_set_up
  end type random_suite


  type, extends(serial_test_base) :: random_test
    procedure(test_recursion_down), nopass, pointer :: testroutine
  contains
    procedure :: run => random_test_run
  end type random_test

contains


  function test_suite() result(testsuite)
    type(random_suite) :: testsuite

    call testsuite%set_name("fixtured")
    call testsuite%add_test([&
        & random_test("recursion_down", test_recursion_down),&
        & random_test("recursion_up", test_recursion_down)&
        & ])

  end function test_suite


  subroutine random_suite_set_up(this, ctx)
    class(random_suite), intent(inout) :: this
    class(context), intent(inout) :: ctx

    real :: rand

    call random_seed()
    call random_number(rand)
    this%nn = int(20.0 * rand) + 1

  end subroutine random_suite_set_up


  subroutine random_test_run(this, ctx)
    class(random_test), intent(inout) :: this
    class(context), intent(inout) :: ctx

    type(random_suite), pointer :: mysuite => null()

    select type (testsuite => ctx%testsuite)
    type is (random_suite)
      mysuite => testsuite
    class default
      error stop "Expected type random_suite, obtained something else"
    end select
    call this%testroutine(ctx, mysuite)

  end subroutine random_test_run


  subroutine test_recursion_down(ctx, mysuite)
    class(context), intent(inout) :: ctx
    class(random_suite), intent(in) :: mysuite

    call ctx%check(factorial(mysuite%nn) == mysuite%nn * factorial(mysuite%nn - 1))

  end subroutine test_recursion_down


  subroutine test_recursion_up(ctx, mysuite)
    class(context), intent(inout) :: ctx
    class(random_suite), intent(in) :: mysuite

    call ctx%check(factorial(mysuite%nn + 1) == (mysuite%nn + 1) * factorial(mysuite%nn))

  end subroutine test_recursion_up

end module testmod_fixturedsuite


program testapp_fixtured
  use fortuno, only : serial_app
  use testmod_fixturedsuite, only : test_suite
  implicit none

  type(serial_app), allocatable :: app

  app = serial_app([test_suite()])
  call app%run()

end program testapp_fixtured
