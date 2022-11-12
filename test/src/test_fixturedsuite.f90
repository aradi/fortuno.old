module testmod_fixturedsuite_random
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, serial_suite_base,&
      & serial_test_base
  implicit none


  type, extends(serial_suite_base) :: random_suite
    integer :: nn = -1
  contains
    procedure :: set_up
    procedure :: get_char_repr
  end type random_suite


  type, extends(serial_test_base) :: random_test
    procedure(test_recursion_down), nopass, pointer :: testroutine
  contains
    procedure :: run
  end type random_test

contains


  function test_suite() result(testsuite)
    type(random_suite) :: testsuite

    call testsuite%set_name("fixtured_random")
    call testsuite%add_test([&
        & random_test("recursion_down", test_recursion_down),&
        & random_test("recursion_up", test_recursion_down)&
        & ])

  end function test_suite


  subroutine set_up(this, ctx)
    class(random_suite), intent(inout) :: this
    class(context), intent(inout) :: ctx

    real :: rand

    call random_seed()
    call random_number(rand)
    this%nn = int(20.0 * rand) + 1

  end subroutine set_up


  subroutine get_char_repr(this, repr)
    class(random_suite), intent(in) :: this
    character(:), allocatable, intent(out) :: repr

    character(2) :: reprstr

    write(reprstr, "(i0)") this%nn
    repr = trim(reprstr)

  end subroutine get_char_repr


  subroutine run(this, ctx)
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

  end subroutine run


  subroutine test_recursion_down(ctx, mysuite)
    class(context), intent(inout) :: ctx
    class(random_suite), intent(in) :: mysuite

    call ctx%check(factorial(mysuite%nn + 1) == mysuite%nn * factorial(mysuite%nn - 1))

  end subroutine test_recursion_down


  subroutine test_recursion_up(ctx, mysuite)
    class(context), intent(inout) :: ctx
    class(random_suite), intent(in) :: mysuite

    call ctx%check(factorial(mysuite%nn + 1) == (mysuite%nn + 1) * factorial(mysuite%nn))

  end subroutine test_recursion_up

end module testmod_fixturedsuite_random


module testmod_fixturedsuite_failing
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, serial_suite_base,&
      & test => serial_test
  implicit none


  type, extends(serial_suite_base) :: failing_suite
    integer :: myval = -1
  contains
    procedure :: set_up => failing_suite_set_up
  end type failing_suite

contains


  function test_suite() result(testsuite)
    type(failing_suite) :: testsuite

    call testsuite%set_name("fixtured_failing")
    call testsuite%add_test([&
        & test("factorial_1", test_factorial1)&
        & ])

  end function test_suite


  subroutine failing_suite_set_up(this, ctx)
    class(failing_suite), intent(inout) :: this
    class(context), intent(inout) :: ctx

    call ctx%check(this%myval == 42, msg="Failing on purpose")  ! this will fail

  end subroutine failing_suite_set_up


  subroutine test_factorial1(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(1) == 1)

  end subroutine test_factorial1

end module testmod_fixturedsuite_failing


module testmod_fixturedsuite_skipped
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, serial_suite_base,&
      & test => serial_test
  implicit none


  type, extends(serial_suite_base) :: skipped_suite
    integer :: myval = -1
  contains
    procedure :: set_up => skipped_suite_set_up
  end type skipped_suite

contains


  function test_suite() result(testsuite)
    type(skipped_suite) :: testsuite

    call testsuite%set_name("fixtured_skipped")
    call testsuite%add_test([&
        & test("factorial_1", test_factorial1)&
        & ])

  end function test_suite


  subroutine skipped_suite_set_up(this, ctx)
    class(skipped_suite), intent(inout) :: this
    class(context), intent(inout) :: ctx

    call ctx%skip()

  end subroutine skipped_suite_set_up


  subroutine test_factorial1(ctx)
    class(context), intent(inout) :: ctx

    ! Will be never executed, as suite set_up had been skipped explicitely.
    call ctx%check(factorial(1) == 1)

  end subroutine test_factorial1

end module testmod_fixturedsuite_skipped


program testapp_fixturedsuite
  use fortuno, only : serial_app
  use testmod_fixturedsuite_random, only : test_suite1 => test_suite
  use testmod_fixturedsuite_failing, only : test_suite2 => test_suite
  use testmod_fixturedsuite_skipped, only : test_suite3 => test_suite
  implicit none

  type(serial_app), allocatable :: app

  app = serial_app()
  call app%add_suite(test_suite1())
  call app%add_suite(test_suite2())
  call app%add_suite(test_suite3())
  call app%run()

end program testapp_fixturedsuite
