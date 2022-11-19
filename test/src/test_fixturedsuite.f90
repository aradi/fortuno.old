module testmod_fixturedsuite_random
  use mylib, only : factorial
  use fortuno_serial, only : check, fixtured_test, suite_base, test_suite, suite_ptr
  implicit none


  type, extends(test_suite) :: random_suite
    integer :: nn = -1
  contains
    procedure :: set_up => random_suite_set_up
    procedure :: get_char_repr => random_suite_get_char_repr
  end type random_suite


  type, extends(fixtured_test) :: random_test
    type(random_suite), pointer :: suite => null()
  contains
    procedure :: set_up => random_test_set_up
  end type random_test

contains


  function fixtured_random_suite() result(suite)
    type(random_suite) :: suite

    call suite%set_name("fixtured_random")
    call suite%add_test([&
        & random_test("recursion_down", test_recursion_down),&
        & random_test("recursion_up", test_recursion_down)&
        & ])

  end function fixtured_random_suite


  subroutine random_suite_set_up(this)
    class(random_suite), intent(inout) :: this

    real :: rand

    call random_seed()
    call random_number(rand)
    this%nn = int(20.0 * rand) + 1

  end subroutine random_suite_set_up


  subroutine random_suite_get_char_repr(this, repr)
    class(random_suite), intent(in) :: this
    character(:), allocatable, intent(out) :: repr

    character(2) :: reprstr

    write(reprstr, "(i0)") this%nn
    repr = trim(reprstr)

  end subroutine random_suite_get_char_repr


  subroutine random_test_set_up(this)
    class(random_test), intent(inout) :: this

    class(suite_base), pointer :: genericsuite

    genericsuite => suite_ptr()
    select type (genericsuite)
    type is (random_suite)
      this%suite => genericsuite
    class default
      error stop "Expected random_suite, obtained some other type"
    end select

  end subroutine random_test_set_up


  subroutine test_recursion_down(this)
    class(random_test), intent(in) :: this

    call check(factorial(this%suite%nn + 1) == this%suite%nn * factorial(this%suite%nn - 1))

  end subroutine test_recursion_down


  subroutine test_recursion_up(this)
    class(random_test), intent(in) :: this

    call check(factorial(this%suite%nn + 1) == (this%suite%nn + 1) * factorial(this%suite%nn))

  end subroutine test_recursion_up

end module testmod_fixturedsuite_random


module testmod_fixturedsuite_failing
  use mylib, only : factorial
  use fortuno_serial, only : check, test_suite, test
  implicit none


  type, extends(test_suite) :: failing_suite
    integer :: myval = -1
  contains
    procedure :: set_up
  end type failing_suite

contains


  function fixtured_failing_suite() result(suite)
    type(failing_suite) :: suite

    call suite%set_name("fixtured_failing")
    call suite%add_test([&
        & test("factorial_1", test_factorial_1)&
        & ])

  end function fixtured_failing_suite


  subroutine set_up(this)
    class(failing_suite), intent(inout) :: this

    call check(this%myval == 42, msg="Failing on purpose")  ! this will fail

  end subroutine set_up


  subroutine test_factorial_1()

    ! This will be never checked, as suite setup failed
    call check(factorial(1) == 1)

  end subroutine test_factorial_1

end module testmod_fixturedsuite_failing


module testmod_fixturedsuite_skipped
  use mylib, only : factorial
  use fortuno_serial, only : check, test_suite, test, skip
  implicit none


  type, extends(test_suite) :: skipped_suite
    integer :: myval = -1
  contains
    procedure :: set_up
  end type skipped_suite

contains


  function fixtured_skipped_suite() result(suite)
    type(skipped_suite) :: suite

    call suite%set_name("fixtured_skipped")
    call suite%add_test([&
        & test("factorial_1", test_factorial_1)&
        & ])

  end function fixtured_skipped_suite


  subroutine set_up(this)
    class(skipped_suite), intent(inout) :: this

    call skip()

  end subroutine set_up


  subroutine test_factorial_1()

    ! Will be never executed, as suite set_up had been skipped
    call check(factorial(1) == 1)

  end subroutine test_factorial_1

end module testmod_fixturedsuite_skipped


program testapp_fixturedsuite
  use fortuno_serial, only : test_app
  use testmod_fixturedsuite_random, only : fixtured_random_suite
  use testmod_fixturedsuite_failing, only : fixtured_failing_suite
  use testmod_fixturedsuite_skipped, only : fixtured_skipped_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app()
  call app%add_suite(fixtured_random_suite())
  call app%add_suite(fixtured_failing_suite())
  call app%add_suite(fixtured_skipped_suite())
  call app%run()

end program testapp_fixturedsuite
