module test_simple
  use mylib, only : factorial
  use fortuno, only : test_suite, test_case, test_context
  implicit none

  private
  public :: new_test_suite

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("simple", [&
        & test_case("factorial(0)", test_factorial0),&
        & test_case("factorial(1)", test_factorial1),&
        & test_case("factorial(2,3)", test_factorial23),&
        & test_case("factorial(4,5)", test_factorial45)&
        & ])

  end function new_test_suite


  subroutine test_factorial0(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_factorial0


  subroutine test_factorial1(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(1) == 1)

  end subroutine test_factorial1


  subroutine test_factorial23(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(2) == 2)
    ! Note, if first check failed, further tests would be executed but not recorded
    call ctx%check(factorial(3) == 6)

  end subroutine test_factorial23


  subroutine test_factorial45(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(4) == 24)
    ! Here we skip further tests if the first failed
    if (ctx%failed()) return
    call ctx%check(factorial(5) == 120)

  end subroutine test_factorial45


end module test_simple


program test_simple_driver
  use fortuno, only : serial_driver
  use test_simple, only : new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([new_test_suite()])
  call driver%run()

end program test_simple_driver
