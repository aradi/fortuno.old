module testsuite_simple
  use mylib, only : factorial
  use fortuno, only : context => serial_context, test => serial_test, test_suite
  implicit none

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("simple", [&
        & test("factorial(0)", test_factorial0),&
        & test("factorial(1)", test_factorial1),&
        & test("factorial(2,3)", test_factorial23),&
        & test("factorial(4,5)", test_factorial45)&
        & ])

  end function new_test_suite


  subroutine test_factorial0(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_factorial0


  subroutine test_factorial1(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(1) == 1)

  end subroutine test_factorial1


  subroutine test_factorial23(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(2) == 2)
    ! Note, if first check failed, further tests would be executed but not recorded
    call ctx%check(factorial(3) == 6)

  end subroutine test_factorial23


  subroutine test_factorial45(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(4) == 24)
    ! Here we skip further tests if the first failed
    if (ctx%failed()) return
    call ctx%check(factorial(5) == 120)

  end subroutine test_factorial45


end module testsuite_simple


program testdriver_simple
  use fortuno, only : serial_driver
  use testsuite_simple, only : new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([new_test_suite()])
  call driver%run()

end program testdriver_simple
