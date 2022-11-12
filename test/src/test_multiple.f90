module testmod_multiple_1
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, test => serial_test
  implicit none

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    testsuite = suite("multiple_1", [&
        & test("factorial_0", test_factorial_0)&
        & ])

  end function test_suite


  subroutine test_factorial_0(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_factorial_0

end module testmod_multiple_1


module testmod_multiple_2
  use mylib, only : factorial
  use fortuno, only : context => serial_context, test => serial_test, suite => serial_suite
  implicit none

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    testsuite = suite("multiple_2", [&
        & test("factorial_0_failing", test_factorial_0_failing),&
        & test("factorial_1to5_failing", test_factorial_1to5_failing)&
        & ])

  end function test_suite


  subroutine test_factorial_0_failing(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(0) == 0, msg="Failing on purpose (single check only)")

  end subroutine test_factorial_0_failing


  subroutine test_factorial_1to5_failing(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(1) == 0, msg="Failing on purpose (1st failing check out of 3)")
    call ctx%check(factorial(2) == 1, msg="Failing on purpose (2nd failing check out of 3)")
    call ctx%check(factorial(3) == 6, msg="You should never see this, as this test should pass")
    call ctx%check(factorial(4) == 7, msg="Failing on purpose (3rd failing check out of 3)")
    call ctx%check(factorial(5) == 120, msg="You should never see this, as this test should pass")

  end subroutine test_factorial_1to5_failing

end module testmod_multiple_2


program testapp_multiple
  use fortuno, only : serial_app
  use testmod_multiple_1, only : test_suite_1 => test_suite
  use testmod_multiple_2, only : test_suite_2 => test_suite
  implicit none

  type(serial_app), allocatable :: app

  app = serial_app([test_suite_1(), test_suite_2()])
  call app%run()

end program testapp_multiple
