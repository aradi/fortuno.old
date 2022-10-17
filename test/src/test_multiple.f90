module testsuite_multiple1
  use mylib, only : factorial
  use fortuno, only : context => serial_context, test => serial_test, test_suite
  implicit none

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("multiple1", [&
        & test("factorial(0)", test_factorial0)&
        & ])

  end function new_test_suite


  subroutine test_factorial0(ctx)
    class(context), pointer, intent(in) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_factorial0

end module testsuite_multiple1


module testsuite_multiple2
  use mylib, only : factorial
  use fortuno, only : context => serial_context, test => serial_test, test_suite
  implicit none

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("multiple2", [&
        & test("factorial(0)", test_factorial0_failing),&
        & test("factorial(1-5)", test_factorial_1to5_failing)&
        & ])

  end function new_test_suite


  subroutine test_factorial0_failing(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(0) == 0, msg="Failing on purpose (single check only)")

  end subroutine test_factorial0_failing


  subroutine test_factorial_1to5_failing(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(1) == 0, msg="Failing on purpose (1st failing check out of 3)")
    call ctx%check(factorial(2) == 1, msg="Failing on purpose (2nd failing check out of 3)")
    call ctx%check(factorial(3) == 6, msg="You should never see this, as this test should pass")
    call ctx%check(factorial(4) == 7, msg="Failing on purpose (3rd failing check out of 3)")
    call ctx%check(factorial(5) == 120, msg="You should never see this, as this test should pass")

  end subroutine test_factorial_1to5_failing



end module testsuite_multiple2


program test_driver
  use fortuno, only : serial_driver
  use testsuite_multiple1, only : new_multiple1_suite => new_test_suite
  use testsuite_multiple2, only : new_multiple2_suite => new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([&
      & new_multiple1_suite(), new_multiple2_suite()&
      & ])
  call driver%run()

end program test_driver
