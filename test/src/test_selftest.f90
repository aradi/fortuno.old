module testmod_selftest
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, test => serial_test
  implicit none

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    testsuite = suite("simple", [&
        & test("factorial_0", test_factorial_0),&
        & test("factorial_1", test_factorial_1),&
        & test("factorial_2", test_factorial_2),&
        & test("factorial_fail", test_factorial_fail)&
        & ])

  end function test_suite


  subroutine test_factorial_0(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_factorial_0


  subroutine test_factorial_1(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(1) == 1)

  end subroutine test_factorial_1


  subroutine test_factorial_2(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(2) == 2)

  end subroutine test_factorial_2


  subroutine test_factorial_fail(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(.false.)

  end subroutine test_factorial_fail

end module testmod_selftest


module testmod_selftest_tester
  use fortuno, only : driver_result, is_equal, serial_driver, test => serial_test,&
      & context => serial_context, test_name, teststatus, suite => serial_suite
  use testmod_selftest, only : test_suite_selftest => test_suite
  implicit none

  type(driver_result), allocatable :: drvres

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    call set_up_module()
    testsuite = suite("serial_tester", [&
        & test("nr_of_entries", test_nr_of_entries),&
        & test("results", test_results)&
        & ])

  end function test_suite


  subroutine test_nr_of_entries(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(is_equal(size(drvres%suiteresults), 1))
    call ctx%check(is_equal(size(drvres%testresults), 4), file="test_simple.f90", line=102, &
        & msg="This has intentionally so much info.")

  end subroutine test_nr_of_entries


  subroutine test_results(ctx)
    class(context), intent(inout) :: ctx

    ! Note: only 2 of the 4 tests are selected in set_up_module()
    call ctx%check(all(drvres%testresults(:)%status == [teststatus%ok, teststatus%ok]))

  end subroutine test_results


  subroutine set_up_module()
    type(serial_driver), allocatable :: driver

    driver = serial_driver([test_suite_selftest()])

    call driver%run(driverresult=drvres,&
            & testnames=[test_name("simple", "factorial_0"), test_name("simple", "factorial_1")])

  end subroutine set_up_module

end module testmod_selftest_tester


program testapp_selftest_tester
  use fortuno, only : serial_app
  use testmod_selftest_tester, only : test_suite
  implicit none

  type(serial_app), allocatable :: app

  app = serial_app([test_suite()])
  call app%run()

end program testapp_selftest_tester
