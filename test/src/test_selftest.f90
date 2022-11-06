module testmod_selftest
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, test => serial_test
  implicit none

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    testsuite = suite("simple", [&
        & test("factorial(0)", test_factorial0),&
        & test("factorial(1)", test_factorial1),&
        & test("factorial(2)", test_factorial2),&
        & test("factorial_fail", test_factorialfail)&
        & ])

  end function test_suite


  subroutine test_factorial0(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_factorial0


  subroutine test_factorial1(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(1) == 1)

  end subroutine test_factorial1


  subroutine test_factorial2(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(2) == 2)

  end subroutine test_factorial2


  subroutine test_factorialfail(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(.false.)

  end subroutine test_factorialfail

end module testmod_selftest


module testmod_selftest_tester
  use fortuno, only : driver_result, is_equal, serial_driver, test => serial_test,&
      & context => serial_context, test_name, suite => serial_suite
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
    call ctx%check(is_equal(size(drvres%caseresults), 4), file="test_simple.f90", line=102, &
        & msg="This has intentionally so much info.")

  end subroutine test_nr_of_entries


  subroutine test_results(ctx)
    class(context), intent(inout) :: ctx

    !call ctx%check(all(drvres%caseresults(:)%success .eqv. [.true., .true., .true., .false.]))
    call ctx%check(all(drvres%caseresults(:)%success .eqv. [.true., .true.]))

  end subroutine test_results


  subroutine set_up_module()
    type(serial_driver), allocatable :: driver

    driver = serial_driver([test_suite_selftest()])

    call driver%run(driverresult=drvres,&
            & testnames=[test_name("simple", "factorial(0)"), test_name("simple", "factorial(1)")])

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
