module testsuite_selftest
  use mylib, only : factorial
  use fortuno, only : context => serial_context, test => simple_test, test_suite
  implicit none

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("simple", [&
        & test("factorial(0)", test_factorial0),&
        & test("factorial(1)", test_factorial1),&
        & test("factorial(2)", test_factorial2),&
        & test("factorial_fail", test_factorialfail)&
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


  subroutine test_factorial2(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(2) == 2)

  end subroutine test_factorial2


  subroutine test_factorialfail(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(.false.)

  end subroutine test_factorialfail


end module testsuite_selftest


module testsuite_selftest_tester
  use fortuno, only : driver_result, is_equal, serial_driver, test => simple_test,&
      & context => test_context, test_name, test_suite
  use testsuite_selftest, only : new_selftest_suite => new_test_suite
  implicit none

  type(driver_result), allocatable :: drvres

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    call set_up_module()
    testsuite = test_suite("simple_tester", [&
        & test("nr_of_entries", test_nr_of_entries),&
        & test("results", test_results)&
        & ])

  end function new_test_suite


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

    driver = serial_driver([new_selftest_suite()])

    call driver%run(driverresult=drvres,&
            & testnames=[test_name("simple", "factorial(0)"), test_name("simple", "factorial(1)")])

  end subroutine set_up_module

end module testsuite_selftest_tester


program testdriver_selftest_tester
  use fortuno, only : argument_parser, serial_driver
  use testsuite_selftest_tester, only : new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver
  type(argument_parser), allocatable :: argparser

  driver = serial_driver([new_test_suite()])
  argparser = argument_parser()
  call driver%run(testnames=argparser%get_test_names())

end program testdriver_selftest_tester
