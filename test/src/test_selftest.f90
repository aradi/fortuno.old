module testmod_selftest
  use mylib, only : factorial
  use fortuno_serial, only : check, test, test_suite
  implicit none

contains


  function selftest_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("selftest", [&
        & test("factorial_0", test_factorial_0),&
        & test("factorial_1", test_factorial_1),&
        & test("factorial_2", test_factorial_2),&
        & test("factorial_fail", test_factorial_fail)&
        & ])

  end function selftest_suite


  subroutine test_factorial_0()
    call check(factorial(0) == 1)
  end subroutine test_factorial_0


  subroutine test_factorial_1()
    call check(factorial(1) == 1)
  end subroutine test_factorial_1


  subroutine test_factorial_2()
    call check(factorial(2) == 2)
  end subroutine test_factorial_2


  subroutine test_factorial_fail()
    call check(.false.)
  end subroutine test_factorial_fail

end module testmod_selftest


module testmod_selftest_tester
  use fortuno_serial, only : check, driver_result, is_equal, test_driver, test, test_name,&
      & teststatus, test_suite
  use testmod_selftest, only : selftest_suite
  implicit none

  type(driver_result), allocatable :: drvres

contains


  function serial_tester_suite() result(suite)
    type(test_suite) :: suite

    call set_up_module()
    suite = test_suite("serial_tester", [&
        & test("nr_of_entries", test_nr_of_entries),&
        & test("results", test_results)&
        & ])

  end function serial_tester_suite


  subroutine test_nr_of_entries()

    call check(is_equal(size(drvres%suiteresults), 1))
    call check(is_equal(size(drvres%testresults), 4), file="test_simple.f90", line=102, &
        & msg="This has intentionally so much info.")

  end subroutine test_nr_of_entries


  subroutine test_results()

    ! Note: only 2 of the 4 tests are selected in set_up_module()
    call check(all(drvres%testresults(:)%status == [teststatus%ok, teststatus%ok]))

  end subroutine test_results


  subroutine set_up_module()
    type(test_driver), allocatable :: driver

    driver = test_driver([selftest_suite()])
    call driver%run(driverresult=drvres,&
        & testnames=[test_name("selftest", "factorial_0"), test_name("selftest", "factorial_1")])

  end subroutine set_up_module

end module testmod_selftest_tester


program testapp_selftest_tester
  use fortuno_serial, only : test_app
  use testmod_selftest_tester, only : serial_tester_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app([serial_tester_suite()])
  call app%run()

end program testapp_selftest_tester
