module testmod_multiple_1
  use mylib, only : factorial
  use fortuno_serial, only : check, test_suite, test
  implicit none

contains


  function multiple_1_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("multiple_1", [&
        & test("factorial_0", test_factorial_0)&
        & ])

  end function multiple_1_suite


  subroutine test_factorial_0()
    call check(factorial(0) == 1)
  end subroutine test_factorial_0

end module testmod_multiple_1


module testmod_multiple_2
  use mylib, only : factorial
  use fortuno_serial, only : check, test, test_suite
  implicit none

contains


  function multiple_2_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("multiple_2", [&
        & test("factorial_0_failing", test_factorial_0_failing),&
        & test("factorial_1to5_failing", test_factorial_1to5_failing)&
        & ])

  end function multiple_2_suite


  subroutine test_factorial_0_failing()
    call check(factorial(0) == 0, msg="Failing on purpose (single check only)")
  end subroutine test_factorial_0_failing


  subroutine test_factorial_1to5_failing()
    call check(factorial(1) == 0, msg="Failing on purpose (1st failing check out of 3)")
    call check(factorial(2) == 1, msg="Failing on purpose (2nd failing check out of 3)")
    call check(factorial(3) == 6, msg="You should never see this, as this test should pass")
    call check(factorial(4) == 7, msg="Failing on purpose (3rd failing check out of 3)")
    call check(factorial(5) == 120, msg="You should never see this, as this test should pass")
  end subroutine test_factorial_1to5_failing

end module testmod_multiple_2


program testapp_multiple
  use fortuno_serial, only : test_app
  use testmod_multiple_1, only : multiple_1_suite
  use testmod_multiple_2, only : multiple_2_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app([multiple_1_suite(), multiple_2_suite()])
  call app%run()

end program testapp_multiple
