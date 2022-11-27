module testmod_simple
  use mylib, only : factorial
  use fortuno_serial, only : test_suite, test, check, check_failed, skip
  implicit none

contains


  function simple_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("simple", [&
        & test("factorial_0", test_factorial_0),&
        & test("factorial_1", test_factorial_1),&
        & test("factorial_2_3", test_factorial_2_3),&
        & test("factorial_4_5", test_factorial_4_5)&
        & ])

  end function simple_suite

  ! Test: 0! = 1
  subroutine test_factorial_0()
    call check(factorial(0) == 1)
  end subroutine test_factorial_0

  ! Test: 1! = 1
  subroutine test_factorial_1()
    call check(factorial(1) == 1)
  end subroutine test_factorial_1

  ! When: skip() is called as first context control command
  ! Then: subsequent failing and succesful checks do not change skipped status
  subroutine test_factorial_2_3()
    call skip()
    ! Probably you should return from the test at this point.
    ! No details about any checks beyond this point will be recorded, test will remain 'skipped'
    call check(factorial(2) == 1)
    call check(factorial(3) == 6)
  end subroutine test_factorial_2_3

  ! When: check_failed() is called after successful check
  ! Then: it returns true
  subroutine test_factorial_4_5()
    call check(factorial(4) == 24)
    if (check_failed()) return  ! Abort this test if last check failed
    call check(factorial(5) == 120)
  end subroutine test_factorial_4_5

end module testmod_simple
