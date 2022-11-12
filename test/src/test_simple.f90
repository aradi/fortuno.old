module testmod_simple
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, test => serial_test
  implicit none

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    testsuite = suite("simple", [&
        & test("factorial_0", test_factorial_0),&
        & test("factorial_1", test_factorial_1),&
        & test("factorial_2_3", test_factorial_2_3),&
        & test("factorial_4_5", test_factorial_4_5)&
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


  subroutine test_factorial_2_3(ctx)
    class(context), intent(inout) :: ctx

    ! Skip test
    call ctx%skip()
    ! Probably you should return from the test at this point. No details about any checks beyond
    ! this point will be recorded.
    call ctx%check(factorial(2) == 1) ! Despite failing check, test will remain 'skipped'
    call ctx%check(factorial(3) == 6)

  end subroutine test_factorial_2_3


  subroutine test_factorial_4_5(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(4) == 24)
    if (ctx%check_failed()) return  ! Abort this test if last check failed
    call ctx%check(factorial(5) == 120)

  end subroutine test_factorial_4_5

end module testmod_simple


program testapp_simple
  use fortuno, only : serial_app
  use testmod_simple, only : test_suite
  implicit none

  type(serial_app), allocatable :: app

  app = serial_app([test_suite()])
  call app%run()

end program testapp_simple
