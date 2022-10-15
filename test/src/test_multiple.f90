module testsuite_multiple1
  use mylib, only : factorial
  use fortuno, only : context => serial_context, test => simple_test, test_suite
  implicit none

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("multiple1", [&
        & test("factorial(0)", test_0)&
        & ])

  end function new_test_suite


  subroutine test_0(ctx)
    class(context), pointer, intent(in) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_0

end module testsuite_multiple1


module testsuite_multiple2
  use mylib, only : factorial
  use fortuno, only : context => serial_context, test => simple_test, test_suite
  implicit none

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("multiple2", [&
        & test("factorial(0)", test_0)&
        & ])

  end function new_test_suite


  subroutine test_0(ctx)
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(0) == 0, msg="Failing on purpose")

  end subroutine test_0


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
