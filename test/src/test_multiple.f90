module multiple1
  use mylib, only : factorial
  use fortuno, only : test_suite, test_case, test_context
  implicit none

  private
  public :: new_test_suite

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("multiple1", [&
        & test_case("factorial(0)", test_0)&
        & ])

  end function new_test_suite


  subroutine test_0(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_0

end module multiple1


module multiple2
  use mylib, only : factorial
  use fortuno, only : test_suite, test_case, test_context
  implicit none

  private
  public :: new_test_suite

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("multiple2", [&
        & test_case("factorial(0)", test_0)&
        & ])

  end function new_test_suite


  subroutine test_0(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(0) == 0, msg="Failing on purpose")

  end subroutine test_0


  end module multiple2



  program test_driver
    use fortuno, only : serial_driver
    use multiple1, only : multiple1_suite => new_test_suite
    use multiple2, only : multiple2_suite => new_test_suite
    implicit none

    type(serial_driver), allocatable :: driver

    driver = serial_driver([&
        & multiple1_suite(), multiple2_suite()&
        & ])
    call driver%run()

  end program test_driver
