module testsuite_parametrized
  use mylib, only : factorial
  use fortuno, only : context => serial_context, suite => serial_suite, serial_test_base
  implicit none


  type, extends(serial_test_base) :: factcalc_test
    integer :: arg, res
  contains
    procedure :: run
  end type

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    testsuite = suite("param", [&
        & factcalc_test("factorial(0)", 0, 1),&
        & factcalc_test("factorial(1)", 1, 1),&
        & factcalc_test("factorial(2)", 2, 2),&
        & factcalc_test("factorial(3)", 3, 6),&
        & factcalc_test("factorial(4)", 4, 24)&
        & ])

  end function test_suite


  subroutine run(this, ctx)
    class(factcalc_test), intent(inout) :: this
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(this%arg) == this%res)

  end subroutine run

end module testsuite_parametrized


program testdriver_parameterized
  use fortuno, only : serial_driver
  use testsuite_parametrized, only : test_suite
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([test_suite()])
  call driver%run()

end program testdriver_parameterized
