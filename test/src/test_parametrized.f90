module testsuite_parametrized
  use mylib, only : factorial
  use fortuno, only : serial_test_case, test_suite, serial_context
  implicit none


  type, extends(serial_test_case) :: factcalc_test
    integer :: arg, res
  contains
    procedure :: run
  end type


contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("param", [&
        & factcalc_test("factorial(0)", 0, 1),&
        & factcalc_test("factorial(1)", 1, 1),&
        & factcalc_test("factorial(2)", 2, 2),&
        & factcalc_test("factorial(3)", 3, 6),&
        & factcalc_test("factorial(4)", 4, 24)&
        & ])

  end function new_test_suite


  subroutine run(this, ctx)
    class(factcalc_test), intent(inout) :: this
    class(serial_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(this%arg) == this%res)

  end subroutine run

end module testsuite_parametrized


program testdriver_parameterized
  use fortuno, only : serial_driver
  use testsuite_parametrized, only : new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([new_test_suite()])
  call driver%run()

end program testdriver_parameterized
