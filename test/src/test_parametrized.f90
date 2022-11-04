module testsuite_parametrized
  use mylib, only : factorial
  use fortuno, only : serial_test_base, suite_base, serial_context
  implicit none


  type, extends(serial_test_base) :: factcalc_test
    integer :: arg, res
  contains
    procedure :: run
  end type

contains


  function new_suite_base() result(testsuite)
    type(suite_base) :: testsuite

    testsuite = suite_base("param", [&
        & factcalc_test("factorial(0)", 0, 1),&
        & factcalc_test("factorial(1)", 1, 1),&
        & factcalc_test("factorial(2)", 2, 2),&
        & factcalc_test("factorial(3)", 3, 6),&
        & factcalc_test("factorial(4)", 4, 24)&
        & ])

  end function new_suite_base


  subroutine run(this, ctx)
    class(factcalc_test), intent(inout) :: this
    class(serial_context), intent(inout) :: ctx

    call ctx%check(factorial(this%arg) == this%res)

  end subroutine run

end module testsuite_parametrized


program testdriver_parameterized
  use fortuno, only : serial_driver
  use testsuite_parametrized, only : new_suite_base
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([new_suite_base()])
  call driver%run()

end program testdriver_parameterized
