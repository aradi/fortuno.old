module testmod_parametrized
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
        & factcalc_test("factorial_0", 0, 1),&
        & factcalc_test("factorial_1", 1, 1),&
        & factcalc_test("factorial_2", 2, 2),&
        & factcalc_test("factorial_3", 3, 6),&
        & factcalc_test("factorial_4", 4, 24)&
        & ])

  end function test_suite


  subroutine run(this, ctx)
    class(factcalc_test), intent(inout) :: this
    class(context), intent(inout) :: ctx

    call ctx%check(factorial(this%arg) == this%res)

  end subroutine run

end module testmod_parametrized


program testapp_parameterized
  use fortuno, only : serial_app
  use testmod_parametrized, only : test_suite
  implicit none

  type(serial_app), allocatable :: app

  app = serial_app([test_suite()])
  call app%run()

end program testapp_parameterized
