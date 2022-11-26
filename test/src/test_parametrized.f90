module testmod_parametrized
  use mylib, only : factorial
  use fortuno_serial, only : check, test_suite, fixtured_test
  implicit none


  type, extends(fixtured_test) :: factcalc_test
    integer :: arg, res
  end type

contains


  function param_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("param", [&
        & factcalc_test("factorial_0", test_factorial, arg=0, res=1),&
        & factcalc_test("factorial_1", test_factorial, arg=1, res=1),&
        & factcalc_test("factorial_2", test_factorial, arg=2, res=2),&
        & factcalc_test("factorial_3", test_factorial, arg=3, res=6),&
        & factcalc_test("factorial_4", test_factorial, arg=4, res=24)&
        & ])

  end function param_suite


  subroutine test_factorial(this)
    class(factcalc_test), intent(in) :: this

    call check(factorial(this%arg) == this%res)

  end subroutine test_factorial

end module testmod_parametrized


program testapp_parameterized
  use fortuno_serial, only : test_app
  use testmod_parametrized, only : param_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app([param_suite()])
  call app%run()

end program testapp_parameterized
