module testmod_parametrized
  use mylib, only: factorial
  use fortuno_serial, only: check, test_base, test_suite, test_suite_base_cls
  implicit none

  type, extends(test_base) :: factcalc_test
    integer :: arg, res
  contains
    procedure :: run => test_factcalc
  end type

contains

  function new_suite() result(suite)
    type(test_suite_base_cls) :: suite

    suite%instance =&
        & test_suite("parametrized", [&
        & factcalc_test("factorial_0", arg=0, res=1),&
        & factcalc_test("factorial_1", arg=1, res=1),&
        & factcalc_test("factorial_2", arg=2, res=2),&
        & factcalc_test("factorial_3", arg=3, res=6),&
        & factcalc_test("factorial_4", arg=4, res=24)&
        & ])

  end function new_suite

  subroutine test_factcalc(this)
    class(factcalc_test), intent(inout) :: this

    call check(factorial(this%arg) == this%res)

  end subroutine test_factcalc

end module testmod_parametrized
