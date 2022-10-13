module parametrizedests
  use mylib, only : factorial
  use fortuno, only : test_case, test_suite, test_context, testroutine_ifc
  implicit none

  private
  public :: new_test_suite

  type :: calc
    integer :: arg
    integer :: res
  end type

  type, extends(test_case) :: parametrized
    type(calc) :: factcalc
  end type


contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("param", [&
        & parametrized("factorial(0)", test_fact_calc, factcalc=calc(0, 1)),&
        & parametrized("factorial(1)", test_fact_calc, factcalc=calc(1, 1)),&
        & parametrized("factorial(2)", test_fact_calc, factcalc=calc(2, 2)),&
        & parametrized("factorial(3)", test_fact_calc, factcalc=calc(3, 6)),&
        & parametrized("factorial(4)", test_fact_calc, factcalc=calc(4, 24))&
        & ])

  end function new_test_suite


  subroutine test_fact_calc(ctx)
    class(test_context), pointer, intent(in) :: ctx

    type(parametrized), pointer :: caseptr
    caseptr => parametrized_ptr(ctx%testcase)

    call ctx%check(factorial(caseptr%factcalc%arg) == caseptr%factcalc%res)

  end subroutine test_fact_calc


  function parametrized_ptr(testcase) result(mycase)
    class(test_case), pointer, intent(in) :: testcase
    type(parametrized), pointer :: mycase

    select type (testcase)
    type is (parametrized)
      mycase => testcase
    class default
      error stop "Internal error, expected parametrized, received something else"
    end select

  end function parametrized_ptr


end module parametrizedests


program test_driver
  use fortuno, only : serial_driver
  use parametrizedests, only : new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([new_test_suite()])
  call driver%run()

end program test_driver
