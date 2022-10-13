module parametrized2_tests
  use mylib, only : factorial
  use fortuno, only : test_case, test_suite, test_context, testroutine_ifc
  implicit none

  private
  public :: new_test_suite

  type :: calc
    integer :: arg
    integer :: res
  end type

  type, extends(test_case) :: param_test
    type(calc) :: factcalc
  end type

  interface param_test
    module procedure new_param_test
  end interface

  type(calc), parameter :: factcalcs(*) = [&
      & calc(0, 1), calc(1, 1), calc(2, 2), calc(3, 6), calc(4, 24)&
      & ]


contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    integer :: icalc
    character(200) :: name

    testsuite = test_suite("param2")
    do icalc = 1, size(factcalcs)
      write(name, "(a, i0, a)") "factorial(", factcalcs(icalc)%arg, ")"
      call testsuite%add_test_case(param_test(trim(name), test_fact_calc, factcalcs(icalc)))
    end do

  end function new_test_suite


  subroutine test_fact_calc(ctx)
    class(test_context), pointer, intent(in) :: ctx

    type(param_test), pointer :: testcase
    testcase => param_test_ptr(ctx%testcase)

    call ctx%check(factorial(testcase%factcalc%arg) == testcase%factcalc%res)
    if (ctx%failed()) return

  end subroutine test_fact_calc


  function new_param_test(name, testroutine, factcalc) result(this)
    character(*), intent(in) :: name
    procedure(testroutine_ifc), pointer, intent(in) :: testroutine
    type(calc), intent(in) :: factcalc
    type(param_test) :: this

    this%test_case = test_case(name, testroutine)
    this%factcalc = factcalc

  end function new_param_test


  function param_test_ptr(testcase) result(mycase)
    class(test_case), pointer, intent(in) :: testcase
    type(param_test), pointer :: mycase

    select type (testcase)
    type is (param_test)
      mycase => testcase
    class default
      error stop "Internal error, expected param_test, received something else"
    end select

  end function param_test_ptr

end module parametrized2_tests


program test_driver
  use fortuno, only : serial_driver
  use parametrized2_tests, only : new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([new_test_suite()])
  call driver%run()

end program test_driver
