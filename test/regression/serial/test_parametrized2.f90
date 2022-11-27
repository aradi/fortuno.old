module testmod_parametrized2
  use mylib, only : factorial
  use fortuno_serial, only : check, test_suite, fixtured_test
  implicit none


  type :: calc
    integer :: arg, res
  end type

  type(calc), parameter :: factcalcs(*) = [&
      & calc(0, 1), calc(1, 1), calc(2, 2), calc(3, 6), calc(4, 24)&
      & ]


  type, extends(fixtured_test) :: factcalc_test
    type(calc) :: factcalc
  end type factcalc_test

contains


  function parametrized2_suite() result(suite)
    type(test_suite) :: suite

    integer :: icalc
    character(200) :: name

    suite = test_suite("parametrized2")
    do icalc = 1, size(factcalcs)
      write(name, "(a, i0)") "factorial_", factcalcs(icalc)%arg
      call suite%add_test(factcalc_test(trim(name), test_fact_calc, factcalc=factcalcs(icalc)))
    end do

  end function parametrized2_suite


  subroutine test_fact_calc(this)
    class(factcalc_test), intent(in) :: this

    call check(factorial(this%factcalc%arg) == this%factcalc%res)

  end subroutine test_fact_calc

end module testmod_parametrized2
