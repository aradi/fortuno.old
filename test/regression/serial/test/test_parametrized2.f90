module testmod_parametrized2
  use mylib, only: factorial
  use fortuno_serial, only: check, test_base, test_suite, test_suite_base_cls
  implicit none

  type :: calc
    integer :: arg, res
  end type

  type(calc), parameter :: factcalcs(*) = [&
      & calc(0, 1), calc(1, 1), calc(2, 2), calc(3, 6), calc(4, 24)&
      & ]

  type, extends(test_base) :: factcalc_test
    type(calc) :: data
  contains
    procedure :: run
  end type factcalc_test

contains

  function new_suite() result(suite)
    type(test_suite_base_cls) :: suite

    integer :: icalc
    character(200) :: name

    suite%instance = test_suite("parametrized2")
    do icalc = 1, size(factcalcs)
      write (name, "(a, i0)") "factorial_", factcalcs(icalc)%arg
      call suite%instance%add_test(&
          & factcalc_test(trim(name), data=factcalcs(icalc)))
    end do

  end function new_suite

  subroutine run(this)
    class(factcalc_test), intent(inout) :: this

    call check(factorial(this%data%arg) == this%data%res)

  end subroutine run

end module testmod_parametrized2
