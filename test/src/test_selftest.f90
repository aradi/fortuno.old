module test_simple
  use mylib, only : factorial
  use fortuno, only : test_suite, test_case, test_context, test_options
  implicit none

  private
  public :: new_test_suite

contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("simple", [&
        & test_case("factorial(0)", test_0),&
        & test_case("factorial(1)", test_1),&
        & test_case("factorial(2)", test_2),&
        & test_case("should_fail", test_3, options=test_options(should_fail=.true.))&
        & ])

  end function new_test_suite


  subroutine test_0(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(0) == 1)

  end subroutine test_0


  subroutine test_1(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(1) == 1)

  end subroutine test_1


  subroutine test_2(ctx)
    class(test_context), pointer, intent(in) :: ctx

    !call ctx%check(factorial(2) == 3, msg="This has failed on purpose")
    call ctx%check(factorial(2) == 2)

  end subroutine test_2


  subroutine test_3(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(.false.)

  end subroutine test_3


end module test_simple


module test_simple_tester
  use fortuno, only : driver_result, is_equal, serial_driver, test_case, test_context, test_name,&
      & test_suite
  use test_simple, only : new_test_simple_suite => new_test_suite
  implicit none

  type(driver_result), allocatable :: drvres

contains

  subroutine set_up_module()
    type(serial_driver), allocatable :: driver

    driver = serial_driver([new_test_simple_suite()])

    call driver%run(driverresult=drvres,&
            & testnames=[test_name("simple", "factorial(0)"), test_name("simple", "factorial(1)")])

  end subroutine set_up_module


  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    call set_up_module()
    testsuite = test_suite("simple_tester", [&
        & test_case("nr_of_entries", test_nr_of_entries),&
        & test_case("results", test_results)&
        & ])

  end function new_test_suite


  subroutine test_nr_of_entries(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(is_equal(size(drvres%suiteresults), 1))
    if (ctx%failed()) return

    call ctx%check(is_equal(size(drvres%caseresults), 4), file="test_simple.f90", line=102, &
        & msg="This has intentionally so much info.")

  end subroutine test_nr_of_entries


  subroutine test_results(ctx)
    class(test_context), pointer, intent(in) :: ctx

    !call ctx%check(all(drvres%caseresults(:)%success .eqv. [.true., .true., .true., .false.]))
    call ctx%check(all(drvres%caseresults(:)%success .eqv. [.true., .true.]))

  end subroutine test_results


end module test_simple_tester


program test_simple_tester_driver
  use fortuno, only : argument_parser, serial_driver
  use test_simple_tester, only : new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver
  type(argument_parser), allocatable :: argparser

  driver = serial_driver([new_test_suite()])
  argparser = argument_parser()
  call driver%run(testnames=argparser%get_test_names())

end program test_simple_tester_driver
