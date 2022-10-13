module random_tests
  use mylib, only : factorial
  use fortuno, only : test_case, test_context, test_suite, testroutine_ifc
  implicit none

  private
  public :: new_test_suite

  type, extends(test_case) :: random_test
    integer :: nn = -1
  contains
    procedure, nopass :: set_up => random_test_set_up
    procedure :: get_status_str => random_test_get_status_str
  end type


contains

  subroutine random_test_set_up(ctx)
    class(test_context), pointer, intent(in) :: ctx

    type(random_test), pointer :: testcase
    real :: rand

    testcase => random_test_ptr(ctx%testcase)

    call random_seed()
    call random_number(rand)
    testcase%nn = int(20.0 * rand) + 1

  end subroutine random_test_set_up


  subroutine random_test_get_status_str(this, state)
    class(random_test), intent(in) :: this
    character(:), allocatable, intent(out) :: state

    character(5) :: buffer

    write(buffer, "(a, i2.2)") "n=", this%nn
    state = trim(buffer)

  end subroutine random_test_get_status_str


  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    integer :: ii

    testsuite = test_suite("fixtured", [&
        & (random_test("random_recursion", test_recursion), ii = 1, 10)&
        & ])

  end function new_test_suite


  subroutine test_recursion(ctx)
    class(test_context), pointer, intent(in) :: ctx

    type(random_test), pointer :: testcase

    testcase => random_test_ptr(ctx%testcase)
    call ctx%check(factorial(testcase%nn) == testcase%nn * factorial(testcase%nn - 1))
    if (ctx%failed()) return

  end subroutine test_recursion


  function random_test_ptr(testcase) result(mycase)
    class(test_case), pointer, intent(in) :: testcase
    type(random_test), pointer :: mycase

    select type (testcase)
    type is (random_test)
      mycase => testcase
    class default
      error stop "Internal error, expected random_test, received something else"
    end select

  end function random_test_ptr

end module random_tests


program test_driver
  use fortuno, only : serial_driver
  use random_tests, only : new_test_suite
  implicit none

  type(serial_driver), allocatable :: driver

  driver = serial_driver([new_test_suite()])
  call driver%run()

end program test_driver
