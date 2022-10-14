module test_coa_simple
  use mylib, only : factorial
  use fortuno, only : test_suite, test_case, test_context
  use fortuno_coarray, only : coa_context, coa_context_ptr
  implicit none

  private
  public :: new_test_suite

  type, extends(test_case) :: div_n_failure
    integer :: divisor
    integer :: remainder
  contains
    procedure :: get_status_str => div_n_failure_get_status_str
  end type


contains

  function new_test_suite() result(testsuite)
    type(test_suite) :: testsuite

    testsuite = test_suite("simple", [&
        & test_case("factorial(0)", test_0),&
        & test_case("factorial(1)", test_1),&
        & test_case("factorial(2)", test_2)&
        & ])
    call testsuite%add_test_case(div_n_failure("divnfailure", test_3, divisor=3, remainder=0))

  end function new_test_suite


  subroutine test_0(ctx)
    class(test_context), pointer, intent(in) :: ctx

    integer, allocatable :: buffer[:]

    allocate(buffer[*])
    if (this_image() == 1) then
      buffer = 42
    else
      buffer = -1
    end if

    if (this_image() == 1) then
      call ctx%check(buffer == 42)
    else
      call ctx%check(buffer == -1)
    end if
    if (ctx%failed()) return

    buffer = buffer[1]
    call ctx%check(buffer == 42)

  end subroutine test_0


  subroutine test_1(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(1) == 1)

  end subroutine test_1


  subroutine test_2(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%check(factorial(2) == 3, msg="This has intentionally failed on all images")

  end subroutine test_2


  subroutine test_3(ctx)
    class(test_context), pointer, intent(in) :: ctx

    type(div_n_failure), pointer :: testcase
    character(100) :: msg

    testcase => div_n_failure_ptr(ctx%testcase)
    if (mod(this_image() - 1, testcase%divisor) == testcase%remainder) then
      write(msg, "(a, i0)") "This has intentionally failed on image ", this_image()
      call ctx%check(.false., trim(msg))
    else
      call ctx%check(.true.)
    end if

  end subroutine test_3


  subroutine div_n_failure_get_status_str(this, state)
    class(div_n_failure), intent(in) :: this
    character(:), allocatable, intent(out) :: state

    character(100) :: buffer

    write(buffer, "(a, i0, a, i0)") "d=", this%divisor, ",r=", this%remainder
    state = trim(buffer)

  end subroutine div_n_failure_get_status_str


  function div_n_failure_ptr(testcase) result(mycase)
    class(test_case), pointer, intent(in) :: testcase
    type(div_n_failure), pointer :: mycase

    select type (testcase)
    type is (div_n_failure)
      mycase => testcase
    class default
      error stop "Internal error, expected div_n_failure, received something else"
    end select

  end function div_n_failure_ptr


end module test_coa_simple


program test_simple_driver
  use fortuno_coarray, only : coa_driver
  use test_coa_simple, only : new_test_suite
  implicit none

  type(coa_driver), allocatable :: driver

  driver = coa_driver([new_test_suite()])
  call driver%run()

end program test_simple_driver
