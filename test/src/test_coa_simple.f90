module testsuite_coa_simple
  use mylib, only : factorial
  use fortuno, only : is_equal, suite_base
  use fortuno_coarray, only : coa_context, coa_test, coa_test_base
  implicit none


  type, extends(coa_test_base) :: div_n_failure
    procedure(test_divnfailure), nopass, pointer :: testproc
    integer :: divisor, remainder
  contains
    procedure :: run => div_n_failure__run
  end type

contains


  function new_suite_base() result(testsuite)
    type(suite_base) :: testsuite

    testsuite = suite_base("coa_simple", [&
        & coa_test("broadcast", test_broadcast),&
        & coa_test("allreduce", test_allreduce)&
        & ])
    call testsuite%add_test(&
        & div_n_failure("divnfailure(3, 0)", test_divnfailure, divisor=3, remainder=0))

  end function new_suite_base


  subroutine test_broadcast(ctx)
    class(coa_context), intent(inout) :: ctx

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

  end subroutine test_broadcast


  subroutine test_allreduce(ctx)
    class(coa_context), intent(inout) :: ctx

    integer, allocatable :: buffer[:]
    integer :: iimg, expected

    allocate(buffer[*])
    buffer = this_image()
    sync all

    if (this_image() == 1) then
      do iimg = 2, num_images()
        buffer= buffer + buffer[iimg]
      end do
    end if
    sync all

    buffer = buffer[1]
    expected = num_images() * (num_images() + 1) / 2
    call ctx%check(buffer == expected)

  end subroutine test_allreduce


  subroutine test_divnfailure(ctx, mycase)
    class(coa_context), intent(inout) :: ctx
    class(div_n_failure), intent(in) :: mycase

    character(100) :: msg

    if (mod(this_image() - 1, mycase%divisor) == mycase%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose on image ", this_image()
      call ctx%check(.false., msg=trim(msg))
    else
      call ctx%check(.true.)
    end if

    if (mod(this_image() - 2, mycase%divisor) == mycase%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose (the 2nd time) on image ", this_image()
      call ctx%check(is_equal(3, 2), msg=trim(msg))
    else
      call ctx%check(is_equal(2, 2))
    end if

    if (mod(this_image() - 3, mycase%divisor) == mycase%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose (the 3rd time) on image ", this_image()
      call ctx%check(is_equal(4, 3), msg=trim(msg))
    else
      call ctx%check(is_equal(3, 3))
    end if

  end subroutine test_divnfailure


  subroutine div_n_failure__run(this, ctx)
    class(div_n_failure), intent(inout) :: this
    class(coa_context), intent(inout) :: ctx

    call this%testproc(ctx, this)

  end subroutine div_n_failure__run

end module testsuite_coa_simple


program testdriver_coa_simple
  use fortuno_coarray, only : coa_driver
  use testsuite_coa_simple, only : new_suite_base
  implicit none

  type(coa_driver), allocatable :: driver

  driver = coa_driver([new_suite_base()])
  call driver%run()

end program testdriver_coa_simple
