module testmod_coa_simple
  use mylib, only : factorial
  use fortuno, only : is_equal, suite_base
  use fortuno_coarray, only : context => coa_context, suite => coa_suite, test => coa_test,&
      & coa_test_base
  implicit none


  type, extends(coa_test_base) :: div_n_failure
    procedure(test_divnfailure), nopass, pointer :: testproc
    integer :: divisor, remainder
  contains
    procedure :: run
  end type

contains


  function test_suite() result(testsuite)
    type(suite) :: testsuite

    testsuite = suite("coa_simple", [&
        & test("broadcast", test_broadcast),&
        & test("allreduce", test_allreduce),&
        & test("imgs_lt_4", test_imgs_lt_4),&
        & test("imgs_ge_4", test_imgs_ge_4)&
        & ])
    call testsuite%add_test(&
        & div_n_failure("divnfailure(3, 0)", test_divnfailure, divisor=3, remainder=0))

  end function test_suite


  subroutine test_broadcast(ctx)
    class(context), intent(inout) :: ctx

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
    class(context), intent(inout) :: ctx

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


  subroutine test_imgs_lt_4(ctx)
    class(context), intent(inout) :: ctx

    if (num_images() >= 4) then
      call ctx%skip()
      return
    end if
    ! Here you can put tests, which work only up to 3 images

  end subroutine test_imgs_lt_4


  subroutine test_imgs_ge_4(ctx)
    class(context), intent(inout) :: ctx

    if (num_images() < 4) then
      call ctx%skip()
      return
    end if
    ! Here you can put tests, which work only for 4 images or more

  end subroutine test_imgs_ge_4


  subroutine test_divnfailure(ctx, mytest)
    class(context), intent(inout) :: ctx
    class(div_n_failure), intent(in) :: mytest

    character(100) :: msg

    if (mod(this_image() - 1, mytest%divisor) == mytest%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose on image ", this_image()
      call ctx%check(.false., msg=trim(msg))
    else
      call ctx%check(.true.)
    end if

    if (mod(this_image() - 2, mytest%divisor) == mytest%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose (the 2nd time) on image ", this_image()
      call ctx%check(is_equal(3, 2), msg=trim(msg))
    else
      call ctx%check(is_equal(2, 2))
    end if

    if (mod(this_image() - 3, mytest%divisor) == mytest%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose (the 3rd time) on image ", this_image()
      call ctx%check(is_equal(4, 3), msg=trim(msg))
    else
      call ctx%check(is_equal(3, 3))
    end if

  end subroutine test_divnfailure


  subroutine run(this, ctx)
    class(div_n_failure), intent(inout) :: this
    class(context), intent(inout) :: ctx

    call this%testproc(ctx, this)

  end subroutine run

end module testmod_coa_simple


program testapp_coa_simple
  use fortuno_coarray, only : coa_app
  use testmod_coa_simple, only : test_suite
  implicit none

  type(coa_app), allocatable :: app

  app = coa_app([test_suite()])
  call app%run()

end program testapp_coa_simple
