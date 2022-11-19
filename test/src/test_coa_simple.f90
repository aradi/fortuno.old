module testmod_coa_simple
  use mylib, only : factorial
  use fortuno_coarray, only : check, fixtured_test, is_equal, skip, test, test_suite
  implicit none


  type, extends(fixtured_test) :: div_n_failure
    integer :: divisor, remainder
  end type

contains


  function coa_simple_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("coa_simple", [&
        & test("broadcast", test_broadcast),&
        & test("allreduce", test_allreduce),&
        & test("imgs_lt_4", test_imgs_lt_4),&
        & test("imgs_ge_4", test_imgs_ge_4)&
        & ])
    call suite%add_test(&
        & div_n_failure("divnfailure_3_0", test_divnfailure, divisor=3, remainder=0))

  end function coa_simple_suite


  ! Given: image 1 contains a different integer value as all other images
  ! When: image 1 broadcasts its value
  ! Then: all images contain image 1's value
  subroutine test_broadcast()

    integer, parameter :: value_img1 = 1, value_otherimgs = -1
    integer, allocatable :: buffer[:]

    allocate(buffer[*])
    if (this_image() == 1) then
      buffer = value_img1
    else
      buffer = value_otherimgs
    end if

    buffer = buffer[1]

    call check(buffer == value_img1)

  end subroutine test_broadcast


  ! Given: all images contain an integer with their image number as value
  ! When: all reduction is invoked with summation (hand coded here)
  ! Then: all images contain the sum N * (N + 1) / 2, where N = nr. of images
  subroutine test_allreduce()

    integer, allocatable :: buffer[:]
    integer :: expected

    allocate(buffer[*], source=this_image())

    call coa_all_reduce(buffer)

    expected = num_images() * (num_images() + 1) / 2
    call check(buffer == expected)

  end subroutine test_allreduce


  ! Empty test executed only when nr. of images < 4.
  subroutine test_imgs_lt_4()

    if (num_images() >= 4) then
      call skip()
      return
    end if
    ! Here you can put tests, which work only up to 3 images

  end subroutine test_imgs_lt_4


  subroutine test_imgs_ge_4()

    if (num_images() < 4) then
      call skip()
      return
    end if
    ! Here you can put tests, which work only for 4 images or more

  end subroutine test_imgs_ge_4


  subroutine test_divnfailure(this)
    class(div_n_failure), intent(in) :: this

    character(100) :: msg

    if (mod(this_image() - 1, this%divisor) == this%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose on image ", this_image()
      call check(.false., msg=trim(msg))
    else
      call check(.true.)
    end if

    if (mod(this_image() - 2, this%divisor) == this%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose (the 2nd time) on image ", this_image()
      call check(is_equal(3, 2), msg=trim(msg))
    else
      call check(is_equal(2, 2))
    end if

    if (mod(this_image() - 3, this%divisor) == this%remainder) then
      write(msg, "(a, i0)") "This has failed on purpose (the 3rd time) on image ", this_image()
      call check(is_equal(4, 3), msg=trim(msg))
    else
      call check(is_equal(3, 3))
    end if

  end subroutine test_divnfailure


  ! Hand coded all reduction with summation (modern compiler might use co_sum() instead)
  subroutine coa_all_reduce(buffer)
    integer, intent(inout) :: buffer[*]

    integer :: iimg

    sync all
    if (this_image() == 1) then
      do iimg = 2, num_images()
        buffer = buffer + buffer[iimg]
      end do
    end if
    sync all
    buffer = buffer[1]

  end subroutine coa_all_reduce

end module testmod_coa_simple


program testapp_coa_simple
  use fortuno_coarray, only : test_app
  use testmod_coa_simple, only : coa_simple_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app([coa_simple_suite()])
  call app%run()

end program testapp_coa_simple
