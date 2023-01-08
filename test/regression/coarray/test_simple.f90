module testmod_simple
  use mylib, only : allreduce_sum, broadcast
  use fortuno_coarray, only : check, is_equal, skip, test_suite_base_cls, test, test_base,&
      & tbc => test_base_cls, test_suite
  implicit none


  type, extends(test_base) :: div_n_failure
    integer :: divisor, remainder
  contains
    procedure :: run => test_divnfailure
  end type

contains


  function new_suite() result(suite)
    type(test_suite_base_cls) :: suite

    ! Since the tests in the suite initializer have different types, they must be wrapped with
    ! test_base_class (Fortran does not permit arrays with elements of differing types).
    ! Alternatively you could use subsequent %add_test() calls to tests of different types.
    suite%instance =&
        & test_suite("simple", [&
        & tbc(test("broadcast", proc=test_broadcast)),&
        & tbc(test("allreduce", proc=test_allreduce)),&
        & tbc(test("imgs_lt_4", proc=test_imgs_lt_4)),&
        & tbc(test("imgs_ge_4", proc=test_imgs_ge_4)),&
        & tbc(div_n_failure("divnfailure_3_0", divisor=3, remainder=0))&
        & ])

  end function new_suite


  ! Given: source image contains a different integer value as all other images
  ! When: source image broadcasts its value
  ! Then: all images contain source image's value
  subroutine test_broadcast()

    integer, parameter :: source_img = 1, source_img_value = 1, other_img_value = -1
    integer, allocatable :: buffer[:]

    allocate(buffer[*])
    if (this_image() == source_img) then
      buffer = source_img_value
    else
      buffer = other_img_value
    end if

    call broadcast(buffer, source_img)

    call check(buffer == source_img_value)

  end subroutine test_broadcast


  ! Given: all images contain an integer with their image number as value
  ! When: all reduction is invoked with summation
  ! Then: all images contain the sum N * (N + 1) / 2, where N = nr. of images
  subroutine test_allreduce()

    integer, allocatable :: buffer[:]
    integer :: expected

    allocate(buffer[*], source=this_image())

    call allreduce_sum(buffer)

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
    class(div_n_failure), intent(inout) :: this

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

end module testmod_simple
