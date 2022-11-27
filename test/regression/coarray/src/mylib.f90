module mylib
  implicit none

  private
  public :: allreduce_sum, broadcast

contains


  !> Broadcasts a scalar integer.
  subroutine broadcast(buffer, source)

    !> Buffer to broadcast
    integer, intent(inout) :: buffer[*]

    !> Source image
    integer, intent(in) :: source

    ! Algorithm below serves only demonstration purposes, use co_broadcast() in production code.
    sync all
    buffer = buffer[source]

  end subroutine broadcast


  !> Reduces a scalar integer by summation on all images.
  subroutine allreduce_sum(val)

    !> Value to reduce by summation
    integer, intent(inout) :: val[*]

    integer :: iimg

    ! Algorithm below serves only demonstration purposes, use co_reduce() in production code
    sync all
    if (this_image() == 1) then
      do iimg = 2, num_images()
        val = val + val[iimg]
      end do
    end if
    sync all
    val = val[1]

  end subroutine allreduce_sum

end module mylib
