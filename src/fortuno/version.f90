module fortuno_version
  implicit none

  private
  public :: get_version

  integer, private :: version(3) = [ 0, 1, 0]

contains


  subroutine get_version(major, minor, patch)
    integer, intent(out) :: major, minor, patch

    major = version(1)
    minor = version(2)
    patch = version(3)

  end subroutine get_version

end module fortuno_version
