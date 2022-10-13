program test_version
  use fortuno, only : get_version

  integer, parameter :: major_expected = 0
  integer, parameter :: minor_expected = 1
  integer, parameter :: patch_expected = 0
  integer :: major, minor, patch

  call get_version(major, minor, patch)
  if (major /= major_expected) error stop "Unexpected major version obtained"
  if (minor /= minor_expected) error stop "Unexpected minor version obtained"
  if (patch /= patch_expected) error stop "Unexpected patch version obtained"

end program test_version
