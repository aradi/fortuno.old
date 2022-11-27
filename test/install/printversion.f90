program test_build
  use iso_fortran_env, only : stdout => output_unit
  use fortuno_serial, only : get_version
  implicit none

  integer :: major, minor, patch

  call get_version(major, minor, patch)
  write(stdout, "(a, 3(i0, a))") "Fortuno (", major, ".", minor, ".", patch, ")"

end program test_build
