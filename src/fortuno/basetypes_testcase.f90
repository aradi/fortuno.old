submodule (fortuno_basetypes) testcase
  implicit none

contains

  module subroutine test_case_get_status_str(this, state)
    class(test_case), intent(in) :: this
    character(:), allocatable, intent(out) :: state
  end subroutine test_case_get_status_str


end submodule testcase
