submodule (fortuno_basetypes) testsuite
  implicit none

contains


  module function new_test_suite(name, testcases) result(this)
    character(*), intent(in) :: name
    class(test_case), optional, intent(in) :: testcases(:)
    type(test_suite) :: this

    this%name = name
    if (present(testcases)) call this%add_test_case(testcases)

  end function new_test_suite


  module subroutine test_suite_set_up(this, ctx)
    class(test_suite), intent(inout) :: this
    class(test_context), pointer, intent(in) :: ctx
  end subroutine test_suite_set_up


  module subroutine test_suite_tear_down(this, ctx)
    class(test_suite), intent(inout) :: this
    class(test_context), pointer, intent(in) :: ctx
  end subroutine test_suite_tear_down


  module subroutine test_suite_add_test_case_single(this, testcase)
    class(test_suite), intent(inout) :: this
    class(test_case), intent(in) :: testcase

    call add_slots_(this%testcases, 1)
    this%testcases(size(this%testcases))%instance = testcase

  end subroutine test_suite_add_test_case_single


  module subroutine test_suite_add_test_case_array(this, testcases)
    class(test_suite), intent(inout) :: this
    class(test_case), intent(in) :: testcases(:)

    integer :: istart, ii

    call add_slots_(this%testcases, size(testcases))
    istart = size(this%testcases) - size(testcases)
    do ii = 1, size(testcases)
      this%testcases(istart + ii)%instance = testcases(ii)
    end do

  end subroutine test_suite_add_test_case_array


  subroutine add_slots_(testcases, newslots)
    type(test_case_cls), allocatable, intent(inout) :: testcases(:)
    integer, intent(in) :: newslots

    type(test_case_cls), allocatable :: buffer(:)
    integer :: ii

    if (.not. allocated(testcases)) then
      allocate(testcases(newslots))
    else
      call move_alloc(testcases, buffer)
      allocate(testcases(size(buffer) + newslots))
      do ii = 1, size(buffer)
        call move_alloc(buffer(ii)%instance, testcases(ii)%instance)
      end do
    end if

  end subroutine add_slots_


end submodule testsuite
