submodule (fortuno_basetypes) testcase
  implicit none

contains


  module function new_test_case(name, testroutine, options) result(this)
    character(*), intent(in) :: name
    procedure(testroutine_ifc), pointer, intent(in) :: testroutine
    type(test_options), optional, intent(in) :: options
    type(test_case) :: this

    this%name = name
    this%testroutine => testroutine
    if (present(options)) this%options = options

  end function new_test_case


  module subroutine test_case_set_up(ctx)
    class(test_context), pointer, intent(in) :: ctx
    continue
  end subroutine test_case_set_up


  module subroutine test_case_run(ctx)
    class(test_context), pointer, intent(in) :: ctx

    call ctx%testcase%testroutine(ctx)

  end subroutine test_case_run


  module subroutine test_case_tear_down(ctx)
    class(test_context), pointer, intent(in) :: ctx
    continue
  end subroutine test_case_tear_down


  module subroutine test_case_get_status_str(this, state)
    class(test_case), intent(in) :: this
    character(:), allocatable, intent(out) :: state
    continue
  end subroutine test_case_get_status_str


end submodule testcase
