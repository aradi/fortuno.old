module fortuno_suitebase
  use fortuno_testbase, only : test_base, test_base_cls
  implicit none

  private
  public :: suite_base, suite_base_cls, init_suite_base


  type, abstract :: suite_base
    character(:), allocatable :: name
    type(test_base_cls), allocatable :: tests(:)
  contains
    procedure :: set_name
    procedure, private :: add_test_scalar
    procedure, private :: add_test_array
    procedure, private :: add_test_cls_array
    generic :: add_test => add_test_scalar, add_test_array, add_test_cls_array
    procedure :: get_char_repr
  end type suite_base


  type :: suite_base_cls
    class(suite_base), allocatable :: instance
  end type suite_base_cls

contains


  subroutine init_suite_base(this, name, tests)
    class(suite_base), intent(inout) :: this
    character(*), intent(in) :: name
    class(test_base), optional, intent(in) :: tests(:)

    this%name = name
    if (present(tests)) call this%add_test(tests)

  end subroutine init_suite_base


  subroutine set_name(this, name)
    class(suite_base), intent(inout) :: this
    character(*), intent(in) :: name

    this%name = name

  end subroutine set_name


  subroutine add_test_scalar(this, test)
    class(suite_base), intent(inout) :: this
    class(test_base), intent(in) :: test

    call add_slots_(this%tests, 1)
    this%tests(size(this%tests))%instance = test

  end subroutine add_test_scalar


  subroutine add_test_array(this, tests)
    class(suite_base), intent(inout) :: this
    class(test_base), intent(in) :: tests(:)

    integer :: istart, itest

    call add_slots_(this%tests, size(tests))
    istart = size(this%tests) - size(tests)
    do itest = 1, size(tests)
      this%tests(istart + itest)%instance = tests(itest)
    end do

  end subroutine add_test_array


  subroutine add_test_cls_array(this, tests)
    class(suite_base), intent(inout) :: this
    type(test_base_cls), intent(inout) :: tests(:)

    integer :: istart, itest

    call add_slots_(this%tests, size(tests))
    istart = size(this%tests) - size(tests)
    do itest = 1, size(tests)
      call move_alloc(tests(itest)%instance, this%tests(istart + itest)%instance)
    end do

  end subroutine add_test_cls_array


  subroutine get_char_repr(this, repr)
    class(suite_base), intent(in) :: this
    character(:), allocatable, intent(out) :: repr
  end subroutine get_char_repr


  subroutine add_slots_(tests, newslots)
    type(test_base_cls), allocatable, intent(inout) :: tests(:)
    integer, intent(in) :: newslots

    type(test_base_cls), allocatable :: buffer(:)
    integer :: ii

    if (.not. allocated(tests)) then
      allocate(tests(newslots))
    else
      call move_alloc(tests, buffer)
      allocate(tests(size(buffer) + newslots))
      do ii = 1, size(buffer)
        call move_alloc(buffer(ii)%instance, tests(ii)%instance)
      end do
    end if

  end subroutine add_slots_


end module fortuno_suitebase