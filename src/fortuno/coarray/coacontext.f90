module fortuno_coarray_coacontext
  use fortuno_basetypes, only : test_case, test_context, test_suite
  use fortuno_contextfactory, only : context_factory
  use fortuno_coarray_coafailureinfo, only : coa_failure_info
  implicit none

  private
  public :: coa_context, coa_context_ptr
  public :: coa_context_factory


  type, extends(test_context) :: coa_context
    logical, allocatable :: failedimages(:)
  contains
    procedure :: check_logical => coa_context_check_logical
  end type coa_context


  type, extends(context_factory) :: coa_context_factory
  contains
    procedure :: create_context => coa_context_factory_create_context
  end type coa_context_factory


contains

  subroutine coa_context_check_logical(this, cond, msg, file, line)
    class(coa_context), intent(inout) :: this
    logical, intent(in) :: cond
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    type(coa_failure_info), allocatable :: failureinfo
    logical, allocatable :: globalcond(:)[:]
    integer :: iimg

    allocate(globalcond(num_images())[*], source=.true.)
    globalcond(this_image()) = cond
    sync all

    ! TODO: replace hand-coded all reduce with corresponding reduction
    if (this_image() == 1) then
      do iimg = 2, num_images()
        globalcond(iimg) = globalcond(iimg)[iimg]
      end do
    end if
    sync all
    do iimg = 2, num_images()
      globalcond(:) = globalcond(:)[1]
    end do

    call this%register_check(all(globalcond))
    if (.not. this%check_failed()) return

    allocate(failureinfo)
    failureinfo%checknr = this%nchecks
    if (present(msg)) failureinfo%message = msg
    if (present(file)) failureinfo%file = file
    if (present(line)) failureinfo%line = line
    failureinfo%failedimages = .not. globalcond
    if (allocated(this%failureinfo)) call move_alloc(this%failureinfo, failureinfo%previous)
    call move_alloc(failureinfo, this%failureinfo)

  end subroutine coa_context_check_logical


  function coa_context_ptr(trg) result(ptr)
    class(test_context), pointer, intent(in) :: trg
    type(coa_context), pointer :: ptr

    select type (trg)
    type is (coa_context)
      ptr => trg
    class default
      error stop "Internal error, expected coa_context, received something else"
    end select

  end function coa_context_ptr


  subroutine coa_context_factory_create_context(this, testsuite, testcase, ctx)
    class(coa_context_factory), intent(in) :: this
    class(test_suite), pointer, intent(in) :: testsuite
    class(test_case), pointer, intent(in) :: testcase
    class(test_context), allocatable, intent(out) :: ctx

    type(coa_context), allocatable :: coactx

    allocate(coactx)
    coactx%testsuite => testsuite
    coactx%testcase => testcase
    call move_alloc(coactx, ctx)

  end subroutine coa_context_factory_create_context


end module fortuno_coarray_coacontext
