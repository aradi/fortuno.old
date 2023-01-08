module fortuno_mpi_mpisuite
  use fortuno_mpi_mpicontext, only : mpi_context
  use fortuno_mpi_mpitest, only : mpi_test_base, mpi_test_base_cls
  use fortuno_genericsuite, only : init_generic_suite, generic_suite
  use fortuno_generictest, only : generic_test_cls
  implicit none

  private
  public :: mpi_suite, mpi_suite_base, mpi_suite_base_cls


  type, extends(generic_suite) :: mpi_suite_base
  contains
    procedure :: set_up => mpi_suite_base_set_up
    procedure :: tear_down => mpi_suite_base_tear_down
  end type mpi_suite_base


  type :: mpi_suite_base_cls
    class(mpi_suite_base), allocatable :: instance
  end type mpi_suite_base_cls


  type, extends(mpi_suite_base) :: mpi_suite
  end type mpi_suite


  interface mpi_suite
    module procedure new_mpi_suite_test, new_mpi_suite_test_cls
  end interface mpi_suite

contains


  subroutine mpi_suite_base_set_up(this)
    class(mpi_suite_base), intent(inout) :: this

  end subroutine mpi_suite_base_set_up


  subroutine mpi_suite_base_tear_down(this)
    class(mpi_suite_base), intent(inout) :: this

  end subroutine mpi_suite_base_tear_down


  function new_mpi_suite_test(name, tests) result(this)
    character(*), intent(in) :: name
    class(mpi_test_base), optional, intent(in) :: tests(:)
    type(mpi_suite) :: this

    call init_generic_suite(this, name, tests)

  end function new_mpi_suite_test


  function new_mpi_suite_test_cls(name, tests) result(this)
    character(*), intent(in) :: name
    type(mpi_test_base_cls), intent(in) :: tests(:)
    type(mpi_suite) :: this

    type(generic_test_cls), allocatable :: stbc(:)
    integer :: itest

    call init_generic_suite(this, name)
    allocate(stbc(size(tests)))
    do itest = 1, size(tests)
      stbc(itest)%instance = tests(itest)%instance
    end do
    call this%add_test(stbc)

  end function new_mpi_suite_test_cls

end module fortuno_mpi_mpisuite