module fortuno_mpi_mpisuite
  use fortuno_basetypes, only : init_suite_base, suite_base
  use fortuno_mpi_mpicontext, only : mpi_context
  use fortuno_mpi_mpitest, only : mpi_test_base
  implicit none

  private
  public :: mpi_suite, mpi_suite_base


  type, extends(suite_base) :: mpi_suite_base
  contains
    procedure :: set_up => mpi_suite_base_set_up
    procedure :: tear_down => mpi_suite_base_tear_down
  end type mpi_suite_base


  type, extends(mpi_suite_base) :: mpi_suite
  end type mpi_suite


  interface mpi_suite
    module procedure new_mpi_suite
  end interface mpi_suite

contains


  subroutine mpi_suite_base_set_up(this, ctx)
    class(mpi_suite_base), intent(inout) :: this
    class(mpi_context), intent(inout) :: ctx

  end subroutine mpi_suite_base_set_up


  subroutine mpi_suite_base_tear_down(this, ctx)
    class(mpi_suite_base), intent(inout) :: this
    class(mpi_context), intent(inout) :: ctx

  end subroutine mpi_suite_base_tear_down


  function new_mpi_suite(name, tests) result(this)
    character(*), intent(in) :: name
    class(mpi_test_base), optional, intent(in) :: tests(:)
    type(mpi_suite) :: this

    call init_suite_base(this, name, tests)

  end function new_mpi_suite

end module fortuno_mpi_mpisuite