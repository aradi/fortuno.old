module fortuno_mpi_mpicontext
  use mpi_f08, only : mpi_comm, mpi_allreduce, MPI_IN_PLACE, MPI_LOGICAL, MPI_LAND
  use fortuno_basetypes, only : test_base, context_base, suite_base
  use fortuno_contextfactory, only : context_factory
  use fortuno_mpi_mpifailureinfo, only : mpi_failure_info
  implicit none

  private
  public :: mpi_context
  public :: mpi_context_factory
  public :: mpi_env

  type :: mpi_env
    type(mpi_comm) :: comm
    integer :: rank
    integer :: commsize
  end type mpi_env


  type, extends(context_base) :: mpi_context
    type(mpi_env) :: mpi
    logical, allocatable :: failedranks(:)
  contains
    procedure :: check_logical => mpi_context_check_logical
  end type mpi_context


  type, extends(context_factory) :: mpi_context_factory
    type(mpi_env) :: mpi
  contains
    procedure :: create_context => mpi_context_factory_create_context
  end type mpi_context_factory


contains

  subroutine mpi_context_check_logical(this, cond, msg, file, line)
    class(mpi_context), intent(inout) :: this
    logical, intent(in) :: cond
    character(*), optional, intent(in) :: msg
    character(*), optional, intent(in) :: file
    integer, optional, intent(in) :: line

    type(mpi_failure_info), allocatable :: failureinfo
    logical :: globalcond(this%mpi%commsize)

    globalcond(:) = .true.
    globalcond(this%mpi%rank + 1) = cond
    call mpi_allreduce(MPI_IN_PLACE, globalcond, this%mpi%commsize, MPI_LOGICAL, MPI_LAND,&
        & this%mpi%comm)
    call this%register_check(all(globalcond))
    if (.not. this%check_failed()) return
    allocate(failureinfo)
    failureinfo%checknr = this%nchecks
    if (present(msg)) failureinfo%message = msg
    if (present(file)) failureinfo%file = file
    if (present(line)) failureinfo%line = line
    failureinfo%failedranks = .not. globalcond
    if (allocated(this%failureinfo)) call move_alloc(this%failureinfo, failureinfo%previous)
    call move_alloc(failureinfo, this%failureinfo)

  end subroutine mpi_context_check_logical


  subroutine mpi_context_factory_create_context(this, testsuite, testcase, ctx)
    class(mpi_context_factory), intent(in) :: this
    class(suite_base), pointer, intent(in) :: testsuite
    class(test_base), pointer, intent(in) :: testcase
    class(context_base), allocatable, intent(out) :: ctx

    type(mpi_context), allocatable :: mpictx

    allocate(mpictx)
    mpictx%testsuite => testsuite
    mpictx%testcase => testcase
    mpictx%mpi = this%mpi
    call move_alloc(mpictx, ctx)

  end subroutine mpi_context_factory_create_context


end module fortuno_mpi_mpicontext
