module fortuno_mpi_mpicontext
  use mpi_f08, only : mpi_comm, mpi_allreduce, MPI_IN_PLACE, MPI_INTEGER, MPI_PROD
  use fortuno_genericcontext, only : generic_context
  use fortuno_contextfactory, only : context_factory
  use fortuno_mpi_mpifailureinfo, only : mpi_failure_info
  use fortuno_genericsuite, only : generic_suite
  use fortuno_generictest, only : generic_test

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


  type, extends(generic_context) :: mpi_context
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
    integer :: globalcondint(this%mpi%commsize)

    globalcond(:) = .true.
    globalcond(this%mpi%rank + 1) = cond

    ! workaround: ifort and intelmpi
    ! allreduce() with MPI_LOGICALS and MPI_LAND seems to result in some broken logical
    ! representation resulting in incorrect findloc(..., dim=1) results. Therefore, map logicals
    ! to integers before reduction and map them back afterwards again.
    globalcondint(:) = merge(1, 0, globalcond)
    call mpi_allreduce(MPI_IN_PLACE, globalcondint, this%mpi%commsize, MPI_INTEGER, MPI_PROD,&
        & this%mpi%comm)
    globalcond(:) = globalcondint == 1

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


  subroutine mpi_context_factory_create_context(this, testsuite, ctx)
    class(mpi_context_factory), intent(in) :: this
    class(generic_suite), pointer, intent(in) :: testsuite
    class(generic_context), allocatable, intent(out) :: ctx

    type(mpi_context), allocatable :: mpictx

    allocate(mpictx)
    mpictx%suite=> testsuite
    mpictx%mpi = this%mpi
    call move_alloc(mpictx, ctx)

  end subroutine mpi_context_factory_create_context

end module fortuno_mpi_mpicontext
