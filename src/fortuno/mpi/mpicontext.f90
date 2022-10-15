module fortuno_mpi_mpicontext
  use mpi_f08, only : mpi_comm, mpi_allreduce, MPI_IN_PLACE, MPI_LOGICAL, MPI_LAND
  use fortuno_basetypes, only : test_case, test_context, test_suite
  use fortuno_contextfactory, only : context_factory
  use fortuno_mpi_mpifailureinfo, only : mpi_failure_info
  implicit none

  private
  public :: mpi_context, mpi_context_ptr
  public :: mpi_context_factory
  public :: mpi_env

  type :: mpi_env
    type(mpi_comm) :: comm
    integer :: rank
    integer :: commsize
  end type mpi_env


  type, extends(test_context) :: mpi_context
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
    logical :: globalcond(0 : this%mpi%commsize - 1)

    this%nchecks = this%nchecks + 1

    globalcond(:) = .true.
    globalcond(this%mpi%rank) = cond
    call mpi_allreduce(MPI_IN_PLACE, globalcond, this%mpi%commsize, MPI_LOGICAL, MPI_LAND, &
        & this%mpi%comm)
    if (all(globalcond)) return
    call this%mark_as_failed()
    allocate(failureinfo)
    failureinfo%checknr = this%nchecks
    if (present(msg)) failureinfo%message = msg
    if (present(file)) failureinfo%file = file
    if (present(line)) failureinfo%line = line
    failureinfo%failedranks = .not. globalcond
    call move_alloc(failureinfo, this%failureinfo)

  end subroutine mpi_context_check_logical


  function mpi_context_ptr(trg) result(ptr)
    class(test_context), pointer, intent(in) :: trg
    type(mpi_context), pointer :: ptr

    select type (trg)
    type is (mpi_context)
      ptr => trg
    class default
      error stop "Internal error, expected mpi_context, received something else"
    end select

  end function mpi_context_ptr


  subroutine mpi_context_factory_create_context(this, testsuite, testcase, ctx)
    class(mpi_context_factory), intent(in) :: this
    class(test_suite), pointer, intent(in) :: testsuite
    class(test_case), pointer, intent(in) :: testcase
    class(test_context), allocatable, intent(out) :: ctx

    type(mpi_context), allocatable :: mpictx

    allocate(mpictx)
    mpictx%testsuite => testsuite
    mpictx%testcase => testcase
    mpictx%mpi = this%mpi
    call move_alloc(mpictx, ctx)

  end subroutine mpi_context_factory_create_context


end module fortuno_mpi_mpicontext
