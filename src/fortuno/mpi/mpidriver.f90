module fortuno_mpi_mpidriver
  use iso_fortran_env, only : stderr => error_unit
  use mpi_f08, only : MPI_Comm, MPI_Comm_rank, MPI_Comm_size, MPI_COMM_WORLD, MPI_Finalize, MPI_Init
  use fortuno_basetypes, only : test_case, test_context, test_suite, test_suite_cls
  use fortuno_contextfactory, only : context_factory
  use fortuno_genericdriver, only : test_case_runner
  use fortuno_mpi_mpicontext, only : mpi_context, mpi_context_factory, mpi_env
  use fortuno_mpi_mpilogger, only : mpi_logger
  use fortuno_serial_serialdriver, only : serial_driver
  use fortuno_testerror, only : test_error
  use fortuno_testlogger, only : test_logger
  use fortuno_utils, only : string
  implicit none

  private
  public :: mpi_driver, mpi_test_case


  type, extends(test_case_runner) :: mpi_context_runner
  contains
    procedure :: run_test_case
  end type mpi_context_runner


  type, extends(serial_driver) :: mpi_driver
    type(MPI_Comm) :: mpicomm
    integer :: commsize
    integer :: myrank
  contains
    procedure :: set_up
    procedure :: tear_down
    procedure :: create_context_factory
    procedure :: create_logger
    procedure :: create_test_case_runner
    procedure :: stop_on_error
  end type


  interface mpi_driver
    module procedure new_mpi_driver
  end interface


  type, extends(test_case), abstract :: mpi_test_case
  contains
    procedure(mpi_test_case_run_iface), deferred :: run
  end type mpi_test_case


  abstract interface
    subroutine mpi_test_case_run_iface(this, ctx)
      import :: mpi_test_case, mpi_context
      class(mpi_test_case), intent(inout) :: this
      class(mpi_context), pointer, intent(in) :: ctx
    end subroutine mpi_test_case_run_iface
  end interface


contains


  function new_mpi_driver(testsuites) result(this)
    class(test_suite), optional, intent(in) :: testsuites(:)
    type(mpi_driver) :: this

    if (present(testsuites)) call this%add_test_suite(testsuites)

  end function new_mpi_driver


  subroutine set_up(this)
    class(mpi_driver), intent(inout) :: this

    this%mpicomm = MPI_COMM_WORLD
    call MPI_Init()
    call MPI_Comm_size(this%mpicomm, this%commsize)
    call MPI_Comm_rank(this%mpicomm, this%myrank)

  end subroutine set_up


  subroutine tear_down(this)
    class(mpi_driver), intent(inout) :: this

    call MPI_Finalize()

  end subroutine tear_down


  subroutine create_context_factory(this, ctxfact)
    class(mpi_driver), intent(in) :: this
    class(context_factory), allocatable, intent(out) :: ctxfact

    ctxfact = mpi_context_factory(&
        & mpi_env(comm=this%mpicomm, rank=this%myrank, commsize=this%commsize))

  end subroutine create_context_factory


  subroutine create_logger(this, logger)
    class(mpi_driver), intent(in) :: this
    class(test_logger), allocatable, intent(out) :: logger

    logger = mpi_logger(myrank=this%myrank, mpicomm=this%mpicomm)

  end subroutine create_logger


  subroutine create_test_case_runner(this, runner)
    class(mpi_driver), intent(in) :: this
    class(test_case_runner), allocatable, intent(out) :: runner

    allocate(mpi_context_runner :: runner)

  end subroutine create_test_case_runner


  subroutine stop_on_error(this, error)
    class(mpi_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    if (this%myrank == 0) write(stderr, "(a, dt)") "Error: ", error
    call MPI_Finalize()
    error stop 1, quiet = .true.

  end subroutine stop_on_error


  subroutine run_test_case(this, testcase, ctx)
    class(mpi_context_runner), intent(in) :: this
    class(test_case), pointer, intent(in) :: testcase
    class(test_context), pointer, intent(in) :: ctx

    class(mpi_context), pointer :: myctx
    class(mpi_test_case), pointer :: mycase

    select type(ctx)
    class is (mpi_context)
      myctx => ctx
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    select type(testcase)
    class is (mpi_test_case)
      mycase => testcase
    class default
      error stop "Internal error, expected serial_context, obtained something else"
    end select

    call mycase%run(myctx)

  end subroutine run_test_case


end module fortuno_mpi_mpidriver
