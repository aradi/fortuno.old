module fortuno_mpi_mpidriver
  use iso_fortran_env, only : stderr => error_unit
  use mpi_f08, only : MPI_Comm, MPI_Comm_rank, MPI_Comm_size, MPI_COMM_WORLD, MPI_Finalize, MPI_Init
  use fortuno_genericcontext, only : generic_context
  use fortuno_contextfactory, only : context_factory
  use fortuno_genericdriver, only : generic_driver, test_runner
  use fortuno_mpi_mpicontext, only : mpi_context, mpi_context_factory, mpi_env
  use fortuno_mpi_mpigctx, only : restore_global_context, set_global_context
  use fortuno_mpi_mpilogger, only : mpi_logger
  use fortuno_mpi_mpisuite, only : mpi_suite_base, mpi_suite_base_cls
  use fortuno_mpi_mpitest, only : mpi_test_base
  use fortuno_genericsuite, only : generic_suite, generic_suite_cls
  use fortuno_generictest, only : generic_test
  use fortuno_testerror, only : test_error
  use fortuno_testlogger, only : test_logger
  use fortuno_utils, only : string
  implicit none

  private
  public :: mpi_driver


  type, extends(test_runner) :: mpi_runner
  contains
    procedure :: set_up_suite
    procedure :: tear_down_suite
    procedure :: run_test
  end type mpi_runner


  type, extends(generic_driver) :: mpi_driver
    type(MPI_Comm) :: mpicomm
    integer :: commsize
    integer :: myrank
  contains
    procedure :: set_up
    procedure :: tear_down
    procedure :: create_context_factory
    procedure :: create_logger
    procedure :: create_test_runner
    procedure :: stop_on_error
  end type


  interface mpi_driver
    module procedure new_mpi_driver_suite, new_mpi_driver_suite_cls
  end interface

contains


  function new_mpi_driver_suite(testsuites) result(this)
    class(generic_suite), optional, intent(in) :: testsuites(:)
    type(mpi_driver) :: this

    if (present(testsuites)) call this%add_generic_suite(testsuites)

  end function new_mpi_driver_suite


  function new_mpi_driver_suite_cls(testsuites) result(this)
    type(mpi_suite_base_cls), intent(in) :: testsuites(:)
    type(mpi_driver) :: this

    type(generic_suite_cls), allocatable :: sbc(:)
    integer :: isuite

    allocate(sbc(size(testsuites)))
    do isuite = 1, size(testsuites)
      sbc(isuite)%instance = testsuites(isuite)%instance
    end do
    call this%add_generic_suite(sbc)

  end function new_mpi_driver_suite_cls


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


  subroutine create_test_runner(this, runner)
    class(mpi_driver), intent(in) :: this
    class(test_runner), allocatable, intent(out) :: runner

    allocate(mpi_runner :: runner)

  end subroutine create_test_runner


  subroutine stop_on_error(this, error)
    class(mpi_driver), intent(inout) :: this
    type(test_error), allocatable, intent(in) :: error

    if (.not. allocated(error)) return
    if (allocated(error%message) .and. this%myrank == 0) write(stderr, "(a)") error%message
    call MPI_Finalize()
    error stop 1, quiet = .true.

  end subroutine stop_on_error


  subroutine set_up_suite(this, testsuite, ctx)
    class(mpi_runner), intent(in) :: this
    class(generic_suite), pointer, intent(in) :: testsuite
    class(generic_context), pointer, intent(in) :: ctx

    class(mpi_context), pointer :: myctx, oldctx
    class(mpi_suite_base), pointer :: mysuite

    select type(ctx)
    class is (mpi_context)
      myctx => ctx
    class default
      error stop "Internal error, expected mpi_context, obtained something else"
    end select

    select type(testsuite)
    class is (mpi_suite_base)
      mysuite => testsuite
    class default
      error stop "Internal error, expected mpi_context, obtained something else"
    end select

    call set_global_context(myctx, oldctx)
    call mysuite%set_up()
    call restore_global_context(oldctx)

  end subroutine set_up_suite


  subroutine tear_down_suite(this, testsuite, ctx)
    class(mpi_runner), intent(in) :: this
    class(generic_suite), pointer, intent(in) :: testsuite
    class(generic_context), pointer, intent(in) :: ctx

    class(mpi_context), pointer :: myctx, oldctx
    class(mpi_suite_base), pointer :: mysuite

    select type(ctx)
    class is (mpi_context)
      myctx => ctx
    class default
      error stop "Internal error, expected mpi_context, obtained something else"
    end select

    select type(testsuite)
    class is (mpi_suite_base)
      mysuite => testsuite
    class default
      error stop "Internal error, expected mpi_suite_base, obtained something else"
    end select

    call set_global_context(myctx, oldctx)
    call mysuite%tear_down()
    call restore_global_context(oldctx)

  end subroutine tear_down_suite


  subroutine run_test(this, test, ctx)
    class(mpi_runner), intent(in) :: this
    class(generic_test), pointer, intent(in) :: test
    class(generic_context), pointer, intent(in) :: ctx

    class(mpi_context), pointer :: myctx, oldctx
    class(mpi_test_base), pointer :: mytest

    select type(ctx)
    class is (mpi_context)
      myctx => ctx
    class default
      error stop "Internal error, expected mpi_context, obtained something else"
    end select

    select type(test)
    class is (mpi_test_base)
      mytest => test
    class default
      error stop "Internal error, expected mpi_test_base, obtained something else"
    end select

    call set_global_context(myctx, oldctx)
    call mytest%run()
    call restore_global_context(oldctx)

  end subroutine run_test

end module fortuno_mpi_mpidriver
