module fortuno_genericdriver
  use fortuno_basetypes, only : suite_base, suite_base_cls, context_base, test_base, teststatus
  use fortuno_contextfactory, only : context_factory
  use fortuno_testlogger, only : driver_result, test_logger, init_test_result, testtypes
  use fortuno_testerror, only : test_error
  implicit none

  private
  public :: generic_driver, test_name, test_runner


  type, abstract :: test_runner
  contains
    procedure(set_up_suite_i), deferred :: set_up_suite
    procedure(tear_down_suite_i), deferred :: tear_down_suite
    procedure(run_test_i), deferred :: run_test
  end type test_runner


  abstract interface

    subroutine set_up_suite_i(this, testsuite, ctx)
      import :: test_runner, suite_base, context_base
      implicit none
      class(test_runner), intent(in) :: this
      class(suite_base), pointer, intent(in) :: testsuite
      class(context_base), pointer, intent(in) :: ctx
    end subroutine set_up_suite_i


    subroutine tear_down_suite_i(this, testsuite, ctx)
      import :: test_runner, suite_base, context_base
      implicit none
      class(test_runner), intent(in) :: this
      class(suite_base), pointer, intent(in) :: testsuite
      class(context_base), pointer, intent(in) :: ctx
    end subroutine tear_down_suite_i


    subroutine run_test_i(this, testcase, ctx)
      import :: test_runner, test_base, context_base
      implicit none
      class(test_runner), intent(in) :: this
      class(test_base), pointer, intent(in) :: testcase
      class(context_base), pointer, intent(in) :: ctx
    end subroutine run_test_i

  end interface


  type :: test_name
    character(:), allocatable :: suitename
    character(:), allocatable :: casename
  end type test_name


  type, abstract :: generic_driver
    type(suite_base_cls), allocatable :: testsuites(:)
  contains
    procedure :: add_suite_base_scalar
    procedure :: add_suite_base_array
    generic :: add_suite_base => add_suite_base_scalar, add_suite_base_array
    procedure :: run
    procedure :: set_up
    procedure :: tear_down
    procedure(create_context_factory_i), deferred :: create_context_factory
    procedure(create_logger_i), deferred :: create_logger
    procedure(create_test_runner_i), deferred :: create_test_runner
    procedure(stop_on_error_i), deferred :: stop_on_error
  end type generic_driver


  abstract interface

    subroutine stop_on_error_i(this, error)
      import :: generic_driver, test_error
      implicit none
      class(generic_driver), intent(inout) :: this
      type(test_error), allocatable, intent(in) :: error
    end subroutine stop_on_error_i

    subroutine create_context_factory_i(this, ctxfact)
      import :: generic_driver, context_factory
      implicit none
      class(generic_driver), intent(in) :: this
      class(context_factory), allocatable, intent(out) :: ctxfact
    end subroutine create_context_factory_i

    subroutine create_logger_i(this, logger)
      import :: generic_driver, test_logger
      implicit none
      class(generic_driver), intent(in) :: this
      class(test_logger), allocatable, intent(out) :: logger
    end subroutine create_logger_i

    subroutine create_test_runner_i(this, runner)
      import :: generic_driver, test_runner
      class(generic_driver), intent(in) :: this
      class(test_runner), allocatable, intent(out) :: runner
    end subroutine create_test_runner_i

  end interface

contains


  subroutine add_suite_base_scalar(this, testsuite)
    class(generic_driver), intent(inout) :: this
    class(suite_base), intent(in) :: testsuite

    call add_slots_(this%testsuites, 1)
    this%testsuites(size(this%testsuites))%instance = testsuite

  end subroutine add_suite_base_scalar


  subroutine add_suite_base_array(this, testsuites)
    class(generic_driver), intent(inout) :: this
    class(suite_base), intent(in) :: testsuites(:)

    integer :: ii, istart

    call add_slots_(this%testsuites, size(testsuites))
    istart = size(this%testsuites) - size(testsuites)
    do ii = 1, size(testsuites)
      this%testsuites(istart + ii)%instance = testsuites(ii)
    end do

  end subroutine add_suite_base_array


  subroutine run(this, testnames, error, driverresult)
    class(generic_driver), target, intent(inout) :: this
    type(test_name), optional, intent(in) :: testnames(:)
    type(test_error), allocatable, optional, intent(out) :: error
    type(driver_result), allocatable, optional, intent(out) :: driverresult

    class(test_logger), allocatable :: logger
    class(test_runner), allocatable :: runner
    type(test_error), allocatable :: error0
    type(driver_result), allocatable :: driverresult0
    class(context_factory), allocatable :: ctxfact
    integer, allocatable :: testinds(:, :)

    call this%set_up()

    call this%create_context_factory(ctxfact)
    call this%create_logger(logger)
    call this%create_test_runner(runner)
    call get_test_indices_(this%testsuites, testinds, testnames=testnames)
    call run_tests_(this%testsuites, testinds, ctxfact, logger, runner, driverresult0)

    if (driverresult0%failed) error0 = test_error(code=1, message="Some checks failed")
    if (present(error)) call move_alloc(error0, error)
    if (present(driverresult)) call move_alloc(driverresult0, driverresult)
    if (present(error) .or. present(driverresult)) return

    call logger%log_results(driverresult0)
    call this%stop_on_error(error0)

    call this%tear_down()

  end subroutine run


  subroutine set_up(this)
    class(generic_driver), intent(inout) :: this

  end subroutine set_up


  subroutine tear_down(this)
    class(generic_driver), intent(inout) :: this

  end subroutine tear_down


  subroutine get_test_indices_(testsuites, testinds, testnames)
    type(suite_base_cls), intent(in) :: testsuites(:)
    integer, allocatable, intent(out) :: testinds(:,:)
    type(test_name), optional, intent(in) :: testnames(:)

    logical :: usetestnames
    integer :: itest, isuite, icase, ntests
    integer :: ii

    ntests = 0
    usetestnames = .false.
    if (present(testnames)) then
      ntests = size(testnames)
      usetestnames = ntests > 0
    end if
    if (ntests == 0) then
      do isuite = 1, size(testsuites)
        ntests = ntests + size(testsuites(isuite)%instance%testcases)
      end do
    end if

    allocate(testinds(2, ntests))

    if (usetestnames) then
      do itest = 1, ntests
        isuite = 0
        do ii = 1, size(testsuites)
          if (testsuites(ii)%instance%name == testnames(itest)%suitename) then
            isuite = ii
            exit
          end if
        end do
        if (isuite == 0) error stop "Test suite '" // testnames(itest)%suitename // "' not found"

        icase = 0
        do ii = 1, size(testsuites(isuite)%instance%testcases)
          if (testsuites(isuite)%instance%testcases(ii)%instance%name&
              & == testnames(itest)%casename) then
            icase = ii
            exit
          end if
        end do
        if (icase == 0) error stop "Test case '" // testnames(itest)%suitename // "/"&
            & // testnames(itest)%casename // "' not found"

        testinds(:, itest) = [isuite, icase]
      end do
    else
      itest = 0
      do isuite = 1, size(testsuites)
        associate(testsuite => testsuites(isuite)%instance)
          do icase = 1, size(testsuite%testcases)
            associate(testcase => testsuite%testcases(icase)%instance)
              itest = itest + 1
              testinds(:, itest) = [isuite, icase]
            end associate
          end do
        end associate
      end do

    end if

  end subroutine get_test_indices_


  subroutine run_tests_(testsuites, testinds, ctxfact, logger, runner, driverresult)
    type(suite_base_cls), target, intent(inout) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    class(context_factory), intent(in) :: ctxfact
    class(test_logger), intent(inout) :: logger
    class(test_runner), intent(in) :: runner
    type(driver_result), allocatable, intent(out) :: driverresult

    call allocate_driver_result_(testsuites, testinds, driverresult)
    call logger%begin_short_log()
    call initialize_suites_(testsuites, testinds, ctxfact, logger, runner, driverresult)
    call execute_tests_(testsuites, testinds, ctxfact, logger, runner, driverresult)
    call finalize_suites_(testsuites, testinds, ctxfact, logger, runner, driverresult)
    call logger%end_short_log()

    driverresult%failed = any(driverresult%suiteresults(:,:)%status == teststatus%failed) &
        & .or. any(driverresult%caseresults(:)%status == teststatus%failed)

  end subroutine run_tests_


  subroutine initialize_suites_(testsuites, testinds, ctxfact, logger, runner, driverresult)
    type(suite_base_cls), target, intent(inout) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    class(context_factory), intent(in) :: ctxfact
    class(test_logger), intent(inout) :: logger
    class(test_runner), intent(in) :: runner
    type(driver_result), intent(inout) :: driverresult

    class(context_base), allocatable, target :: ctx
    class(context_base), pointer :: ctxptr
    character(:), allocatable :: repr
    logical, allocatable :: done(:)
    integer :: nsuites, ntests, itest, isuite, isuiteres

    nsuites = size(testsuites)
    ntests = size(testinds, dim=2)

    allocate(done(nsuites), source=.false.)
    do itest = 1, ntests
      isuite = testinds(1, itest)
      if (done(isuite)) cycle
      isuiteres = driverresult%casetosuite(itest)

      associate (&
          & testsuite => testsuites(isuite)%instance,&
          & suiteresult => driverresult%suiteresults(1, isuiteres))

        call ctxfact%create_context(testsuite, null(), ctx)
        ctxptr => ctx
        call runner%set_up_suite(testsuite, ctxptr)
        call testsuite%get_char_repr(repr)
        call init_test_result(suiteresult, testsuite%name, repr, ctx)
        call logger%short_log_result(testtypes%suitesetup, suiteresult)
        done(isuite) = .true.

      end associate
    end do

  end subroutine initialize_suites_


  subroutine finalize_suites_(testsuites, testinds, ctxfact, logger, runner, driverresult)
    type(suite_base_cls), target, intent(inout) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    class(context_factory), intent(in) :: ctxfact
    class(test_logger), intent(inout) :: logger
    class(test_runner), intent(in) :: runner
    type(driver_result), intent(inout) :: driverresult

    class(context_base), allocatable, target :: ctx
    class(context_base), pointer :: ctxptr
    character(:), allocatable :: repr
    logical, allocatable :: done(:)
    integer :: nsuites, ntests, itest, isuite, isuiteres

    nsuites = size(testsuites)
    ntests = size(testinds, dim=2)

    allocate(done(nsuites), source=.false.)
    do itest = 1, ntests
      isuite = testinds(1, itest)
      isuiteres = driverresult%casetosuite(itest)
      if (done(isuite)) cycle

      associate (&
          & testsuite => testsuites(isuite)%instance,&
          & suiteresults => driverresult%suiteresults(:, isuiteres))

        call ctxfact%create_context(testsuite, null(), ctx)
        if  (suiteresults(1)%status == teststatus%ok) then
          ctxptr => ctx
          call runner%tear_down_suite(testsuite, ctxptr)
        else
          call ctx%skip()
        end if
        call testsuite%get_char_repr(repr)
        call init_test_result(suiteresults(2), testsuite%name, repr, ctx)
        call logger%short_log_result(testtypes%suiteteardown, suiteresults(2))
        done(isuite) = .true.

      end associate
    end do

  end subroutine finalize_suites_


  subroutine execute_tests_(testsuites, testinds, ctxfact, logger, runner, driverresult)
    type(suite_base_cls), target, intent(inout) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    class(context_factory), intent(in) :: ctxfact
    class(test_logger), intent(inout) :: logger
    class(test_runner), intent(in) :: runner
    type(driver_result), intent(inout) :: driverresult

    class(context_base), allocatable, target :: ctx
    class(context_base), pointer :: ctxptr
    character(:), allocatable :: testrepr
    integer :: itest, ntests, isuite, isuiteres, icase

    ntests = size(testinds, dim=2)
    do itest = 1, ntests
      isuite = testinds(1, itest)
      icase = testinds(2, itest)
      isuiteres = driverresult%casetosuite(itest)
      associate (&
          & testsuite => testsuites(isuite)%instance,&
          & testcase => testsuites(isuite)%instance%testcases(icase)%instance,&
          & suiteresult => driverresult%suiteresults(1, isuiteres),&
          & caseresult => driverresult%caseresults(itest))

        call ctxfact%create_context(testsuite, testcase, ctx)
        if (suiteresult%status /= teststatus%ok) then
          call ctx%skip()
        else
          ctxptr => ctx
          call runner%run_test(testcase, ctxptr)
        end if
        call testcase%get_char_repr(testrepr)
        call init_test_result(caseresult, testcase%name, testrepr, ctx)
        call logger%short_log_result(testtypes%caserun, suiteresult, caseresult)
      end associate
    end do

  end subroutine execute_tests_


  subroutine add_slots_(testsuites, newslots)
    type(suite_base_cls), allocatable, intent(inout) :: testsuites(:)
    integer, intent(in) :: newslots

    type(suite_base_cls), allocatable :: buffer(:)
    integer :: ii

    if (.not. allocated(testsuites)) then
      allocate(testsuites(newslots))
    else
      call move_alloc(testsuites, buffer)
      allocate(testsuites(size(buffer) + newslots))
      do ii = 1, size(buffer)
        call move_alloc(buffer(ii)%instance, testsuites(ii)%instance)
      end do
    end if

  end subroutine add_slots_


  function nr_tests_(testsuites) result(ntests)
    type(suite_base_cls), allocatable, intent(in) :: testsuites(:)
    integer :: ntests

    integer :: isuite

    ntests = 0
    do isuite = 1, size(testsuites)
      associate (testsuite => testsuites(isuite)%instance)
        ntests = ntests + size(testsuite%testcases)
      end associate
    end do

  end function nr_tests_


  subroutine allocate_driver_result_(testsuites, testinds, driverresult)
    type(suite_base_cls), intent(in) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    type(driver_result), allocatable, intent(out) :: driverresult

    integer, allocatable :: suitemap(:)
    logical, allocatable :: included(:)
    integer :: isuite, itest, nsuites, ntests, nnewsuites

    nsuites = size(testsuites)
    ntests = size(testinds, dim=2)
    allocate(driverresult)

    allocate(included(nsuites), source=.false.)
    do itest = 1, ntests
      included(testinds(1, itest)) = .true.
    end do
    allocate(suitemap(nsuites), source=0)
    nnewsuites = 0
    do isuite = 1, nsuites
      if (included(isuite)) then
        nnewsuites = nnewsuites + 1
        suitemap(isuite) = nnewsuites
      end if
    end do

    allocate(driverresult%suiteresults(2, nnewsuites))
    allocate(driverresult%caseresults(ntests))

    allocate(driverresult%casetosuite(ntests))
    do itest = 1, ntests
      driverresult%casetosuite(itest) = suitemap(testinds(1, itest))
    end do

  end subroutine allocate_driver_result_

end module fortuno_genericdriver