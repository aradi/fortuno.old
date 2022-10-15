module fortuno_genericdriver
  use iso_fortran_env, only : stderr => error_unit
  use fortuno_basetypes, only : test_suite, test_suite_cls, test_context, test_case
  use fortuno_contextfactory, only : context_factory
  use fortuno_testlogger, only : driver_result, test_logger, test_status, init_test_status
  use fortuno_testerror, only : test_error
  implicit none

  private
  public :: generic_driver, test_name


  type :: test_name
    character(:), allocatable :: suitename
    character(:), allocatable :: casename
  end type test_name


  type, abstract :: generic_driver
    type(test_suite_cls), allocatable :: testsuites(:)
  contains
    procedure :: add_test_suite_single
    procedure :: add_test_suite_array
    generic :: add_test_suite => add_test_suite_single, add_test_suite_array
    procedure :: run
    procedure :: set_up
    procedure :: tear_down
    procedure(create_context_factory_iface), deferred :: create_context_factory
    procedure(create_logger_iface), deferred :: create_logger
    procedure(stop_on_error_iface), deferred :: stop_on_error
  end type generic_driver


  abstract interface

    subroutine stop_on_error_iface(this, error)
      import :: generic_driver, test_error
      implicit none
      class(generic_driver), intent(inout) :: this
      type(test_error), allocatable, intent(in) :: error
    end subroutine stop_on_error_iface

    subroutine create_context_factory_iface(this, ctxfact)
      import :: generic_driver, context_factory
      implicit none
      class(generic_driver), intent(in) :: this
      class(context_factory), allocatable, intent(out) :: ctxfact
    end subroutine create_context_factory_iface

    subroutine create_logger_iface(this, logger)
      import :: generic_driver, test_logger
      implicit none
      class(generic_driver), intent(in) :: this
      class(test_logger), allocatable, intent(out) :: logger
    end subroutine create_logger_iface

  end interface


contains


  subroutine add_test_suite_single(this, testsuite)
    class(generic_driver), intent(inout) :: this
    class(test_suite), intent(in) :: testsuite

    call add_slots_(this%testsuites, 1)
    this%testsuites(size(this%testsuites))%instance = testsuite

  end subroutine add_test_suite_single


  subroutine add_test_suite_array(this, testsuites)
    class(generic_driver), intent(inout) :: this
    class(test_suite), intent(in) :: testsuites(:)

    integer :: ii, istart

    call add_slots_(this%testsuites, size(testsuites))
    istart = size(this%testsuites) - size(testsuites)
    do ii = 1, size(testsuites)
      this%testsuites(istart + ii)%instance = testsuites(ii)
    end do

  end subroutine add_test_suite_array


  subroutine run(this, testnames, error, driverresult)
    class(generic_driver), target, intent(inout) :: this
    type(test_name), optional, intent(in) :: testnames(:)
    type(test_error), allocatable, optional, intent(out) :: error
    type(driver_result), allocatable, optional, intent(out) :: driverresult

    class(test_logger), allocatable :: logger
    type(test_error), allocatable :: error0
    type(driver_result), allocatable :: driverresult0
    class(context_factory), allocatable :: ctxfact
    integer, allocatable :: testinds(:, :)

    call this%set_up()

    call this%create_context_factory(ctxfact)
    call this%create_logger(logger)
    call get_test_indices_(this%testsuites, testinds, testnames=testnames)
    call run_tests_(this%testsuites, testinds, ctxfact, logger, driverresult0)

    if (driverresult0%failed) error0 = test_error(code=1, message="Some tests failed")
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
    type(test_suite_cls), intent(in) :: testsuites(:)
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


  subroutine run_tests_(testsuites, testinds, ctxfact, logger, driverresult)
    type(test_suite_cls), target, intent(inout) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    class(context_factory), intent(in) :: ctxfact
    class(test_logger), intent(inout) :: logger
    type(driver_result), allocatable, intent(out) :: driverresult

    call allocate_driver_result_(testsuites, testinds, driverresult)
    call initialize_suites_(testsuites, testinds, ctxfact, driverresult)
    call logger%begin_short_log()
    call execute_tests_(testsuites, testinds, ctxfact, logger, driverresult)
    call logger%end_short_log()
    call finalize_suites_(testsuites, testinds, ctxfact, driverresult)

    driverresult%failed = .not. (all(driverresult%suiteresults(:)%success) &
        & .and. all(driverresult%caseresults(:)%success))

  end subroutine run_tests_


  subroutine initialize_suites_(testsuites, testinds, ctxfact, driverresult)
    type(test_suite_cls), target, intent(inout) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    class(context_factory), intent(in) :: ctxfact
    type(driver_result), intent(inout) :: driverresult

    class(test_context), allocatable, target :: ctx
    class(test_context), pointer :: ctxptr
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
          & suiteresult => driverresult%suiteresults(isuiteres))

        call ctxfact%create_context(testsuite, null(), ctx)
        ctxptr => ctx
        call testsuite%set_up(ctxptr)
        call init_test_status(suiteresult, .not. ctx%failed(), testsuite%name, repr, ctx)
        done(isuite) = .true.

      end associate
    end do

  end subroutine initialize_suites_


  subroutine finalize_suites_(testsuites, testinds, ctxfact, driverresult)
    type(test_suite_cls), target, intent(inout) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    class(context_factory), intent(in) :: ctxfact
    type(driver_result), intent(inout) :: driverresult

    class(test_context), allocatable, target :: ctx
    class(test_context), pointer :: ctxptr
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
          & suiteresult => driverresult%suiteresults(isuiteres))

        ! Suite initialization failed, finalization should be skipped
        if  (.not. suiteresult%success) then
          done(isuite) = .true.
          cycle
        end if

        call ctxfact%create_context(testsuite, null(), ctx)
        ctxptr => ctx
        call testsuite%tear_down(ctxptr)
        call init_test_status(suiteresult, .not. ctx%failed(), testsuite%name, repr, ctx)
        done(isuite) = .true.

      end associate
    end do

  end subroutine finalize_suites_


  subroutine execute_tests_(testsuites, testinds, ctxfact, logger, driverresult)
    type(test_suite_cls), target, intent(inout) :: testsuites(:)
    integer, intent(in) :: testinds(:,:)
    class(context_factory), intent(in) :: ctxfact
    class(test_logger), intent(inout) :: logger
    type(driver_result), intent(inout) :: driverresult

    class(test_context), allocatable, target :: ctx
    class(test_context), pointer :: ctxptr
    character(:), allocatable :: testrepr
    integer :: itest, ntests, isuite, isuiteres, icase
    logical :: success

    ntests = size(testinds, dim=2)
    do itest = 1, ntests
      isuite = testinds(1, itest)
      icase = testinds(2, itest)
      isuiteres = driverresult%casetosuite(itest)
      associate (&
          & testsuite => testsuites(isuite)%instance,&
          & testcase => testsuites(isuite)%instance%testcases(icase)%instance,&
          & suiteresult => driverresult%suiteresults(isuiteres),&
          & caseresult => driverresult%caseresults(itest))

        if (.not. suiteresult%success) then
          ! We should signalize failure here instead of just cycling over it
          cycle
        end if

        call ctxfact%create_context(testsuite, testcase, ctx)
        ctxptr => ctx
        call testcase%run(ctxptr)
        call testcase%get_status_str(testrepr)
        success = .not. ctx%failed()
        call logger%short_log_result(testsuite%name, testcase%name, success)
        call init_test_status(caseresult, success, testcase%name, testrepr, ctx)
      end associate
    end do

  end subroutine execute_tests_


  subroutine add_slots_(testsuites, newslots)
    type(test_suite_cls), allocatable, intent(inout) :: testsuites(:)
    integer, intent(in) :: newslots

    type(test_suite_cls), allocatable :: buffer(:)
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
    type(test_suite_cls), allocatable, intent(in) :: testsuites(:)
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
    type(test_suite_cls), intent(in) :: testsuites(:)
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

    allocate(driverresult%suiteresults(nnewsuites))
    allocate(driverresult%caseresults(ntests))

    allocate(driverresult%casetosuite(ntests))
    do itest = 1, ntests
      driverresult%casetosuite(itest) = suitemap(testinds(1, itest))
    end do

  end subroutine allocate_driver_result_


end module fortuno_genericdriver