module testmod_selftest_fixture
  use fortuno_serial, only : check, test, test_suite
  implicit none

  private
  public :: selftest_fixture_suite

contains


  function selftest_fixture_suite() result(suite)
    type(test_suite) :: suite

    suite = test_suite("selftest_fixture", [&
        & test("check_t", test_check_t),&
        & test("check_f", test_check_f),&
        & test("check_tf", test_check_tf),&
        & test("check_tft", test_check_tft)&
        & ])

  end function selftest_fixture_suite


  subroutine test_check_t()
    call check(.true.)
  end subroutine test_check_t


  subroutine test_check_f()
    call check(.false.)
  end subroutine test_check_f


  subroutine test_check_tf()
    call check(.true.)
    call check(.false.)
  end subroutine test_check_tf


  subroutine test_check_tft()
    call check(.true.)
    call check(.false.)
    call check(.true.)
  end subroutine test_check_tft

end module testmod_selftest_fixture


module testmod_selftest
  use fortuno_serial, only : check, check_failed, driver_result, is_equal, test_driver, test,&
      & test_name, ts => teststatus, test_suite
  use testmod_selftest_fixture, only : selftest_fixture_suite
  implicit none

  ! Results obtained after running the tests of the "selftest_fixture" suite during initialization
  type(driver_result), allocatable :: drvres

contains


  function selftest_suite() result(suite)
    type(test_suite) :: suite

    call set_up_global_fixture()
    suite = test_suite("selftest", [&
        & test("nr_of_entries", test_nr_of_entries),&
        & test("results", test_results)&
        & ])

  end function selftest_suite


  subroutine set_up_global_fixture()
    type(test_driver), allocatable :: driver

    driver = test_driver([selftest_fixture_suite()])
    call driver%run(driverresult=drvres)

  end subroutine set_up_global_fixture


  subroutine test_nr_of_entries()
    ! Every test suite has two results: setup and teardown
    call check(all(shape(drvres%suiteresults) == [2, 1]))
    call check(is_equal(size(drvres%testresults), 4))
  end subroutine test_nr_of_entries


  subroutine test_results()
    call check(is_equal(size(drvres%testresults), 4))
    if (check_failed()) return
    call check(all(drvres%testresults(:)%status == [ts%ok, ts%failed, ts%failed, ts%failed]))
  end subroutine test_results

end module testmod_selftest
