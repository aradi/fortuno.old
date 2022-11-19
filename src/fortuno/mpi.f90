module fortuno_mpi
  use fortuno_common, only : context_base, test_base, suite_base, init_suite_base, teststatus,&
      & is_equal, test_name, test_error, driver_result, get_version
  use fortuno_mpi_simplified, only : check, check_failed, comm_handle_f, comm_handle_f08, &
      & comm_rank, comm_size, failed, globalctx, skip, suite_ptr, test, fixtured_test, test_suite,&
      & test_driver, test_app
  implicit none

end module fortuno_mpi
