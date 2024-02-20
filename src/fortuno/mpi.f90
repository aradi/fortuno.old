module fortuno_mpi
  use fortuno_common, only: teststatus, is_equal, test_name, test_error, driver_result, get_version
  use fortuno_mpi_simplified, only: check, check_failed, cmd_app, comm_handle_f,&
      & comm_handle_f08, comm_rank, comm_size, failed, globalctx, skip, suite_ptr, test,&
      & test_driver, test_base, test_base_cls, test_suite, test_suite_base_cls
  implicit none

end module fortuno_mpi
