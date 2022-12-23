module fortuno_mpi_simplified
  use fortuno_mpi_mpigctx, only : check, check_failed, comm_handle_f, comm_handle_f08, comm_rank,&
      & comm_size, failed, globalctx => mpigctx, skip, suite_ptr
  use fortuno_mpi_mpicmdapp, only : cmd_app => mpi_cmd_app
  use fortuno_mpi_mpitest, only : fixtured_test => mpi_fixtured_test, test => mpi_test,&
      & test_base_cls => mpi_test_base_cls
  use fortuno_mpi_mpidriver, only : test_driver => mpi_driver
  use fortuno_mpi_mpisuite, only : test_suite => mpi_suite, suite_base_cls => mpi_suite_base_cls
  implicit none

end module fortuno_mpi_simplified
