module fortuno_mpi
  use fortuno_mpi_mpicontext, only : mpi_context, mpi_context_ptr
  use fortuno_mpi_mpidriver, only : mpi_driver
  implicit none

  private
  public :: mpi_context, mpi_context_ptr, mpi_driver

end module fortuno_mpi