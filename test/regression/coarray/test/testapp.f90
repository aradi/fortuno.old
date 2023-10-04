program testapp_mpi
  use fortuno_coarray, only: cmd_app
  use testmod_simple, only: simple_suite => new_suite
  implicit none

  type(cmd_app), allocatable :: app

  app = cmd_app([&
      & simple_suite()&
      & ])
  call app%run()

end program testapp_mpi
