program testapp_mpi
  use fortuno_coarray, only : test_app
  use testmod_simple, only : simple_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app([&
      & simple_suite()&
      & ])
  call app%run()

end program testapp_mpi
