program testapp
  use fortuno_serial, only : test_app
  use testmod_simple, only : simple_suite
  use testmod_fixtured, only : fixtured_suite
  use testmod_parametrized, only : parametrized_suite
  use testmod_parametrized2, only : parametrized2_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app([&
      & simple_suite(),&
      & fixtured_suite(),&
      & parametrized_suite(),&
      & parametrized2_suite()&
      & ])
  call app%run()

end program testapp
