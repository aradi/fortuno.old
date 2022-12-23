program testapp
  use fortuno_serial, only : cmd_app
  use testmod_simple, only : simple_suite
  use testmod_fixtured, only : fixtured_suite
  use testmod_parametrized, only : parametrized_suite
  use testmod_parametrized2, only : parametrized2_suite
  implicit none

  type(cmd_app), allocatable :: app

  app = cmd_app([&
      & simple_suite(),&
      & fixtured_suite(),&
      & parametrized_suite(),&
      & parametrized2_suite()&
      & ])
  call app%run()

end program testapp
