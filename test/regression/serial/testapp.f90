program testapp
  use fortuno_serial, only : cmd_app, sbc => suite_base_cls
  use testmod_simple, only : simple_suite
  use testmod_fixtured, only : fixtured_suite
  use testmod_parametrized, only : parametrized_suite
  use testmod_parametrized2, only : parametrized2_suite
  implicit none

  type(cmd_app), allocatable :: app

  app = cmd_app([&
      & sbc(simple_suite()),&
      & sbc(fixtured_suite()),&
      & sbc(parametrized_suite()),&
      & sbc(parametrized2_suite())&
      & ])
  call app%run()

end program testapp
