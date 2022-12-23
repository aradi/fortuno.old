program testapp
  use fortuno_serial, only : cmd_app
  use testmod_selftest, only : selftest_suite
  implicit none

  type(cmd_app), allocatable :: app

  app = cmd_app([selftest_suite()])
  call app%run()

end program testapp
