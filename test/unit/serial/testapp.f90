program testapp
  use fortuno_serial, only : test_app
  use testmod_selftest, only : selftest_suite
  implicit none

  type(test_app), allocatable :: app

  app = test_app([selftest_suite()])
  call app%run()

end program testapp
