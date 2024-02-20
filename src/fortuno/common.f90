module fortuno_common
  use fortuno_checkers, only: is_equal
  use fortuno_genericcontext, only: generic_context
  use fortuno_genericdriver, only: test_name
  use fortuno_genericsuite, only: init_generic_suite, generic_suite
  use fortuno_generictest, only: generic_test
  use fortuno_testerror, only: test_error
  use fortuno_testlogger, only: driver_result
  use fortuno_teststatus, only: teststatus
  use fortuno_version, only: get_version
  implicit none

end module fortuno_common
