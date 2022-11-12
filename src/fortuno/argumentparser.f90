module fortuno_argumentparser
  use fortuno_genericdriver, only : test_name
  use fortuno_utils, only : dyn_char
  implicit none

  private
  public :: argument_parser


  type :: argument_parser
    private
    type(dyn_char), allocatable :: args(:)
  contains
    procedure :: get_test_names
  end type


  interface argument_parser
    module procedure new_argument_parser
  end interface

contains


  function new_argument_parser(args) result(this)
    type(dyn_char), optional, intent(in) :: args(:)
    type(argument_parser) :: this

    if (present(args)) then
      this%args = args
    else
      call get_args_from_cmd_line_(this%args)
    end if

  end function new_argument_parser


  function get_test_names(this) result(testnames)
    class(argument_parser), intent(in) :: this
    type(test_name), allocatable :: testnames(:)

    integer :: iarg, iname, nnames

    nnames = 0
    do iarg = 1, size(this%args)
      associate (arg => this%args(iarg))
        if (arg%starts_with("-") .or. arg%starts_with("--")) cycle
        nnames = nnames + 1
      end associate
    end do

    allocate(testnames(nnames))
    iname = 0
    do iarg = 1, size(this%args)
      associate (arg => this%args(iarg))
        if (arg%starts_with("-") .or. arg%starts_with("--")) cycle
        iname = iname + 1
        call get_test_name_from_arg_(arg%as_char(), testnames(iname))
      end associate
    end do

  end function get_test_names


  subroutine get_args_from_cmd_line_(args)
    type(dyn_char), allocatable, intent(out) :: args(:)

    integer :: nargs, iarg, arglen

    nargs = command_argument_count()
    allocate(args(nargs))
    do iarg = 1, nargs
      block
        character(:), allocatable :: buffer
        call get_command_argument(iarg, length=arglen)
        allocate(character(arglen) :: buffer)
        call get_command_argument(iarg, value=buffer)
        args(iarg) = buffer
      end block
    end do

  end subroutine get_args_from_cmd_line_


  subroutine get_test_name_from_arg_(arg, testname)
    character(*), intent(in) :: arg
    type(test_name), intent(out) :: testname

    integer :: seppos

    seppos = index(arg, "/")
    if (seppos == 0) then
      testname%suitename = arg
      testname%testname = ""
      return
    end if
    testname%suitename = arg(1 : seppos - 1)
    testname%testname = arg(seppos + 1 :)

  end subroutine get_test_name_from_arg_

end module fortuno_argumentparser