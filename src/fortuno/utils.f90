module fortuno_utils
  implicit none

  private
  public :: dyn_char
  public :: keyword_arg_enforcer_
  public :: nr_digits, string


  interface string
    module procedure string_from_integer
  end interface


  type :: dyn_char
    private
    character(:), allocatable :: content
  contains
    procedure, private :: assign_from_char => dyn_char_assign_from_char
    procedure, pass(this), private :: assign_to_char => dyn_char_assign_to_char
    generic :: assignment(=) => assign_from_char, assign_to_char
    procedure :: as_char => dyn_char_as_char
    procedure, private :: get_repr_len => dyn_char_get_repr_len
    procedure :: has_content => dyn_char_has_content
    procedure :: starts_with => dyn_char_starts_with
  end type dyn_char


  ! Dummy type used to enforce explicit keyword arguments in type initializations
  ! Should only be used internally in fortuno and not exported to consumers!
  type :: keyword_arg_enforcer_
  end type keyword_arg_enforcer_


contains


  subroutine dyn_char_assign_from_char(this, rhs)
    class(dyn_char), intent(out) :: this
    character(*), intent(in) :: rhs

    this%content = rhs

  end subroutine dyn_char_assign_from_char


  subroutine dyn_char_assign_to_char(lhs, this)
    character(*), intent(out) :: lhs
    class(dyn_char), intent(in) :: this

    lhs = this%content

  end subroutine dyn_char_assign_to_char


  pure function dyn_char_get_repr_len(this) result(reprlen)
    class(dyn_char), intent(in) :: this
    integer :: reprlen

    if (allocated(this%content)) then
      reprlen = len(this%content)
    else
      reprlen = 0
    end if

  end function dyn_char_get_repr_len


  pure function dyn_char_as_char(this) result(chr)
    class(dyn_char), intent(in) :: this
    character(len=this%get_repr_len()) :: chr

    if (len(chr) > 0) chr = this%content

  end function dyn_char_as_char


  pure function dyn_char_has_content(this) result(hascontent)
    class(dyn_char), intent(in) :: this
    logical :: hascontent

    hascontent = allocated(this%content)

  end function dyn_char_has_content


  pure function dyn_char_starts_with(this, str) result(starts_with)
    class(dyn_char), intent(in) :: this
    character(*), intent(in) :: str
    logical :: starts_with

    integer :: strlen

    starts_with = .false.
    if (.not. this%has_content()) return
    strlen = len(str)
    if (len(this%content) < strlen) return
    starts_with = (this%content(1:strlen) == str)

  end function dyn_char_starts_with


  function string_from_integer(val, maxval) result(str)
    integer, intent(in) :: val
    integer, optional, intent(in) :: maxval
    character(:), allocatable :: str

    integer, parameter :: maxintlen = int(log(real(huge(maxval))) / log(10.0)) + 1
    ! Add extra char to accomodate eventual minus sign
    integer, parameter :: maxintlenlen = int(log(real(maxintlen)) / log(10.0)) + 1

    character(maxintlen + 1) :: buffer
    character(2 * maxintlenlen + 4) :: formstr
    integer :: ndigits

    if (present(maxval)) then
      ndigits = nr_digits(maxval)
      write(formstr, "(a, i0, a, i0, a)") "(i", ndigits, ".", ndigits, ")"
      write(buffer, formstr) val
    else
      write(buffer, "(i0)") val
    end if
    str = trim(buffer)

  end function string_from_integer


  function nr_digits(val) result(ndigits)
    integer, intent(in) :: val
    integer :: ndigits

    ! Make sure, we can treat val = 0 correctly
    ndigits = int(log(real(max(abs(val), 1))) / log(10.0)) + 1
    if (val < 0) ndigits = ndigits + 1

  end function nr_digits

end module fortuno_utils