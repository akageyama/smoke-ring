!-------------------------------------------------------------------
! class-hpc-smoke-ring: A simple sample field solver.
!
!    by Akira Kageyama, Kobe University, Japan.
!       email: sgks@mac.com
!
!    Copyright 2018 Akira Kageyama
!
!    This software is released under the MIT License.
!
!-------------------------------------------------------------------
!    src/ut.f90
!-------------------------------------------------------------------

module ut_m
  use constants_m
  implicit none
  private
  public :: ut__assert,       &
            ut__fatal,        &
            ut__int_to_str3,  &
            ut__int_to_str7,  &
            ut__message

  interface ut__message
     module procedure message_decorated_str,          &
                      message_decorated_str_dint,     &
                      message_decorated_str_sint,     &
                      message_str,                    &
                      message_str_double,             &
                      message_str_float,              &
                      message_str_dint,               &
                      message_str_dint_double,        &
                      message_str_dint_double_double, &
                      message_str_sint,               &
                      message_str_sint_double,        &
                      message_str_sint_double_double
  end interface



contains


  subroutine message_decorated_str(mark,string)
    character, intent(in)        :: mark
    character(len=*), intent(in) :: string
    !
    ! Usage:
    !    call message_decorated_str('#',"This is a test.")
    ! Output:
    !                              ###################
    !                              # This is a test. #
    !                              ###################
    integer(SI) :: len

    len = len_trim(string) + 4

    write(6,*) repeat(mark,len)
    write(6,*) mark//' '//trim(string)//' '//mark
    write(6,*) repeat(mark,len)
  end subroutine message_decorated_str


  subroutine message_decorated_str_dint(mark,string,int)
    character, intent(in)        :: mark
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: int
    !
    ! Usage:
    !    call mess...('#','This is message at nloop = ', nloop)
    !
    character(len=200) :: string_int

    write(string_int,'(a,i8)') string, int
    call message_decorated_str(mark, string_int)
  end subroutine message_decorated_str_dint


  subroutine message_decorated_str_sint(mark,string,int)
    character, intent(in)        :: mark
    character(len=*), intent(in) :: string
    integer(SI), intent(in)      :: int
    !
    ! Usage:
    !    call mess...('#','This is message at nloop = ', nloop)
    !
    character(len=200) :: string_int

    write(string_int,'(a,i8)') string, int
    call message_decorated_str(mark, string_int)
  end subroutine message_decorated_str_sint


  subroutine message_str(string)
    character(len=*), intent(in) :: string

    write(6,*) string
  end subroutine message_str


  subroutine message_str_double(string, double)
    character(len=*), intent(in) :: string
    real(DR), intent(in)         :: double

    write(6,*) string, double
  end subroutine message_str_double


  subroutine message_str_float(string, float)
    character(len=*), intent(in) :: string
    real(SR), intent(in)         :: float

    write(6,*) string, float
  end subroutine message_str_float


  subroutine message_str_dint(string, int)
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: int

   write(6,*) string, int
  end subroutine message_str_dint


  subroutine message_str_sint(string, int)
    character(len=*), intent(in) :: string
    integer(SI), intent(in)      :: int

   write(6,*) string, int
  end subroutine message_str_sint


  subroutine message_str_dint_double(string, i1, d1)
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: i1
    real(DR), intent(in)         :: d1

    write(6,*) string, i1, d1
  end subroutine message_str_Dint_double


  subroutine message_str_sint_double(string, i1, d1)
    character(len=*), intent(in) :: string
    integer(SI), intent(in)      :: i1
    real(DR), intent(in)         :: d1

    write(6,*) string, i1, d1
  end subroutine message_str_sint_double


  subroutine message_str_dint_double_double(string, i1, d1, d2)
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: i1
    real(DR), intent(in)         :: d1, d2

    write(6,*) string, i1, d1, d2
  end subroutine message_str_Dint_double_double


  subroutine message_str_sint_double_double(string, i1, d1, d2)
    character(len=*), intent(in) :: string
    integer(SI), intent(in)      :: i1
    real(DR), intent(in)         :: d1, d2

    write(6,*) string, i1, d1, d2
  end subroutine message_str_sint_double_double



!
! Private
!===============
! Public
!


  subroutine ut__assert(condition, last_will)
    logical, intent(in)          :: condition
    character(len=*), intent(in) :: last_will

    if (.not.condition) then
       call ut__fatal(last_will)
    end if
  end subroutine ut__assert


  subroutine ut__fatal(last_will)
    character(len=*), intent(in) :: last_will
    !
    !  Print fatal message and exit.
    !
    call ut__message('!',last_will)
    stop 'Program stopped by ut__fatal.'
  end subroutine ut__fatal


  function ut__int_to_str3(i) result(str3)
    integer(SI), intent(in) :: i
    character(len=3) :: str3
    !
    !  Convert an integer into 3 characters.
    !             e.g., i=10 --> str3="010"
    !
    if ( i>999 ) then
      str3 = 'XXX'
    else
      write(str3,'(i3.3)') i
    end if
  end function ut__int_to_str3



  function ut__int_to_str7(i) result(str7)
    integer(SI), intent(in) :: i
    character(len=7) :: str7
    !
    !  Convert an integer into 7 characters.
    !             e.g., i=12345 --> str7="0012345"
    !
    if ( i>9999999 ) then
      str7 = 'XXXXXXX'
    else
      write(str7,'(i7.7)') i
    end if
  end function ut__int_to_str7

end module ut_m
