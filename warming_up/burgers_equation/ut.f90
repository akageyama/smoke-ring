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
!    warming_up/burgers_equation/ut.f90
!-------------------------------------------------------------------

module ut_m
  use constants_m
  implicit none
  private
  public :: ut__assert, &
            ut__fatal,  &
            ut__i2c3,   &
            ut__message

  interface ut__message
     module procedure message_decorated_str,          &
                      message_decorated_str_sint,     &
                      message_decorated_str_dint,     &
                      message_str,                    &
                      message_str_double,             &
                      message_str_double_double,      &
                      message_str_sint,               &
                      message_str_dint,               &
                      message_str_sint_double,        &
                      message_str_dint_double
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
    !
    integer(SI) :: len

    len = len_trim(string) + 4

    write(6,*) repeat(mark,len)
    write(6,*) mark//' '//trim(string)//' '//mark
    write(6,*) repeat(mark,len)
  end subroutine message_decorated_str


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


  subroutine message_decorated_str_dint(mark,string,int)
    character, intent(in)        :: mark
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: int
    !
    ! Usage:
    !    call mess...('#','This is message at nloop = ', nloop)
    !
    character(len=200) :: string_int

    write(string_int,'(a,i16)') string, int

    call message_decorated_str(mark, string_int)
  end subroutine message_decorated_str_dint


  subroutine message_str(string)
    character(len=*), intent(in) :: string

    write(6,*) string
  end subroutine message_str


  subroutine message_str_double(string, double)
    character(len=*), intent(in) :: string
    real(DR), intent(in)         :: double

    write(6,*) string, double
  end subroutine message_str_double

  subroutine message_str_double_double(string, double1, double2)
    character(len=*), intent(in) :: string
    real(DR), intent(in)         :: double1
    real(DR), intent(in)         :: double2

    write(6,*) string, double1, double2
  end subroutine message_str_double_double


  subroutine message_str_sint(string, int)
    character(len=*), intent(in) :: string
    integer(SI), intent(in)      :: int

    write(6,*) string, int
  end subroutine message_str_sint

  subroutine message_str_dint(string, int)
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: int

    write(6,*) string, int
  end subroutine message_str_dint


  subroutine message_str_sint_double(string, i1, d1)
    character(len=*), intent(in) :: string
    integer(SI), intent(in)      :: i1
    real(DR), intent(in)         :: d1

    write(6,*) string, i1, d1
  end subroutine message_str_sint_double


  subroutine message_str_dint_double(string, i1, d1)
    character(len=*), intent(in) :: string
    integer(DI), intent(in)      :: i1
    real(DR), intent(in)         :: d1

    write(6,*) string, i1, d1
  end subroutine message_str_dint_double


!
! Private
!===============
! Public
!


  subroutine ut__assert(must_be_true,message)
    logical,          intent(in) :: must_be_true
    character(len=*), intent(in) :: message

    if ( .not.must_be_true ) then
       call ut__fatal(message)
    end if
  end subroutine ut__assert


  subroutine ut__fatal(last_will)
    character(len=*), intent(in) :: last_will
    !
    !   Print the last_will and stop.
    !
    call ut__message('!',last_will)
    stop 'Program stopped by ut__fatal.'
  end subroutine ut__fatal


  function ut__i2c3(i)
    integer(SI), intent(in) :: i
    character(len=3)        :: ut__i2c3
    !
    !  Convert an integer into 3 characters.
    !             e.g., i=10 --> str3="010"
    !
    if (i>999) then
       ut__i2c3 = 'XXX'
    else
       write(ut__i2c3,'(i3.3)') i
    end if
  end function ut__i2c3

end module ut_m
