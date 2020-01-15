!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.15
!
!  ユーティリティライブラリ
!  
!  @note これは教育用のコードである。
!        メモリの節約については全く考慮していない。
!
module ut_m
  use constants_m
  implicit none
  private
  public :: ut__assert, &
            ut__fatal,  &
            ut__i2c3,   &
            ut__message

  interface ut__message
    !! 文字列（+ 数値）を標準出力に書き出すルーチンの多重定義
    !! 
    !! `message_decorated_なんとか` は飾り文字で囲む
    !! 
    !! `message_型名[_型名]` は文字列の後のその型の数値を同じ行に書く
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
    !! 飾り文字でメーセージ文を囲む
    !!  
    !!   Usage:
    !!
    !!      call message_decorated_str('#',"This is a test.")
    !!
    !!   Output:
    !! 
    !!      ###################
    !!      # This is a test. #
    !!      ###################
    !!  
    character, intent(in)        :: mark    !! この文字でメーセージを囲む
    character(len=*), intent(in) :: string  !! 出力メーセージ（文字列）
     
    integer(SI) :: len

    len = len_trim(string) + 4

    write(6,*) repeat(mark,len)
    write(6,*) mark//' '//trim(string)//' '//mark
    write(6,*) repeat(mark,len)
  end subroutine message_decorated_str


  subroutine message_decorated_str_sint(mark,string,int)
    !!  飾り文字でメーセージ文と単精度整数を囲む
    !!
    !!  Usage:
    !!
    !!     call mess...('#','This is message at nloop = ', nloop)
    !!
    character, intent(in)        :: mark   !! この文字で全体を囲む
    character(len=*), intent(in) :: string !! メーセージ文字列
    integer(SI), intent(in)      :: int    !! 文字列の後に書く単精度整数
    character(len=200) :: string_int

    write(string_int,'(a,i8)') string, int

    call message_decorated_str(mark, string_int)
  end subroutine message_decorated_str_sint


  subroutine message_decorated_str_dint(mark,string,int) 
    !! 飾り文字でメーセージ文と倍精度整数を囲む
    !!
    !! Usage:
    !!
    !!    call mess...('#','This is message at nloop = ', nloop)
    !!
    character, intent(in)        :: mark   !! この文字で全体を囲む
    character(len=*), intent(in) :: string !! メーセージ文字列
    integer(DI), intent(in)      :: int    !! 文字列の後に書く倍精度整数
    character(len=200) :: string_int

    write(string_int,'(a,i16)') string, int

    call message_decorated_str(mark, string_int)
  end subroutine message_decorated_str_dint


  subroutine message_str(string)
    !! 普通のprint文
    character(len=*), intent(in) :: string !! メーセージ文字列

    write(6,*) string
  end subroutine message_str


  subroutine message_str_double(string, double)
    !! 文字列 + 倍精度実数2つの出力
    character(len=*), intent(in) :: string !! メーセージ文字列
    real(DR), intent(in)         :: double !! 書き出される実数

    write(6,*) string, double
  end subroutine message_str_double

  subroutine message_str_double_double(string, double1, double2)
    !! 飾り文字でメーセージ文と倍精度実数2つを囲む
    character(len=*), intent(in) :: string  !! メーセージ文字列
    real(DR), intent(in)         :: double1 !! 書き出される実数1
    real(DR), intent(in)         :: double2 !! 書き出される実数2

    write(6,*) string, double1, double2
  end subroutine message_str_double_double


  subroutine message_str_sint(string, int)
    !! 文字列 + 単精度整数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(SI), intent(in)      :: int     !! 書き出される整数

    write(6,*) string, int
  end subroutine message_str_sint

  subroutine message_str_dint(string, int)
    !! 文字列 + 倍精度整数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(DI), intent(in)      :: int     !! 書き出される整数

    write(6,*) string, int
  end subroutine message_str_dint


  subroutine message_str_sint_double(string, i1, d1)
    !! 文字列 + 単精度整数 + 倍精度実数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(SI), intent(in)      :: i1      !! 書き出される整数
    real(DR), intent(in)         :: d1      !! 書き出される実数

    write(6,*) string, i1, d1
  end subroutine message_str_sint_double


  subroutine message_str_dint_double(string, i1, d1)
    !! 文字列 + 倍精度整数 + 倍精度実数の出力
    character(len=*), intent(in) :: string  !! メーセージ文字列
    integer(DI), intent(in)      :: i1      !! 書き出される整数
    real(DR), intent(in)         :: d1      !! 書き出される実数

    write(6,*) string, i1, d1
  end subroutine message_str_dint_double


!
! Private
!===============
! Public
!


  subroutine ut__assert(must_be_true,message)
    !! アサーション
    logical,          intent(in) :: must_be_true !! 必須条件
    character(len=*), intent(in) :: message      !! 遺言

    if ( .not.must_be_true ) then
       call ut__fatal(message)
    end if
  end subroutine ut__assert


  subroutine ut__fatal(last_will)
    !! 致命的エラー
    !!
    !!   Print the last_will and stop.
    !!
    character(len=*), intent(in) :: last_will   !! 遺言
    call ut__message('!',last_will)
    stop 'Program stopped by ut__fatal.'
  end subroutine ut__fatal


  function ut__i2c3(i)
    !! 整数を文字列（3文字固定）に変換する
    !!
    !!  Convert an integer into 3 characters.
    !!
    !!             e.g., i=10 --> str3="010"
    !!
    integer(SI), intent(in) :: i
    character(len=3)        :: ut__i2c3
    if (i>999) then
       ut__i2c3 = 'XXX'
    else
       write(ut__i2c3,'(i3.3)') i
    end if
  end function ut__i2c3

end module ut_m
