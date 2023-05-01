!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.22
!
!  デバッグ用の標準出力ルーチン
!
!  @note 
!    parameterのDebugがtrueのときだけ処理する
!
module debug_m
  use constants_m ! 定数定義
  use ut_m        ! ユーティリティ
  implicit none   ! 暗黙の型宣言無効化。必須

  private ! このモジュール内の変数・ルーチン等はデフォルトで非公開
  public :: debug__print

  interface debug__print
    !! 各種デバッグ出力の多重定義
    module procedure print_str,             &
                     print_str_dint,        &
                     print_str_dint_double, &
                     print_str_double,      &
                     print_str_sint,        &
                     print_str_sint_double
  end interface


contains


  subroutine print_str(string)
    !! Debugパラメータがtrueのとき文字列を出力する
    character(len=*), intent(in) :: string !! 出力文字列

    if ( DEBUG_MODE ) then
      ! Debugをflaseに設定したときは何もしない
      call ut__message('debug: '//string)
    end if
  end subroutine print_str


  subroutine print_str_double(string, double)
    !! Debugパラメータがtrueのとき文字列と実数を出力する
    character(len=*), intent(in) :: string !! 出力文字列
    real(DR), intent(in)         :: double !! 出力実数

    if ( DEBUG_MODE ) then
      ! Debugをflaseに設定したときは何もしない
      call ut__message('debug: '//string, double)
    end if
  end subroutine print_str_double


  subroutine print_str_dint(string, int)
    !! Debugパラメータがtrueのとき文字列と整数を出力する
    character(len=*), intent(in) :: string !! 出力文字列
    integer(DI), intent(in)      :: int    !! 出力整数

    if ( DEBUG_MODE ) then
      ! Debugをflaseに設定したときは何もしない
      call ut__message('debug: '//trim(string), int)
    end if
  end subroutine print_str_dint


  subroutine print_str_sint(string, int)
    !! Debugパラメータがtrueのとき文字列と整数を出力する
    character(len=*), intent(in) :: string !! 出力文字列
    integer(SI), intent(in)      :: int    !! 出力整数

    if ( DEBUG_MODE ) then
      ! Debugをflaseに設定したときは何もしない
      call ut__message('debug: '//trim(string), int)
    end if
  end subroutine print_str_sint


  subroutine print_str_dint_double(string, i1, d1)
    !! Debugパラメータがtrueのとき文字列と整数と実数を出力する
    character(len=*), intent(in) :: string !! 出力文字列
    integer(DI), intent(in)      :: i1     !! 出力整数
    real(DR), intent(in)         :: d1     !! 出力実数

    if ( DEBUG_MODE ) then
      call ut__message('debug: '//trim(string), i1, d1)
    end if
  end subroutine print_str_dint_double


  subroutine print_str_sint_double(string, i1, d1)
    !! Debugパラメータがtrueのとき文字列と整数と実数を出力する
    character(len=*), intent(in) :: string !! 出力文字列
    integer(SI), intent(in)      :: i1     !! 出力整数
    real(DR), intent(in)         :: d1     !! 出力実数

    if ( DEBUG_MODE ) then
      ! Debugをflaseに設定したときは何もしない
      call ut__message('debug: '//trim(string), i1, d1)
    end if
  end subroutine print_str_sint_double

end module debug_m
