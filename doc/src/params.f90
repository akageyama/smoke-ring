!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   シミュレーションパラメータ
! 
!   @note 
!      パラメーターはnamelistを使ってファイルから読み込む
! 
!   @note 
!      先頭一文字だけが大文字になっている変数例えば Read_done などは
!      このモジュールを名前空間とする変数を意味する。つまりモジュール
!      外からはアクセスできないがモジュール内のルーチン関数からは
!      アクセスできるものである。
!      このようにスコープの広い変数を乱用するとバグの温床になるが
!      この程度の小さなプログラムであれば問題ないであろう。
! 
!   @note
!      ファイル番号10番をparams__readで使っている。
! 
!   @note
!     namelistファイルの内容を変更する場合は:
!       (1) このファイルの少し下の行にあるnamelist文を書き換える。
!       (2) params__readを書き換える。
!       (3) namelist__get_double, _integer等の対応する行も書き換える。
! 
!   @note
!     namelist文中のスラッシュで囲まれた名称（たとえば/simulation/）
!     や、namelist変数名（たとえばTotal_nloop）はnamelistファイル
!     の中での記述と対応していなければいけない。
!!<

module params_m
  use constants_m  ! 定数定義
  use ut_m         ! ユーティリティ
  use iso_fortran_env, only : output_unit
  implicit none    ! 暗黙の型宣言無効化。必須
  private ! このモジュール内の変数・ルーチン等はデフォルトで非公開

  logical, save :: Read_done = .false.  ! 読み込みが済んだか否か

  integer, parameter :: STRING_LENGTH_MAX = 200  ! 文字列長
                                ! 足りなくなったら大きくする。

  integer :: Total_nloop        ! 一度のジョブで計算するループ回数
  integer :: Slicedata_nskip    ! 何ステップ毎に断面データを書き出すか
  character(len=STRING_LENGTH_MAX) :: Slicedata_tag  
                                ! 断面データファイル名用
  real(DR) :: Viscosity, Kappa  ! 粘性率と熱拡散率

  namelist /simulation/     Total_nloop
  namelist /visualization/  Slicedata_nskip,   &
                            Slicedata_tag
  namelist /fluid_property/ Viscosity,  &
                            Kappa

  type :: params_t
  contains
    procedure, nopass :: read => params__read
    procedure, nopass :: get_double => params__get_double
    procedure, nopass :: get_integer => params__get_integer
    procedure, nopass :: get_logical => params__get_logical
    procedure, nopass :: get_string => params__get_string
  end type params_t

  type(params_t), public :: Params


contains


  function params__get_double( variable )
    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
    real(DR) :: params__get_double            !! その値
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が倍精度浮動小数点数の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。

    call ut__assert( Read_done, &
                     'params_m(__LINE):  Read params file first.' )

    select case (variable)
      case                 ('Kappa')     ! 熱拡散率
        params__get_double = Kappa
      case                 ('Viscosity') ! 粘性率
        params__get_double = Viscosity
      case default                       ! そんなnamelist変数は想定外
        call ut__message( '? arg = ', variable )
        call ut__fatal( 'params_m(90): case error.' )
    end select
  end function params__get_double


  function params__get_integer( variable )
    character(len=*), intent(in) :: variable    !! 問い合わせ変数の名前
    integer :: params__get_integer          !! その値
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が整数の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。

    call ut__assert( Read_done, &
                     'params_m(104): Read params file first.' )

    select case (variable)
      case                  ('Slicedata_nskip')  ! 何ステップごとに
        params__get_integer = Slicedata_nskip    ! 断面をディスクに書き出すか
      case                  ('Total_nloop')      ! シミュレーションジョブ
        params__get_integer = Total_nloop        ! の実行最大ループ数
      case default                               ! 想定外
        call ut__message( '? arg = ', variable ) 
        call ut__fatal( 'params_m(113): case error.' )
    end select
  end function params__get_integer


  function params__get_logical( variable )
    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
    logical :: params__get_logical            !! その値
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が論理値の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。

    call ut__assert( Read_done, &
                     'params_m(127): Read params file first.' )

    select case (variable)
      case default                     ! 想定外
        call ut__message( '? arg = ', variable )
        call ut__fatal( 'params_m(132): case error.' )
    end select
  end function params__get_logical


  subroutine params__read
    !! namelistファイルをディスクから読み込む。
    !! ファイル名はコマンド第一引数。
    !!
    !! namelistデータファイルの内容を変更する場合は
    !! 以下のread文も適宜変更する。
    !!
    character(len=STRING_LENGTH_MAX) :: params_file
    integer :: file_unit

    call ut__assert( command_argument_count()==1, &
                     "params_m(148): Usage: smoke_ring param_file")
    call get_command_argument(1,params_file)

    !*******<params_file のサンプル>*********
    ! &simulation      Total_nloop = 2000 /
    ! &visualization   Slicedata_nskip  = 100, Slicedata_tag = '_data_slice' /
    ! &fluid_property  Viscosity = 3.0e-2, Kappa = 3.e-2 /
    !*******</params_file のサンプル>*********

    open(newunit=file_unit,file=trim(params_file))
      read(file_unit,nml=simulation)
      read(file_unit,nml=visualization)
      read(file_unit,nml=fluid_property)
    close(file_unit)

    write(output_unit,nml=simulation)
    write(output_unit,nml=visualization)
    write(output_unit,nml=fluid_property)

    Read_done = .true.
  end subroutine params__read


  function params__get_string( variable )
    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
    character(len=STRING_LENGTH_MAX) :: params__get_string  !! その値
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が文字列の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。

    call ut__assert( Read_done, &
                     'params_m(180): Read params file first.' )

    select case (variable)
      case                 ('Slicedata_tag')   ! 断面ファイル名に使う
        params__get_string = Slicedata_tag
      case default
        call ut__message( '? arg = ', variable ) ! 想定外
        call ut__fatal( 'params_m(187): case error.' )
    end select
  end function params__get_string

end module params_m
