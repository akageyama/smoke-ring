!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.22
!
!  シミュレーションパラメータ
!
!  @note 
!     パラメーターはnamelistを使ってファイルから読み込む
!
!  @note 
!     先頭一文字だけが大文字になっている変数例えば Read_done などは
!     このモジュールを名前空間とする変数を意味する。つまりモジュール
!     外からはアクセスできないがモジュール内のルーチン関数からは
!     アクセスできるものである。
!     このようにスコープの広い変数を乱用するとバグの温床になるが
!     この程度の小さなプログラムであれば問題ないであろう。
!
!  @note
!     ファイル番号10番をparams__readで使っている。
!
!  @note
!    namelistファイルの内容を変更する場合は:
!      (1) このファイルの少し下の行にあるnamelist文を書き換える。
!      (2) params__readを書き換える。
!      (3) namelist__get_double, _integer等の対応する行も書き換える。
!
!  @note
!    namelist文中のスラッシュで囲まれた名称（たとえば/simulation/）
!    や、namelist変数名（たとえばTotal_nloop）はnamelistファイル
!    の中での記述と対応していなければいけない。
!  

module params_m
  use constants_m  ! 定数定義
  use ut_m         ! ユーティリティ
  implicit none    ! 暗黙の型宣言無効化。必須
  private ! このモジュール内の変数・ルーチン等はデフォルトで非公開
  public :: & ![routines]
            params__get_double,   &
            params__get_integer,  &
            params__get_logical,  &
            params__read,         &
            params__get_string

  logical, save :: Read_done = .false.  ! 読み込みが済んだか否か

  integer(SI), parameter :: STRING_LENGTH_MAX = 200  ! 文字列長
                                  ! 足りなくなったら大きくする。

  integer(SI) :: Total_nloop      ! 一度のジョブで計算するループ回数
  integer(SI) :: Slicedata_nskip  ! 何ステップに一度、断面データを書き出すか
  character(len=STRING_LENGTH_MAX) :: Slicedata_tag  ! 断面データファイル名用
  real(DR) :: Viscosity, Kappa    ! 粘性率と熱拡散率
  logical  :: Debug               ! デバッグ出力用フラグ

  namelist /simulation/     Total_nloop
  namelist /visualization/  Slicedata_nskip,  Slicedata_tag
  namelist /fluid_property/ Viscosity, Kappa
  namelist /flags/          Debug


contains

  function params__get_double(variable)
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が倍精度浮動小数点数の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。
    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
    real(DR) :: params__get_double            !! その値

    call ut__assert(Read_done, &
                    '<params__get_double> Read params file first.')

    select case (variable)
      case                 ('Kappa')     ! 熱拡散率
        params__get_double = Kappa
      case                 ('Viscosity') ! 粘性率
        params__get_double = Viscosity
      case default                       ! そんなnamelist変数は想定外
        call ut__message('? arg = ', variable)
        call ut__fatal('<params__get_double> not in the params?')
    end select
  end function params__get_double


  function params__get_integer(variable)
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が整数の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。
    character(len=*), intent(in) :: variable    !! 問い合わせ変数の名前
    integer(SI) :: params__get_integer          !! その値

    call ut__assert(Read_done, &
                    '<params__get_integer> Read params file first.')

    select case (variable)
      case                  ('Slicedata_nskip')  ! 何ステップごとに
        params__get_integer = Slicedata_nskip    ! 断面をディスクに書き出すか
      case                  ('Total_nloop')      ! シミュレーションジョブ
        params__get_integer = Total_nloop        ! の実行最大ループ数
      case default                               ! 想定外
        call ut__message('? arg = ', variable) 
        call ut__fatal('<params__get_integer> not in the params?')
    end select
  end function params__get_integer


  function params__get_logical(variable)
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が論理値の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。
    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
    logical :: params__get_logical            !! その値

    call ut__assert(Read_done, &
                    '<params__get_logical> Read params file first.')

    select case (variable)
      case                  ('Debug')  ! デバッグモードか否か
        params__get_logical = Debug
      case default                     ! 想定外
        call ut__message('? arg = ', variable)
        call ut__fatal('<params__get_logical> not in the params?')
    end select
  end function params__get_logical


  subroutine params__read
    !! namelistファイルをディスクから読み込む。
    !! ファイル名はコマンド第一引数。
    !!
    !! ファイル番号10番をここで使っている。他の場所で
    !! 10番を使う（開きっぱなしにする）場合は問題だが、
    !! その場所でもここのようにopenした後、closeしていれば
    !! 特に問題ではない。
    !!
    !! namelistデータファイルの内容を変更する場合は
    !! 以下のread文も適宜変更すること。
    !!
    character(len=STRING_LENGTH_MAX) :: params_file

    call ut__assert(command_argument_count()==1, &
                    "Usage: smoke_ring param_file")
    call get_command_argument(1,params_file)

    !*******<params_file のサンプル>*********
    ! &simulation      Total_nloop = 2000 /
    ! &visualization   Slicedata_nskip  = 100, Slicedata_tag = '_data_slice' /
    ! &fluid_property  Viscosity = 3.0e-2, Kappa = 3.e-2 /
    ! &flags           Debug = .false. /
    !*******</params_file のサンプル>*********

    open(10,file=trim(params_file))
      read(10,nml=simulation)
      read(10,nml=visualization)
      read(10,nml=fluid_property)
      read(10,nml=flags)
    close(10)

    write(6,nml=simulation)
    write(6,nml=visualization)
    write(6,nml=fluid_property)
    write(6,nml=flags)

    Read_done = .true.
  end subroutine params__read


  function params__get_string(variable)
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! この関数は問い合わせ変数が文字列の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。
    character(len=*), intent(in) :: variable  !! 問い合わせ変数の名前
    character(len=STRING_LENGTH_MAX) :: params__get_string  !! その値

    call ut__assert(Read_done, &
                    '<params__get_string> Read params file first.')

    select case (variable)
      case                 ('Slicedata_tag')   ! 断面ファイル名に使う
        params__get_string = Slicedata_tag
      case default
        call ut__message('? arg = ', variable) ! 想定外
        call ut__fatal('<params__get_string> not in the params?')
    end select
  end function params__get_string

end module params_m
