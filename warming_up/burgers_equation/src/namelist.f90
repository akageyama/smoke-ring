!*******************************************************************
!> author: Akira Kageyama
!  date: 2020.01.15
!
!  Fortranのnamelist機能を使い、入力データを読み込み、
!  管理するモジュール
! 
!### 目的
! 
!  namelistデータの一元管理
!
!### 機能
!
!  入力パラメータを読み込み、保持する。要求に応じて値を教える。
!
!@note
!  実行ファイルのコマンドオプションでnamelistファイルを
!  指定すると仮定している。
!
!@note
!  namelistファイルの内容を変更した場合は:
!    (1) 少し下の行にあるこのファイルのnamelist文を書き換える。
!    (2) namelist__readを書き換える。
!    (3) namelist__get_double, _integer等の対応する行も書き換える。
!
!@note
!  浮動小数点数（double）や整数以外のデータをnamelistで受け取る
!  場合は、namelsit__get_logical等、新たに作る必要がある
!     
module namelist_m
  use constants_m
  use ut_m
  implicit none
  private
  public :: &
            namelist__get_double, &
            namelist__get_integer, &
            namelist__read

  logical, save :: Read_done = .false.

  integer(SI) :: Nx
  real(DR) :: Diffusion_coeff

  namelist /data00/ Nx              ! namelistデータファイルを
  namelist /data01/ Diffusion_coeff ! 書き換えたらここも変更する


contains


  function namelist__get_double(variable)
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! その変数が倍精度浮動小数点数の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。
    character(len=*), intent(in) :: variable !! 問い合わせ変数
    real(DR) :: namelist__get_double         !! 保持データをそのまま返す

    call ut__assert(Read_done,  &
                    '<namelist__get_double> Read namelist first.')

    select case (variable)
    case                ('Diffusion_coeff')
       namelist__get_double = Diffusion_coeff
    case default
       call ut__message('? arg = ', variable)
       call ut__fatal('<namelist__get_double> not in the namelist?')
    end select
  end function namelist__get_double


  function namelist__get_integer(variable)
    !! このモジュールの外からの問い合わせに応じてnamelistデータを返す。
    !! その変数が整数の場合。
    !! この関数の前にnamelist__readが呼ばれている必要がある。
    !! この点はassertで確認している。
    character(len=*), intent(in) :: variable
    integer(SI) :: namelist__get_integer

    call ut__assert(Read_done,  &
                    '<namelist__get_integer> Read namelist first.')

    select case (variable)
    case                 ('Nx')
       namelist__get_integer = Nx
    case default
       call ut__message('? arg = ', variable)
       call ut__fatal('<namelist__get_integer> not in the namelist?')
    end select
  end function namelist__get_integer


  subroutine namelist__read
    !! namelistファイルをディスクから読み込む。
    !! ファイル名はコマンド第一引数。
    !! @note ファイル番号10番をここで使っている。他の場所で
    !! 10番を使う（開きっぱなしにする）場合は問題だが、
    !! その場所でもここのようにopenした後、closeしていれば
    !! 特に問題ではない。
    !!
    !! @note namelistデータファイルの内容を変更する場合は
    !! 当然ながら以下のread文も適宜変更すること。
    !!
    integer(SI), parameter :: FILENAME_MAX_LENGTH = 200
                                  ! 200文字もあれば十分だろう。
    character(len=FILENAME_MAX_LENGTH) :: filename

    !---------------------------------------
    ! namelistデータファイルの中身
    ! &data00 Nx = 500 /
    ! &data01 Diffusion_coeff = 0.005 /
    !---------------------------------------
    call ut__assert(command_argument_count()==1, &
                    "Usage: berguers param_file")
    call get_command_argument(1, filename)
    open(10,file=trim(filename))
      read(10,nml=data00)
      read(10,nml=data01)
    close(10)

    write(6,nml=data00)
    write(6,nml=data01)

    Read_done = .true.
  end subroutine namelist__read

end module namelist_m
