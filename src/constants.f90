!*******************************************************************
!> author: Akira Kageyama
!  date: 2020.01.22
!
!  Fortran定数と数学定数
!
!@note 定数であることが目で見てわかりやすいように全ての文字を
!      大文字で書く。コンパイラには無意味であるので注意。
!
!@note シミュレーション領域は直方体と仮定している。
!      x方向の長さXMAX-XMINである。座標系の原点を
!      シミュレーション領域の中心におくためには
!      XMAXとXMIN絶対値を等しくとる。
!
!@note シミュレーションの（x方向の）空間解像度を上げる
!      ためには同じXMAX-XMINに対して格子点数NXを上げれば良い。
!
!@note x,y,z それぞれの方向の空間解像度、
!      つまり格子間隔dx,dy,dzが異なっていても構わないが、
!      その差が極端に大きくしないほうがよい。
!
module constants_m
  implicit none

  logical, parameter :: DEBUG_MODE = .true.

  ! << f90 constants >>
  integer, parameter :: SI = selected_int_kind(8)   ! 単精度整数種別値
  integer, parameter :: DI = selected_int_kind(16)  ! 倍精度整数種別値
  integer, parameter :: SR = selected_real_kind(6)  ! 単精度実数種別値
  integer, parameter :: DR = selected_real_kind(12) ! 倍精度実数種別値

  ! << Mathematical constants >>
  real(DR), parameter :: PI = 3.1415926535897932_DR ! 円周率
  real(DR), parameter :: TWOPI = PI*2               ! 円周率の2倍

  !<< MPI process constants >>!
  integer, parameter :: NPROC_X = 1 ! x方向MPIプロセス数 mkjob.sh でこの行をgrepする
  integer, parameter :: NPROC_Y = 1 ! y方向MPIプロセス数 mkjob.sh でこの行をgrepする 
  integer, parameter :: NPROC_Z = 1 ! z方向MPIプロセス数 mkjob.sh でこの行をgrepする

  !<< Code utility constants >>!
  integer, parameter :: NIL = -huge(1)

  !<< Grid size constants >>!
  integer, parameter :: NXPP = 60   ! PP = Per Process 
  integer, parameter :: NYPP = 20
  integer, parameter :: NZPP = 20
  integer, parameter :: NXPP1 = NXPP + 1  ! PP1 = PP plus one
  integer, parameter :: NYPP1 = NYPP + 1
  integer, parameter :: NZPP1 = NZPP + 1
  integer, parameter :: NX_GLOBAL = NXPP * NPROC_X + 2 ! x方向の全格子点数
  integer, parameter :: NY_GLOBAL = NYPP * NPROC_Y + 2
  integer, parameter :: NZ_GLOBAL = NZPP * NPROC_Z + 2

  ! 格子サイズのメモ
  ! NX_GLOBAL =  60    ! 格子点数 x方向 粗い解像度
  ! NY_GLOBAL =  20    ! 格子点数 y方向 粗い解像度
  ! NZ_GLOBAL =  20    ! 格子点数 z方向 粗い解像度
  ! ---
  ! NX_GLOBAL =  92    ! 格子点数 x方向
  ! NY_GLOBAL =  32    ! 格子点数 y方向
  ! NZ_GLOBAL =  32    ! 格子点数 z方向
  ! ---
  ! NX_GLOBAL = 152    ! 格子点数 x方向 少し高めの解像度
  ! NY_GLOBAL =  52    ! 格子点数 y方向 少し高めの解像度
  ! NZ_GLOBAL =  52    ! 格子点数 z方向 少し高めの解像度

  ! << Box Size >>
  real(DR), parameter :: XMIN = -1.5_DR ! 計算領域範囲 +x  単位はメートル
  real(DR), parameter :: XMAX = +1.5_DR ! 計算領域範囲 -x
  real(DR), parameter :: YMIN = -0.5_DR ! 計算領域範囲 +y
  real(DR), parameter :: YMAX = +0.5_DR ! 計算領域範囲 -y
  real(DR), parameter :: ZMIN = -0.5_DR ! 計算領域範囲 +z
  real(DR), parameter :: ZMAX = +0.5_DR ! 計算領域範囲 -z
end module constants_m
