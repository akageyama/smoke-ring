!*******************************************************************
!> author: Akira Kageyama
!  date: 2020.01.15
!
!  Fortran定数と数学定数
!
!@note 定数であることが目で見てわかりやすいように全ての文字を
!      大文字で書く。コンパイラには無意味であるので、
!      siとかSiといった変数を使わないように注意。

module constants_m
  implicit none

  ! << f90 constants >>
  integer, parameter :: SI = selected_int_kind(8)   !! 単精度整数種別値
  integer, parameter :: DI = selected_int_kind(16)  !! 倍精度整数種別値
  integer, parameter :: SR = selected_real_kind(6)  !! 単精度実数種別値
  integer, parameter :: DR = selected_real_kind(12) !! 倍精度実数種別値

  ! << Mathematical constants >>
  real(DR), parameter :: PI = 3.1415926535897932_DR !! 円周率
  real(DR), parameter :: TWOPI = PI*2               !! 円周率の2倍
end module constants_m
