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
!    src/constants.f90
!-------------------------------------------------------------------
module constants_m
  implicit none

  ! << f90 constants >>
  integer, parameter :: SI = selected_int_kind(6)
  integer, parameter :: DI = selected_int_kind(15)
  integer, parameter :: SR = selected_real_kind(6)
  integer, parameter :: DR = selected_real_kind(15)

  ! << Mathematical constants >>
  real(DP), parameter :: PI = 3.1415926535897932_DP
  real(DP), parameter :: TWOPI = PI*2

  ! << Job karte >>
  integer, parameter :: KARTE_FINE      =  0
  integer, parameter :: KARTE_TIME_OUT  =  1
  integer, parameter :: KARTE_LOOP_MAX  =  2
  integer, parameter :: KARTE_OVERFLOW  =  3
  integer, parameter :: KARTE_UNDERFLOW =  4

  ! << I/O files >>
  integer, parameter :: FILE_TEMPORAL     = 10
  integer, parameter :: FILE_RESTART      = 80
  integer, parameter :: FILE_STANDARD_OUT = 06
  integer, parameter :: FILE_SLICEDATA    = 51

  ! << Grid Size >>
  integer, parameter :: NX =  92
  integer, parameter :: NY =  32
  integer, parameter :: NZ =  32
  ! integer, parameter :: NX = 152
  ! integer, parameter :: NY =  52
  ! integer, parameter :: NZ =  52

  ! << Box Size >>
  real(DP), parameter :: XMIN = -1.5_DP
  real(DP), parameter :: XMAX = +1.5_DP
  real(DP), parameter :: YMIN = -0.5_DP
  real(DP), parameter :: YMAX = +0.5_DP
  real(DP), parameter :: ZMIN = -0.5_DP
  real(DP), parameter :: ZMAX = +0.5_DP

  ! << Used for strings >>
  integer, parameter :: TAG_STRING_LENGTH_MAX = 100

end module constants_m
