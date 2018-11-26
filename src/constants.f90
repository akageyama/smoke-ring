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
  real(DR), parameter :: PI = 3.1415926535897932_DR
  real(DR), parameter :: TWOPI = PI*2

  ! << Job karte >>
  integer(SI), parameter :: KARTE_FINE      =  0
  integer(SI), parameter :: KARTE_TIME_OUT  =  1
  integer(SI), parameter :: KARTE_LOOP_MAX  =  2
  integer(SI), parameter :: KARTE_OVERFLOW  =  3
  integer(SI), parameter :: KARTE_UNDERFLOW =  4

  ! << I/O files >>
  integer(SI), parameter :: FILE_TEMPORAL     = 10
  integer(SI), parameter :: FILE_RESTART      = 80
  integer(SI), parameter :: FILE_STANDARD_OUT = 06
  integer(SI), parameter :: FILE_SLICEDATA    = 51

  ! << Grid Size >>
  integer(SI), parameter :: NX =  92
  integer(SI), parameter :: NY =  32
  integer(SI), parameter :: NZ =  32
  ! integer(SI), parameter :: NX = 152
  ! integer(SI), parameter :: NY =  52
  ! integer(SI), parameter :: NZ =  52

  ! << Box Size >>
  real(DR), parameter :: XMIN = -1.5_DR
  real(DR), parameter :: XMAX = +1.5_DR
  real(DR), parameter :: YMIN = -0.5_DR
  real(DR), parameter :: YMAX = +0.5_DR
  real(DR), parameter :: ZMIN = -0.5_DR
  real(DR), parameter :: ZMAX = +0.5_DR

  ! << Used for strings >>
  integer(SI), parameter :: TAG_STRING_LENGTH_MAX = 100

end module constants_m
