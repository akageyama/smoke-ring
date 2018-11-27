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
  integer, parameter :: SI = selected_int_kind(8)
  integer, parameter :: DI = selected_int_kind(16)
  integer, parameter :: SR = selected_real_kind(6)
  integer, parameter :: DR = selected_real_kind(12)

  ! << Mathematical constants >>
  real(DR), parameter :: PI = 3.1415926535897932_DR
  real(DR), parameter :: TWOPI = PI*2

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
end module constants_m
