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
!    src/main.f90
!-------------------------------------------------------------------

program main_m
  use constants_m   ! numerical constants
  use ut_m          ! utility functions
  use namelist_m    ! namelis loader
  use debug_m       ! for debugging
  use grid_m        ! grid mesh
  use field_m       ! field operators and operations
  use slicedata_m   ! generate 2-d sliced data
  use solver_m      ! 4th order runge-kutta integration method
  implicit none

  integer(SI) :: karte=KARTE_FINE
  integer(DI) :: nloop
  real(DR) :: dt, time

  type(field__fluid_t) :: fluid

  call namelist__read
  call grid__initialize
  call solver__initialize(fluid)
  call slicedata__initialize

  time = 0.0_DR
  nloop = 0

  call solver__diagnosis(nloop,time,fluid,karte)

  dt = solver__set_time_step(nloop,fluid)

  do while(karte==KARTE_FINE)
    call debug__print("running. nloop=",nloop)
    call solver__advance(time,dt,fluid)
    dt = solver__set_time_step(nloop,fluid)
    nloop = nloop + 1
    if (nloop >= namelist__get_integer('Total_nloop')) karte = KARTE_LOOP_MAX
    call solver__diagnosis(nloop,time,fluid,karte)
    call slicedata__write(nloop,time,fluid)
  end do

  select case (karte)
    case (KARTE_FINE)
      call ut__message('#',"Successfully finished.")
    case (KARTE_LOOP_MAX)
      call ut__message('=',"Reached max nloop = ", nloop)
    case (KARTE_TIME_OUT)
      call ut__message('-',"Time out at nloop = ", nloop)
    case (KARTE_OVERFLOW)
      call ut__message('%',"Overflow at nloop = ", nloop)
    case (KARTE_UNDERFLOW)
      call ut__message('%',"Underflow at nloop = ",nloop)
    case default
      call ut__message('?',"Stopped at nloop = ",  nloop)
  end select
end program main_m
