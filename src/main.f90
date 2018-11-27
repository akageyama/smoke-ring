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
  use params_m      ! parameters
  use debug_m       ! for debugging
  use grid_m        ! grid mesh
  use field_m       ! field operators and operations
  use slicedata_m   ! generate 2-d sliced data
  use solver_m      ! 4th order runge-kutta integration method
  use job_m         ! job monitor
  implicit none

  integer(DI) :: nloop
  real(DR) :: dt, time

  type(field__fluid_t) :: fluid

  call params__read
  call grid%initialize
  call solver__initialize(fluid)
  call slicedata__initialize

  time = 0.0_DR
  nloop = 0

  call solver__diagnosis(nloop,time,fluid)

  dt = solver__set_time_step(nloop,fluid)

  do while(job__karte%state=="fine")
    call debug__print("running. nloop=",nloop)
    call solver__advance(time,dt,fluid)
    dt = solver__set_time_step(nloop,fluid)
    nloop = nloop + 1
    if (nloop>=params__get_integer('Total_nloop')) call job__karte%set("loop_max")
    call solver__diagnosis(nloop,time,fluid)
    call slicedata__write(nloop,time,fluid)
  end do

  call job__finalize(nloop)
end program main_m
