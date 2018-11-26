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
!    warming_up/burgers_equation/main.f90
!-------------------------------------------------------------------

program main
  use constants_m
  use ut_m
  use namelist_m
  use rk4_m
  implicit none

  real(DR), dimension(:), allocatable :: xpos   ! grid position, size=nx
  real(DR), dimension(:), allocatable :: psi    ! size=nx
  real(DR), dimension(:), allocatable :: dpsi01, dpsi02,  &   ! used for RK4.
                                         dpsi03, dpsi04
  integer(SI) :: i, nx
  integer(DI) :: nloop
  integer(DI) :: nloop_max = 1000
  real(DR) :: dx, dt, time, x
  real(DR), parameter :: ONE_SIXTH = 1.0_DR / 6.0_DR
  real(DR), parameter :: CFL_FACTOR = 0.4_DR


  !    1    2    3    4                                    NX-1  NX
  !    !----!----!----!----!----!--             --!----!----!----!
  !         !               \  /                                 !
  !         !                dx = TWOPI / (nx-2)                 !
  !    x=-PI!<---------------------- TWOPI --------------------->!x=+PI
  !         !                                                    !
  !         !                                                    !
  !  --!----!                                                !---!--
  !  NX-1  NX                                                1   2


  call namelist__read

  nx = namelist__get_integer('Nx')
  dx = TWOPI / (nx-2)  ! See above fig.
  dt = dx**2 / namelist__get_double('Diffusion_coeff') * CFL_FACTOR

  allocate(xpos(nx),psi(nx))
  allocate(dpsi01(nx))
  allocate(dpsi02(nx))
  allocate(dpsi03(nx))
  allocate(dpsi04(nx))

  xpos(:) = 0.0_DR
  psi(:)  = 0.0_DR
  dpsi01(:) = 0.0_DR
  dpsi02(:) = 0.0_DR
  dpsi03(:) = 0.0_DR
  dpsi04(:) = 0.0_DR

  do i = 1 , nx
    xpos(i) = -PI + dx*(i-2)  ! Grid location
  end do

  do i = 1 , nx
    x = xpos(i)
    psi(i) = 0.8_DR+0.2_DR*cos(x)     ! Initial condition
  end do

  call ut__message('initial check: nx = ', nx)
  call ut__message('initial check: dx = ', dx)

  time = 0.0_DR

  call iSave     ! Save the initial condition profile to the disk.

  do nloop = 1 , nloop_max
    !--< Runge-Kutta step 1 >--!
    dpsi01(:) = rk4__step('1st',dt,dx,psi)
    call iBoundary_condition(dpsi01)

    !--< Runge-Kutta step 2 >--!
    dpsi02(:) = rk4__step('2nd',dt,dx,psi,dpsi01)
    call iBoundary_condition(dpsi02)

    !--< Runge-Kutta step 3 >--!
    dpsi03(:) = rk4__step('3rd',dt,dx,psi,dpsi02)
    call iBoundary_condition(dpsi03)

    !--< Runge-Kutta step 4 >--!
    dpsi04(:) = rk4__step('4th',dt,dx,psi,dpsi03)
    call iBoundary_condition(dpsi04)

    time = time + dt
    psi(:) = psi(:) + ONE_SIXTH*(dpsi01(:)      &
                              +2*dpsi02(:)      &
                              +2*dpsi03(:)      &
                                +dpsi04(:))

    if ( mod(nloop,4)==0 ) then
      call iSave  ! Save the profile to the disk.
    end if
  end do

contains

  subroutine iBoundary_condition(psi)
    real(DR), dimension(nx), intent(inout) :: psi

    psi(1)    = psi(nx-1)
    psi(nx)   = psi(2)
  end subroutine iBoundary_condition

  subroutine iSave
    integer(SI), save :: counter = 0
    integer(SI) :: i

    open(10,file="output.data" // '.' // ut__i2c3(counter))
      do i = 1 , nx
         write(10,*) xpos(i), psi(i)
      end do
    close(10)
    counter = counter + 1
    call ut__message(" Data saved at nloop, time = ", nloop, time)
  end subroutine iSave

end program main
