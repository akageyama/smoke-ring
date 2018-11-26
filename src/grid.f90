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

module grid_m
  use constants_m
  use ut_m
  use debug_m
  implicit none
  private
  public :: & !<< type >>!
            grid__pos_t,  &
            grid__delta_t,  &
            grid__derivative_operator_1st_t,  &
            grid__derivative_operator_2nd_t
  public :: & !<< variable >>!
            grid__pos,  &
            grid__delta,  &
            grid__d1,  &
            grid__d2,  &
            grid__delta_min
  public :: & !<< routine >>!
            grid__initialize

  type grid__pos_t
    real(DR), dimension(NX) :: x
    real(DR), dimension(NY) :: y
    real(DR), dimension(NZ) :: z
  end type grid__pos_t

  type(grid__pos_t) :: grid__pos

  type grid__delta_t
    real(DR) :: x, y, z
  end type grid__delta_t

  real(DR) :: grid__delta_min

  type(grid__delta_t) :: grid__delta

  type grid__derivative_operator_1st_t
    real(DR) :: x, y, z
  end type grid__derivative_operator_1st_t

  type(grid__derivative_operator_1st_t) :: grid__d1

  type grid__derivative_operator_2nd_t
    real(DR) :: x, y, z
  end type grid__derivative_operator_2nd_t

  type(grid__derivative_operator_2nd_t) :: grid__d2


contains

  subroutine grid__initialize
    !
    !  Here we suppose the periodic boundary condition in all directions.
    !
    integer(SI) :: i, j, k
    real(DR) :: dx, dy, dz

    real(DR), parameter :: NEARLY_ZERO = 1.e-10_DR
    !
    ! Periodic boundary condition.
    !
    !    --+-----+-----|                             |-----+-----+---
    !      6     7     8                             1     2     3
    !            |-----+-----+-----+-----+-----+-----+-----|
    !            1     2     3     4     5     6     7     8
    !               |===================================|
    !              XMIN                                XMAX
    !
    dx = (XMAX-XMIN)/(NX-2)  ! from (1.5) to (NX-0.5), see above figure.
    dy = (YMAX-YMIN)/(NY-2)
    dz = (ZMAX-ZMIN)/(NZ-2)

    grid__delta%x = dx
    grid__delta%y = dy
    grid__delta%z = dz
    call debug__print('grid__delta%x = ', grid__delta%x)
    call debug__print('grid__delta%y = ', grid__delta%y)
    call debug__print('grid__delta%z = ', grid__delta%z)

    grid__delta_min = min(grid__delta%x,grid__delta%y,grid__delta%z)
    call debug__print('grid__delta_min = ', grid__delta_min)

    grid__d1%x = 1.0_DR/(2*dx)   ! factor for 1st derivative
    grid__d1%y = 1.0_DR/(2*dy)
    grid__d1%z = 1.0_DR/(2*dz)

    grid__d2%x = 1.0_DR/(dx**2)  ! factor for 2nd derivative
    grid__d2%y = 1.0_DR/(dy**2)
    grid__d2%z = 1.0_DR/(dz**2)

    do i = 1 , NX
      grid__pos%x(i) = XMIN + dx*(real(i,DR)-1.5_DR)
      call debug__print('grid_x: i, x = ', i, grid__pos%x(i))
    end do

    do j = 1 , NY
      grid__pos%y(j) = YMIN + dy*(real(j,DR)-1.5_DR)
      call debug__print('grid_y: j, y = ', j, grid__pos%y(j))
    end do

    do k = 1 , NZ
      grid__pos%z(k) = ZMIN + dz*(real(k,DR)-1.5_DR)
      call debug__print('grid_z: k, z = ', k, grid__pos%z(k))
    end do

    call ut__assert(abs(grid__pos%x( 1)-XMIN+dx/2) +                 &
                    abs(grid__pos%x(NX)-XMAX-dx/2) +                 &
                    abs(grid__pos%y( 1)-YMIN+dy/2) +                 &
                    abs(grid__pos%y(NY)-YMAX-dy/2) +                 &
                    abs(grid__pos%z( 1)-ZMIN+dz/2) +                 &
                    abs(grid__pos%z(NZ)-ZMAX-dz/2)  < NEARLY_ZERO,   &
                   "<grid__initialize> grid min/max inconsistent.")

    call debug__print('called grid__initialize.')
  end subroutine grid__initialize

end module grid_m

