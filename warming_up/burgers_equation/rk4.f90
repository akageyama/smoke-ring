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
!    warming_up/burgers_equation/rk4.f90
!-------------------------------------------------------------------

module rk4_m
  use constants_m
  use namelist_m
  use ut_m
  implicit none

  private
  public :: rk4__step


contains

  function burgers_equation(nx,dx,psi)
    integer(SI), intent(in) :: nx
    real(DR), intent(in) :: dx
    real(DR), dimension(nx), intent(in) :: psi
    real(DR), dimension(nx) :: burgers_equation

    integer(SI) :: i
    real(DR) :: dx1, dx2

    dx1 = 1.0_DR / (2*dx)
    dx2 = namelist__get_double('Diffusion_coeff')/(dx**2)

    do i = 2 , nx-1
       burgers_equation(i) = - psi(i)*dx1*(psi(i+1)-psi(i-1)) &
                             + dx2*(psi(i+1)-2*psi(i)+psi(i-1))
    end do
  end function burgers_equation


!
! Private
!==========
! Public
!


  function rk4__step(nth,dt,dx,psi,dpsi_prev) result(dpsi_new)
    character(len=3), intent(in) :: nth
    real(DR), intent(in) :: dt
    real(DR), intent(in) :: dx
    real(DR), dimension(:), intent(in) :: psi
    real(DR), dimension(size(psi,dim=1)), intent(in), optional :: dpsi_prev
    real(DR), dimension(size(psi,dim=1)) :: dpsi_new

    real(DR), dimension(size(psi,dim=1)) :: psi_

    select case (nth)
    case ('1st')
       dpsi_new(:) = dt*burgers_equation(size(psi,dim=1),dx,psi)
    case ('2nd')
       psi_(:) = psi(:) + dpsi_prev(:)*0.5_DR
       dpsi_new(:) = dt*burgers_equation(size(psi,dim=1),dx,psi_)
    case ('3rd')
       psi_(:) = psi(:) + dpsi_prev(:)*0.5_DR
       dpsi_new(:) = dt*burgers_equation(size(psi,dim=1),dx,psi_)
    case ('4th')
       psi_(:) = psi(:) + dpsi_prev(:)
       dpsi_new(:) = dt*burgers_equation(size(psi,dim=1),dx,psi_)
    case default
       call ut__fatal('<rk4> Invalid step.')
    end select

  end function rk4__step

end module rk4_m
