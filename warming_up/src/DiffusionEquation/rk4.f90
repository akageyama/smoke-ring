!-----------------------------------------------------------------------------
! 200708_LesHouches/src/DiffusionEquation
!                                       Akira Kageyama
!                                       kage@jamstec.go.jp
!                                       Earth Simulator Center, JAMSTEC, Japan
!-----------------------------------------------------------------------------
! rk4.f90
!              2007.08.02: Revised rk4_step.
!              2007.07.20: Created by Akira Kageyama. 
!-----------------------------------------------------------------------------

module rk4
  use constants
  use namelist
  use ut
  implicit none

  private
  public :: rk4__step


contains


!=========================================================
!       Private
!=========================================================

!_______________________________________________________________private__
!
  function diffusion_equation(nx,dx,psi)
    integer, intent(in)                 :: nx
    real(DP), intent(in)                :: dx
    real(DP), dimension(nx), intent(in) :: psi
    real(DP), dimension(nx)             :: diffusion_equation
!_______________________________________________________________________/
!
    integer :: i
    real(DP) :: dx2

    dx2 = namelist__double('Diffusion_coeff')/(dx**2)

    do i = 2 , nx-1
       diffusion_equation(i) = dx2*(psi(i+1)-2*psi(i)+psi(i-1))
    end do

  end function diffusion_equation


!=========================================================
!       Public
!=========================================================


!_______________________________________________________________public___
!
  function rk4__step(nth,dt,dx,psi,dpsi_prev) result(dpsi_new)
    character(len=3), intent(in) :: nth
    real(DP), intent(in) :: dt
    real(DP), intent(in) :: dx
    real(DP), dimension(:), intent(in) :: psi
    real(DP), dimension(size(psi,dim=1)), intent(in), optional :: dpsi_prev
    real(DP), dimension(size(psi,dim=1)) :: dpsi_new
!_______________________________________________________________________/
!
    real(DP), dimension(size(psi,dim=1)) :: psi_

    select case (nth)
    case ('1st')
       dpsi_new(:) = dt*diffusion_equation(size(psi,dim=1),dx,psi)
    case ('2nd')
       psi_(:) = psi(:) + dpsi_prev(:)*0.5_DP
       dpsi_new(:) = dt*diffusion_equation(size(psi,dim=1),dx,psi_)
    case ('3rd')
       psi_(:) = psi(:) + dpsi_prev(:)*0.5_DP
       dpsi_new(:) = dt*diffusion_equation(size(psi,dim=1),dx,psi_)
    case ('4th')
       psi_(:) = psi(:) + dpsi_prev(:)
       dpsi_new(:) = dt*diffusion_equation(size(psi,dim=1),dx,psi_)
    case default
       call ut__fatal('<rk4> Invalid step.')
    end select

  end function rk4__step

end module rk4
