!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.15
!
!  古典的4次ルンゲ=クッタ積分法モジュール
!
!  @note これは教育用のコードである。
!        メモリの節約については全く考慮していない。
!

module rk4_m

  use constants_m
  use namelist_m
  use ut_m
  implicit none

  private
  public :: rk4__step


contains

  function burgers_equation(nx,dx,psi)
    !! バーガーズ方程式の2階中心差分
    !! 境界点を除いた内側の格子点を更新する
    !! @note 
    !!   ここで更新していない境界上の格子点は、
    !!   境界条件で更新する必要がある。
    integer(SI), intent(in) :: nx                !! 格子点数
    real(DR), intent(in) :: dx                   !! 格子間隔
    real(DR), dimension(nx), intent(in) :: psi   !! 振幅（主変数）
    real(DR), dimension(nx) :: burgers_equation  !! 積分後の配列を返す

    integer(SI) :: i
    real(DR) :: dx1  ! 一階差分演算子（定数）割り算削減のため定義
    real(DR) :: dx2  ! 二階差分演算子（定数）割り算削減のため定義

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
    !! 4段4次のRunge-Kutta法による1ステップ積分の実行
    !! @note 教科書のアルゴリズムをそのまま実装している。
    !!       メモリを贅沢に使っていることに注意。
    character(len=3), intent(in) :: nth       !! 4段の何番目か
    real(DR), intent(in) :: dt                !! 積分時間刻み
    real(DR), intent(in) :: dx                !! 格子間隔
    real(DR), dimension(:), intent(in) :: psi !! 被積分関数
    real(DR), dimension(size(psi,dim=1)), intent(in), optional :: dpsi_prev
                                              !! 前段の増分
    real(DR), dimension(size(psi,dim=1)) :: dpsi_new
                                              !! 現段の増分
                                              !! 配列で返す

    real(DR), dimension(size(psi,dim=1)) :: psi_   ! 作業配列

    ! 古典的な4段4次のRunge-Kutta積分法
    select case (nth)
    case ('1st') ! 第1段
       dpsi_new(:) = dt*burgers_equation(size(psi,dim=1),dx,psi)
    case ('2nd') ! 第2段
       psi_(:) = psi(:) + dpsi_prev(:)*0.5_DR
       dpsi_new(:) = dt*burgers_equation(size(psi,dim=1),dx,psi_)
    case ('3rd') ! 第3段
       psi_(:) = psi(:) + dpsi_prev(:)*0.5_DR
       dpsi_new(:) = dt*burgers_equation(size(psi,dim=1),dx,psi_)
    case ('4th') ! 第4段
       psi_(:) = psi(:) + dpsi_prev(:)
       dpsi_new(:) = dt*burgers_equation(size(psi,dim=1),dx,psi_)
    case default
       call ut__fatal('<rk4> Invalid step.')
    end select

  end function rk4__step

end module rk4_m
