!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.22
!
!  格子点関連情報
!
!  @note 
!     モジュール名と同じgridというpublicな変数（構造体）を
!     定義し、その実体をこのモジュール内に保持している。
!
module grid_m
  use constants_m  ! 定数定義
  use ut_m         ! ユーティリティ
  implicit none    ! 暗黙の型宣言無効化。必須
  private ! このモジュール内の変数・ルーチン等はデフォルトで非公開
! public :: & ![type]
!           grid__pos_t,  &
!           grid__delta_t,  &
!           grid__derivative_operator_1st_t,  &
!           grid__derivative_operator_2nd_t
! public :: & ![variable]
!           grid__pos,  &
!           grid__delta,  &
!           grid__d1,  &
!           grid__d2,  &
!           grid__delta_min
! public :: & ![routine]
!           grid__initialize
  public :: grid
    ! このモジュールの中心変数。これを介して外部と
    ! やり取りする。このグリッド変数の定義 (下記のgrid__t）
    ! と実体（このgrid）が同じ場所（このgrid_mモジュール内）
    ! にあるのは少々変則的かもしれないが、このように規模の
    ! 小さいシミュレーションプログラムではこの方が
    ! むしろわかりやすいかもしれない。

  type grid__pos_t
    !! 格子点位置を収める構造体
    !! posはpositionの意味
    real(DR), dimension(NX) :: x !! x座標
    real(DR), dimension(NY) :: y !! y座標
    real(DR), dimension(NZ) :: z !! z座標
  end type grid__pos_t

  type grid__delta_t
    !! 格子間隔構造体
    real(DR) :: x  !! x方向の格子間隔
    real(DR) :: y  !! y方向の格子間隔
    real(DR) :: z  !! z方向の格子間隔
  end type grid__delta_t

  type grid__derivative_operator_1st_t
    !! 1階微分（差分）をとる時の演算子（定数）
    !! 演算回数の節約のため
    real(DR) :: x  !! x偏微分
    real(DR) :: y  !! y偏微分
    real(DR) :: z  !! z偏微分
  end type grid__derivative_operator_1st_t

  type grid__derivative_operator_2nd_t
    !! 2階微分（差分）をとる時の演算子（定数）
    !! 演算回数の節約のため
    real(DR) :: x   !! x偏微分
    real(DR) :: y   !! y偏微分
    real(DR) :: z   !! z偏微分
  end type grid__derivative_operator_2nd_t

  type, public :: grid__t
    !! 格子点関係のデータを全て収める構造体
    type(grid__pos_t) :: pos                     !! 格子点位置
    real(DR) :: delta_min                        !! 最小の格子間隔
    type(grid__delta_t) :: delta                 !! 格子間隔
    type(grid__derivative_operator_1st_t) :: d1  !! 1階微分演算子定数
    type(grid__derivative_operator_2nd_t) :: d2  !! 2階微分演算子定数
  contains
    procedure :: initialize => grid__initialize  
      !! 初期化関数
      !! 初期化のためのメンバー関数
      !! こうするとgrid%initializeという形でcallできる
  end type grid__t

  type(grid__t) :: grid  ! 実体


contains

  subroutine grid__initialize(self)
    !! gridの初期化
    !! 
    !! ここでは周期境界条件を仮定している。
    !!
    !! 構造体のメンバー関数としてcallするときその
    !! 構造体変数そのものがselfとして自動的に引数にはいる。
    !! たとえば、
    !!     call sampl_grid%initialize
    !! は
    !!     call grid__initialize(sample_grid)
    !! と解釈される。selfという名前でなくても構わない。
    class(grid__t), intent(out) :: self  !! 格子構造体

    integer(SI) :: i, j, k
    real(DR) :: dx, dy, dz  ! 格子間隔

    real(DR), parameter :: NEARLY_ZERO = 1.e-10_DR   ! 十分小さい数
    !
    ! 周期境界条件
    !
    !    --+-----+-----|                             |-----+-----+---
    !      6     7     8                             1     2     3
    !            |-----+-----+-----+-----+-----+-----+-----|
    !            1     2     3     4     5     6     7     8
    !               |===================================|
    !              XMIN                                XMAX
    !
    dx = (XMAX-XMIN)/(NX-2)  ! from (1.5) to (NX-0.5), see above figure.
    dy = (YMAX-YMIN)/(NY-2)  ! ここではxminがi=1とi=2の2つの格子点の
    dz = (ZMAX-ZMIN)/(NZ-2)  ! ちょうど中間に位置すると仮定している。
                             ! burgers_equationのサンプルコードでは
                             ! i=2にXMINが位置していた。両者に実質的な
                             ! 違いはない。

    self%delta%x = dx ! x方向の格子間隔
    self%delta%y = dy ! y方向の格子間隔
    self%delta%z = dz ! z方向の格子間隔

    self%delta_min = min(self%delta%x,self%delta%y,self%delta%z)
      ! 3つの格子間隔の最小値。
      ! CFL条件はこの最小値で決まる。

    self%d1%x = 1.0_DR/(2*dx)   ! 1階差分演算用定数
    self%d1%y = 1.0_DR/(2*dy)   ! dはderivativeを表す
    self%d1%z = 1.0_DR/(2*dz)   ! d1は1階差分の意味

    self%d2%x = 1.0_DR/(dx**2)  ! 2階差分演算用定数
    self%d2%y = 1.0_DR/(dy**2)  ! dはderivativeを表す
    self%d2%z = 1.0_DR/(dz**2)  ! d2は2階差分の意味

    do i = 1 , NX
      ! x方向の格子点位置の設定
      ! XMINはi=1とi=2の中間にある
      self%pos%x(i) = XMIN + dx*(real(i,DR)-1.5_DR)
    end do

    do j = 1 , NY
      ! y方向の格子点位置の設定
      self%pos%y(j) = YMIN + dy*(real(j,DR)-1.5_DR)
    end do

    do k = 1 , NZ
      ! z方向の格子点位置の設定
      self%pos%z(k) = ZMIN + dz*(real(k,DR)-1.5_DR)
    end do

    call ut__assert(abs(self%pos%x( 1)-XMIN+dx/2) +                 &
                    abs(self%pos%x(NX)-XMAX-dx/2) +                 &
                    abs(self%pos%y( 1)-YMIN+dy/2) +                 &
                    abs(self%pos%y(NY)-YMAX-dy/2) +                 &
                    abs(self%pos%z( 1)-ZMIN+dz/2) +                 &
                    abs(self%pos%z(NZ)-ZMAX-dz/2)  < NEARLY_ZERO,   &
                   "<grid__initialize> grid min/max inconsistent?")
      ! シミュレーション領域の範囲を決めるxmin, xmax, ..., zmaxZ
      ! の6個の頂点位置が上で定義した格子点位置と矛盾していない
      ! ことを確認する。万が一この前提が崩れている時はすぐに
      ! シミュレーションを停止する 
  end subroutine grid__initialize

end module grid_m

