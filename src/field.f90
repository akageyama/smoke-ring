!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.22
!
!  流体場データ構造体
!
!  @note コードを簡潔にするため.curl.や.div.などの
!        ベクトル解析演算子をいったんは定義したが、いまは
!        それらの演算子を使う部分はコメントアウトしている。
!        一部のFortranコンパイラでこの部分の挙動が
!        怪しかったからである。
!        演習室のFortranコンパイラでは大丈夫かもしれないが未確認。
!
!  @note このバージョンでは古典的な関数呼び出しで
!        curlやdivなどを実行しているが、コメントを付け替える
!        だけで演算子（.curl.等）が使えるはず。
!
!  @note 配列演算を多用している。つまり一行で書かれている部分も
!        実際は3重do loopで書かれような大量の演算をしているところが
!        多い。このコードをOpenMP化する時には、そのような部分を
!        3重do loopに展開して書き直す必要がある。
!

module field_m
  use constants_m  !! 定数定義
  use grid_m       !! 格子点
  implicit none    !! 暗黙の型宣言無効化。必須

  public  
    !! 通常はデフォルトを非公開（private）とするが、
    !! このモジュール内の変数・ルーチン等はデフォルトで公開。
  private :: & ![assignments]
             assignment_real_to_fluid,           &
             assignment_real_to_vector
  public  :: & ![operators]
             operator_cross_product,             &
             operator_curl,                      &
             operator_div,                       &
             operator_dot_product,               &
             operator_energyintegral,            &
             operator_fluid_add,                 &
             operator_fluid_times_real,          &
             operator_laplacian_scalar,          &
             operator_laplacian_vector,          &
             operator_real_times_fluid,          &
             operator_real_times_vector,         &
             operator_scalar_times_vector,       &
             operator_scalarintegral,            &
             operator_vector_add,                &
             operator_vector_divby_scalar,       &
             operator_vector_times_real,         &
             operator_vector_times_scalar
  private :: & ![routines]
             boundary_condition_fluid,           &
             boundary_condition_scalar,          &
             boundary_condition_vector

  interface field__boundary_condition
    !! 境界条件呼び出しルーチンの多重定義
    !! 境界条件を設定する変数の種類によって
    !! 実際に使用するルーチンが違うが、
    !! 呼び出し側では統一した名前でcallする。
    !! コンパイラは引数の型で使用するルーチンを
    !! 判断する。
    module procedure boundary_condition_fluid,  &
                     boundary_condition_scalar, &
                     boundary_condition_vector
  end interface

  !--- << Types >> ---!

  type field__vector3d_t
    !! 3次元ベクトル場構造体
    !! ここでは配列のサイズを決めているが、
    !! 実行時に不定にしたい場合はallocatable
    !! は配列を使えば良い。しミュレーション
    !! コード全体にサイズ（行数）がそれほど
    !! 多くない今のような場合は、配列サイズ
    !! （=シミュレーションの格子点数）を
    !! 変更する度にコンパイルしてもたいした
    !! 時間はかからないのでこのように
    !! 決め打ちにしても問題ない。
    real(DR), dimension(NX,NY,NZ) :: x  !! x成分
      !! 倍精度浮動小数点数（double real, DR）の
      !! 3次元配列、という意味。念の為。
    real(DR), dimension(NX,NY,NZ) :: y  !! y成分
    real(DR), dimension(NX,NY,NZ) :: z  !! z成分
  end type field__vector3d_t

  type field__fluid_t
    !! このシミュレーションコードで最も大事な
    !! 変数（構造体）。流体の状態を保持する。
    real(DR), dimension(NX,NY,NZ) :: pressure  !! 圧力場
    real(DR), dimension(NX,NY,NZ) :: density   !! 質量密度場
    type(field__vector3d_t)       :: flux      !! 質量フラックス
    !! 質量フラックス (flux) と速度場 (velocity_vector)
    !!    flux = density * velocity_vector
    !! という関係がある。速度場を基本変数にしても問題ない。
    !! 単にこのシミュレーションで解く基本方程式
    !! ナビエ・ストークス方程式）の基本変数をどちらで
    !! 表現するか、の違いである。
  end type field__fluid_t

  !--- << Operators >> ---!

! interface operator(.curl.)
!    !! ベクトル解析のcurl演算子
!    module procedure operator_curl
! end interface
!
! interface operator(.div.)
!    !! ベクトル解析のdivergence演算子
!    module procedure operator_div
! end interface
!
! interface operator(.energyintegral.)
!    !! 全エネルギーを計算（体積積分）する演算子
!    module procedure operator_energyintegral
! end interface
!
! interface operator(.scalarintegral.)
!    !! 任意のスカラー場の体積積分をする演算子
!    module procedure operator_scalarintegral
! end interface
!
! interface operator(.laplacian.)
!    !! ラプラシアン演算子
!    !! スカラー場とベクトル場用の2つの多重定義
!    module procedure operator_laplacian_scalar
!    module procedure operator_laplacian_vector
! end interface
!
! interface operator(.x.)
!    !! ベクトル解析の外積演算子
!    module procedure operator_cross_product
! end interface
!
! interface operator(.dot.)
!    !! ベクトル解析の内積演算子
!    module procedure operator_dot_product
! end interface
!
! interface operator(+)
!    !! 構造体全要素の足し算を+記号で書けるように定義
!    module procedure operator_fluid_add
!    module procedure operator_vector_add
! end interface
!
! interface operator(/)
!    !! ベクトル場の3成分をあるスカラー場て割り算する
!    !! 操作は何度か出てくる（例えば質量フラックスfluxから
!    !! 速度場velocity_vectorを求めるときに
!    !! velocity_vector = flux / mass_density
!    !! という割り算が必要である）この計算を
!    !! スラッシュ記号一つで書けるように定義
!    module procedure operator_vector_divby_scalar
! end interface
!
! interface operator(*)
!    !! 各種構造体に掛け算記号が使えるように定義
!    module procedure operator_integer_times_fluid
!    module procedure operator_fluid_times_integer
!    module procedure operator_fluid_times_real
!    module procedure operator_real_times_fluid
!    module procedure operator_real_times_vector
!    module procedure operator_scalar_times_vector
!    module procedure operator_vector_times_real
!    module procedure operator_vector_times_scalar
! end interface
!
! interface assignment(=)
!    !! 各種構造体に代入記号が使えるように定義
!    module procedure assignment_real_to_fluid
!    module procedure assignment_real_to_vector
! end interface


contains


  subroutine assignment_real_to_fluid(fluid,real)
    !! 流体構造体に実数を代入。
    !! @note 
    !!   0にセットするときによく使う。
    type(field__fluid_t), intent(out) :: fluid
    real(DR),             intent(in)  :: real

    fluid%pressure(:,:,:) = real
    fluid%density(:,:,:)  = real
    fluid%flux%x(:,:,:)   = real
    fluid%flux%y(:,:,:)   = real
    fluid%flux%z(:,:,:)   = real
  end subroutine assignment_real_to_fluid


  subroutine assignment_real_to_vector(vector,real)
    !! ベクトル場に実数を代入。
    !! @note 
    !!   0にセットするときによく使う。
    type(field__vector3d_t), intent(out) :: vector
    real(DR),                intent(in)  :: real

    vector%x(:,:,:) = real
    vector%y(:,:,:) = real
    vector%z(:,:,:) = real
  end subroutine assignment_real_to_vector


  subroutine boundary_condition_fluid(fluid)
    !! 流体構造体の境界条件設定
    type(field__fluid_t), intent(inout) :: fluid

    call boundary_condition_scalar(fluid%pressure)
    call boundary_condition_scalar(fluid%density)
    call boundary_condition_vector(fluid%flux)
  end subroutine boundary_condition_fluid


  subroutine boundary_condition_scalar(scalar)
    !! スカラー場の境界条件設定
    real(DR), dimension(NX,NY,NZ), intent(inout) :: scalar

    scalar( 1,:,:) = scalar(NX-1,:,:)
    scalar(NX,:,:) = scalar(   2,:,:)

    scalar(:, 1,:) = scalar(:,NY-1,:)
    scalar(:,NY,:) = scalar(:,   2,:)

    scalar(:,:, 1) = scalar(:,:,NZ-1)
    scalar(:,:,NZ) = scalar(:,:,   2)
  end subroutine boundary_condition_scalar


  subroutine boundary_condition_vector(vec)
    !! ベクトル場の境界条件設定
    type(field__vector3d_t), intent(inout) :: vec

    vec%x( 1,:,:) = vec%x(NX-1,:,:)    !-- yz-plane --!
    vec%y( 1,:,:) = vec%y(NX-1,:,:)
    vec%z( 1,:,:) = vec%z(NX-1,:,:)
    vec%x(NX,:,:) = vec%x(   2,:,:)
    vec%y(NX,:,:) = vec%y(   2,:,:)
    vec%z(NX,:,:) = vec%z(   2,:,:)

    vec%x(:, 1,:) = vec%x(:,NY-1,:)    !-- zx-plane --!
    vec%y(:, 1,:) = vec%y(:,NY-1,:)
    vec%z(:, 1,:) = vec%z(:,NY-1,:)
    vec%x(:,NY,:) = vec%x(:,   2,:)
    vec%y(:,NY,:) = vec%y(:,   2,:)
    vec%z(:,NY,:) = vec%z(:,   2,:)

    vec%x(:,:, 1) = vec%x(:,:,NZ-1)    !-- xy-plane --!
    vec%y(:,:, 1) = vec%y(:,:,NZ-1)
    vec%z(:,:, 1) = vec%z(:,:,NZ-1)
    vec%x(:,:,NZ) = vec%x(:,:,   2)
    vec%y(:,:,NZ) = vec%y(:,:,   2)
    vec%z(:,:,NZ) = vec%z(:,:,   2)
  end subroutine boundary_condition_vector


  function operator_cross_product(a,b)
    !! ベクトル場の外積
    type(field__vector3d_t), intent(in) :: a, b
    type(field__vector3d_t) :: operator_cross_product

    operator_cross_product%x = (a%y)*(b%z) - (a%z)*(b%y)
    operator_cross_product%y = (a%z)*(b%x) - (a%x)*(b%z)
    operator_cross_product%z = (a%x)*(b%y) - (a%y)*(b%x)
  end function operator_cross_product


  function operator_curl(a)
    !! ベクトル場のcurl
    type(field__vector3d_t), intent(in) :: a
    type(field__vector3d_t) :: operator_curl

    integer(SI) :: i, j, k
    real(DR) :: dx1, dy1, dz1

    dx1 = grid%d1%x  !! x方向の偏微分演算用定数
    dy1 = grid%d1%y  !! y方向の偏微分演算用定数
    dz1 = grid%d1%z  !! z方向の偏微分演算用定数

    do k = 2 , NZ-1
      !! 境界上の格子点を飛ばして、シミュレーション領域内部
      !! の格子点上で差分法によりcurlを計算する
      do j = 2 , NY-1
        do i = 2 , NX-1
          operator_curl%x(i,j,k) = dy1*(a%z(i,j+1,k)-a%z(i,j-1,k)) &
                                 - dz1*(a%y(i,j,k+1)-a%y(i,j,k-1))
          operator_curl%y(i,j,k) = dz1*(a%x(i,j,k+1)-a%x(i,j,k-1)) &
                                 - dx1*(a%z(i+1,j,k)-a%z(i-1,j,k))
          operator_curl%z(i,j,k) = dx1*(a%y(i+1,j,k)-a%y(i-1,j,k)) &
                                 - dy1*(a%x(i,j+1,k)-a%x(i,j-1,k))
        end do
      end do
    end do

    call boundary_condition_vector(operator_curl)
      !! 境界の格子点は境界条件ルーチンで設定する
  end function operator_curl


  function operator_div(a)
    !! ベクトル場のdivergence
    type(field__vector3d_t), intent(in)  :: a
    real(DR), dimension(NX,NY,NZ)        :: operator_div

    integer(SI) :: i, j, k
    real(DR) :: dx1, dy1, dz1

    dx1 = grid%d1%x   !! x方向の偏微分演算用定数
    dy1 = grid%d1%y   !! y方向の偏微分演算用定数
    dz1 = grid%d1%z   !! z方向の偏微分演算用定数

    do k = 2 , NZ-1
      !! 境界上の格子点を飛ばして、シミュレーション領域内部
      !! の格子点上で差分法によりdivergenceを計算する
      do j = 2 , NY-1
         do i = 2 , NX-1
           operator_div(i,j,k) = dx1*(a%x(i+1,j,k)-a%x(i-1,j,k)) &
                               + dy1*(a%y(i,j+1,k)-a%y(i,j-1,k)) &
                               + dz1*(a%z(i,j,k+1)-a%z(i,j,k-1))
         end do
      end do
    end do

    call boundary_condition_scalar(operator_div)
      !! 境界の格子点は境界条件ルーチンで設定する
  end function operator_div


  function operator_dot_product(a,b)
    !! ベクトル場の内積
    type(field__vector3d_t), intent(in) :: a, b
    real(DR), dimension(NX,NY,NZ) :: operator_dot_product

    operator_dot_product = a%x*b%x +a%y*b%y + a%z*b%z
      !! 配列演算
      !! @note
      !!    実際にはここで3重do_loopが回っている
      !!    OpenMP化するときにはこの簡潔な
      !!    書き方をやめて3重do_loopに書き直す必要がある。
  end function operator_dot_product


  function operator_energyintegral(a)
    !! 流体の運動エネルギーの体積積分
    type(field__fluid_t), intent(in) :: a
    real(DR)                         :: operator_energyintegral
      !! flow_energy = (1/2) * rho * vel^2 = (1/2) * (massflux)^2 / rho
    real(DR) :: dvol
    real(DR), dimension(NX,NY,NZ) :: flux_sq  !! sq は2乗(squared)を意味する

    dvol = (grid%delta%x)*(grid%delta%y)*(grid%delta%z)
         !! 現在のシュミレーションでは格子間隔はx, y, z それぞれに
         !! 一様であることを仮定している。つまりdx, dy, dzは空間位置に
         !! 依存せず一定である。

!   flux_sq = (a%flux).dot.(a%flux)
    flux_sq = operator_dot_product(a%flux,a%flux)
      !! 質量フラックスの2乗を一時的な配列にセットする
      !! ユーザ定義演算子が使えるコンパイラならば.dot.を
      !! 使った表記のほうが読みやすいであろう。

    operator_energyintegral                                      &
         = 0.5_DR * sum(    flux_sq(2:NX-1,2:NY-1,2:NZ-1)        &
                        / a%density(2:NX-1,2:NY-1,2:NZ-1)        &
                        ) * dvol
      !! ここで配列演算の添字が1からNXではなく2からNX-1などに
      !! 限定されていることに注意。これは体積積分の範囲を計算領域の
      !! 内部に限定していること、つまり境界上の格子点を除いて
      !! 積分していることを意味する。境界上の格子点まで
      !! 入れると重複してカウントしてしまうからである 
  end function operator_energyintegral


  function operator_fluid_add(a,b)
    !! 流体構造体の足し算演算子 
    type(field__fluid_t), intent(in) :: a, b
    type(field__fluid_t) :: operator_fluid_add

    operator_fluid_add%flux%x   = a%flux%x   + b%flux%x
    operator_fluid_add%flux%y   = a%flux%y   + b%flux%y
    operator_fluid_add%flux%z   = a%flux%z   + b%flux%z
    operator_fluid_add%density  = a%density  + b%density
    operator_fluid_add%pressure = a%pressure + b%pressure
  end function operator_fluid_add


  function operator_fluid_times_integer(fluid,integer)
    !! 流体構造体を整数倍するための演算子 
    type(field__fluid_t), intent(in) :: fluid
    integer(SI),          intent(in) :: integer
    type(field__fluid_t) :: operator_fluid_times_integer

    operator_fluid_times_integer%pressure = integer*(fluid%pressure)
    operator_fluid_times_integer%density  = integer*(fluid%density)
    operator_fluid_times_integer%flux%x   = integer*(fluid%flux%x)
    operator_fluid_times_integer%flux%y   = integer*(fluid%flux%y)
    operator_fluid_times_integer%flux%z   = integer*(fluid%flux%z)
  end function operator_fluid_times_integer


  function operator_fluid_times_real(fluid,real)
    !! 流体構造体を実数倍するための演算子 
    type(field__fluid_t), intent(in) :: fluid
    real(DR),             intent(in) :: real
    type(field__fluid_t) :: operator_fluid_times_real

    operator_fluid_times_real%pressure = real*(fluid%pressure)
    operator_fluid_times_real%density  = real*(fluid%density)
    operator_fluid_times_real%flux%x   = real*(fluid%flux%x)
    operator_fluid_times_real%flux%y   = real*(fluid%flux%y)
    operator_fluid_times_real%flux%z   = real*(fluid%flux%z)
  end function operator_fluid_times_real


  function operator_integer_times_fluid(integer,fluid)
    !! 整数に流体構造体を掛け算するための演算子 
    integer(SI),          intent(in) :: integer
    type(field__fluid_t), intent(in) :: fluid
    type(field__fluid_t) :: operator_integer_times_fluid

    operator_integer_times_fluid%pressure = integer*(fluid%pressure)
    operator_integer_times_fluid%density  = integer*(fluid%density)
    operator_integer_times_fluid%flux%x   = integer*(fluid%flux%x)
    operator_integer_times_fluid%flux%y   = integer*(fluid%flux%y)
    operator_integer_times_fluid%flux%z   = integer*(fluid%flux%z)
  end function operator_integer_times_fluid


  function operator_laplacian_scalar(a)
    !! スカラー場のラプラシアン
    real(DR), dimension(NX,NY,NZ), intent(in) :: a
    real(DR), dimension(NX,NY,NZ) :: operator_laplacian_scalar

    integer(SI) :: i, j, k
    real(DR) :: dx2, dy2, dz2

    dx2 = grid%d2%x   !! x方向の2階偏微分演算用定数
    dy2 = grid%d2%y   !! y方向の2階偏微分演算用定数
    dz2 = grid%d2%z   !! z方向の2階偏微分演算用定数

    do k = 2 , NZ-1
      !! 境界上の格子点を飛ばして、シミュレーション領域内部
      !! の格子点上で差分法により計算する
      do j = 2 , NY-1
        do i = 2 , NX-1
          operator_laplacian_scalar(i,j,k)  &
               = dx2*(a(i+1,j,k)-2*a(i,j,k)+a(i-1,j,k))  &
               + dy2*(a(i,j+1,k)-2*a(i,j,k)+a(i,j-1,k))  &
               + dz2*(a(i,j,k+1)-2*a(i,j,k)+a(i,j,k-1))
        end do
      end do
    end do

    call boundary_condition_scalar(operator_laplacian_scalar)
      !! 境界の格子点は境界条件ルーチンで設定する
  end function operator_laplacian_scalar


  function operator_laplacian_vector(a)
    !! ベクトル場のラプラシアン
    type(field__vector3d_t), intent(in) :: a
    type(field__vector3d_t) :: operator_laplacian_vector

    integer(SI) :: i, j, k
    real(DR) :: dx2, dy2, dz2

    dx2 = grid%d2%x   !! x方向の2階偏微分演算用定数
    dy2 = grid%d2%y   !! y方向の2階偏微分演算用定数
    dz2 = grid%d2%z   !! z方向の2階偏微分演算用定数

    do k = 2 , NZ-1
      !! 境界上の格子点を飛ばして、シミュレーション領域内部
      !! の格子点上で差分法により計算する
      do j = 2 , NY-1
        do i = 2 , NX-1
          operator_laplacian_vector%x(i,j,k)  &
               = dx2*(a%x(i+1,j,k)-2*a%x(i,j,k)+a%x(i-1,j,k))  &
               + dy2*(a%x(i,j+1,k)-2*a%x(i,j,k)+a%x(i,j-1,k))  &
               + dz2*(a%x(i,j,k+1)-2*a%x(i,j,k)+a%x(i,j,k-1))
          operator_laplacian_vector%y(i,j,k)  &
               = dx2*(a%y(i+1,j,k)-2*a%y(i,j,k)+a%y(i-1,j,k))  &
               + dy2*(a%y(i,j+1,k)-2*a%y(i,j,k)+a%y(i,j-1,k))  &
               + dz2*(a%y(i,j,k+1)-2*a%y(i,j,k)+a%y(i,j,k-1))
          operator_laplacian_vector%z(i,j,k)  &
               = dx2*(a%z(i+1,j,k)-2*a%z(i,j,k)+a%z(i-1,j,k))  &
               + dy2*(a%z(i,j+1,k)-2*a%z(i,j,k)+a%z(i,j-1,k))  &
               + dz2*(a%z(i,j,k+1)-2*a%z(i,j,k)+a%z(i,j,k-1))
        end do
     end do
    end do

    call boundary_condition_vector(operator_laplacian_vector)
      !! 境界の格子点は境界条件ルーチンで設定する
  end function operator_laplacian_vector


  function operator_real_times_fluid(real,fluid)
    !! 実数に流体場を掛ける演算子
    real(DR),             intent(in) :: real
    type(field__fluid_t), intent(in) :: fluid
    type(field__fluid_t) :: operator_real_times_fluid

    operator_real_times_fluid%pressure = real*(fluid%pressure)
    operator_real_times_fluid%density  = real*(fluid%density)
    operator_real_times_fluid%flux%x   = real*(fluid%flux%x)
    operator_real_times_fluid%flux%y   = real*(fluid%flux%y)
    operator_real_times_fluid%flux%z   = real*(fluid%flux%z)
  end function operator_real_times_fluid


  function operator_real_times_vector(real,vec)
    !! 実数にベクトル場を掛ける演算子
    real(DR),                intent(in) :: real
    type(field__vector3d_t), intent(in) :: vec
    type(field__vector3d_t) :: operator_real_times_vector

    operator_real_times_vector%x = real*(vec%x)
    operator_real_times_vector%y = real*(vec%y)
    operator_real_times_vector%z = real*(vec%z)
  end function operator_real_times_vector


  function operator_scalar_times_vector(scalar,vec)
    !! スカラー場にベクトル場を掛ける演算子
    real(DR), dimension(NX,NY,NZ), intent(in) :: scalar
    type(field__vector3d_t),        intent(in) :: vec
    type(field__vector3d_t)             :: operator_scalar_times_vector

    operator_scalar_times_vector%x = scalar*(vec%x)
    operator_scalar_times_vector%y = scalar*(vec%y)
    operator_scalar_times_vector%z = scalar*(vec%z)
  end function operator_scalar_times_vector


  function operator_scalarintegral(a)
    !! スカラー場の体積積分演算子
    real(DR), dimension(NX,NY,NZ), intent(in) :: a
    real(DR) :: operator_scalarintegral

    real(DR) :: dvol

    dvol = (grid%delta%x)*(grid%delta%y)*(grid%delta%z)
         !! 現在のシュミレーションでは格子間隔はx, y, z それぞれに
         !! 一様であることを仮定している。つまりdx, dy, dzは空間位置に
         !! 依存せず一定である。
    operator_scalarintegral = sum( a(2:NX-1,2:NY-1,2:NZ-1) ) * dvol
      !! ここで配列演算の添字が1からNXではなく2からNX-1などに
      !! 限定されていることに注意。これは体積積分の範囲を計算領域の
      !! 内部に限定していること、つまり境界上の格子点を除いて
      !! 積分していることを意味する。境界上の格子点まで
      !! 入れると重複してカウントしてしまうからである 
  end function operator_scalarintegral


  function operator_vector_add(a,b)
    !! ベクトル場の和の演算子
    type(field__vector3d_t), intent(in) :: a, b
    type(field__vector3d_t) :: operator_vector_add

    operator_vector_add%x = a%x + b%x
    operator_vector_add%y = a%y + b%y
    operator_vector_add%z = a%z + b%z
  end function operator_vector_add


  function operator_vector_divby_scalar(vec,scalar)
    !! ベクトル場の各成分をスカラー場で割る
    type(field__vector3d_t),       intent(in) :: vec
    real(DR), dimension(NX,NY,NZ), intent(in) :: scalar
    type(field__vector3d_t) :: operator_vector_divby_scalar

    operator_vector_divby_scalar%x = (vec%x) / scalar
    operator_vector_divby_scalar%y = (vec%y) / scalar
    operator_vector_divby_scalar%z = (vec%z) / scalar
  end function operator_vector_divby_scalar


  function operator_vector_times_real(vec,real)
    !! ベクトル場の実数倍の演算子
    type(field__vector3d_t), intent(in) :: vec
    real(DR),                intent(in) :: real
    type(field__vector3d_t) :: operator_vector_times_real

    operator_vector_times_real%x = real*(vec%x)
    operator_vector_times_real%y = real*(vec%y)
    operator_vector_times_real%z = real*(vec%z)
  end function operator_vector_times_real


  function operator_vector_times_scalar(vec,scalar)
    !! ベクトル場にスカラー場を掛ける
    type(field__vector3d_t),       intent(in) :: vec
    real(DR), dimension(NX,NY,NZ), intent(in) :: scalar
    type(field__vector3d_t) :: operator_vector_times_scalar

    operator_vector_times_scalar%x = scalar*(vec%x)
    operator_vector_times_scalar%y = scalar*(vec%y)
    operator_vector_times_scalar%z = scalar*(vec%z)
  end function operator_vector_times_scalar

end module field_m

