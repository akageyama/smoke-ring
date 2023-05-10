!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   流体場データ構造体
! 
!   @note 配列演算を多用している。つまり一行で書かれている部分も
!         実際は3重do loopで書かれような大量の演算をしているところが
!         多い。このコードをOpenMP化する時には、そのような部分を
!         3重do loopに展開して書き直す必要がある。
!!<

module field_m
  use constants_m  ! 定数定義
  use grid_m       ! 格子点
  use mpiut_m      ! MPI通信ユーティリティ
  use parallel_m   ! 並列化
  implicit none    ! 暗黙の型宣言無効化。必須
  private
  public :: operator( .curl. ),            &
            operator( .div. ),             &
            operator( .scalarintegral. ),  &
            operator( .laplacian. ),       &
            operator( .x. ),               &
            operator( .dot. ),             &
            operator( + ),                 &
            operator( * ),                 &
            operator( / )
  public :: assignment( = )
  public :: field__boundary_condition

  interface field__boundary_condition
    !! 境界条件呼び出しルーチンの多重定義
    !! 境界条件を設定する変数の種類によって
    !! 実際に使用するルーチンが違うが、
    !! 呼び出し側では統一した名前でcallする。
    !! コンパイラは引数の型で使用するルーチンを
    !! 判断する。
    module procedure boundary_condition_scalar,  &
                     boundary_condition_vector
  end interface 

  !!>
!  type field__scalar_t
!     !この構造体は efpp_alias.list で以下のようにマクロ定義されている
!     !=> "real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1)"
!  end type field__scalar_t
  !!<

  type, public :: field__vector_t
    !! 3次元ベクトル場構造体
    !! ここではコンパイル時に配列のサイズが
    !! 既に決まっているとしているが、そうでない
    !! 場合、つまり実行時に配列サイズを確定
    !! したい場合にはallocatableな配列を使えば良い。
    !!
    !! コード全体にサイズ（行数）がそれほど
    !! 多くない今のような場合は、配列サイズ
    !! （=シミュレーションの格子点数）を
    !! 変更する度にコンパイルしてもたいした
    !! 時間はかからないのでこのように
    !! 決め打ちにしても問題ない。
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: x  !! x成分
      ! 倍精度浮動小数点数（double real, DR）の
      ! 3次元配列、という意味。念の為。
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: y  !! y成分
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: z  !! z成分
  end type field__vector_t


  !--- << Operators >> ---!

  interface operator( .curl. )
     !! ベクトル解析のcurl演算子
     module procedure operator_curl
  end interface
 
  interface operator( .div. )
     !! ベクトル解析のdivergence演算子
     module procedure operator_div
  end interface
 
  interface operator( .scalarintegral. )
     !! 任意のスカラー場の体積積分をする演算子
     module procedure operator_scalarintegral
  end interface
 
  interface operator( .laplacian. )
     !! ラプラシアン演算子
     !! スカラー場とベクトル場用の2つの多重定義
     module procedure operator_laplacian_scalar
     module procedure operator_laplacian_vector
  end interface
 
  interface operator( .x. )
     !! ベクトル解析の外積演算子
     module procedure operator_cross_product
  end interface
 
  interface operator( .dot. )
     !! ベクトル解析の内積演算子
     module procedure operator_dot_product
  end interface
 
  interface operator( + )
     !! 構造体全要素の足し算を+記号で書けるように定義
     module procedure operator_vector_add
  end interface

 interface operator( / )
    !! ベクトル場の3成分をあるスカラー場で割り算する
    !! 操作は何度か出てくる（例えば質量フラックスfluxから
    !! 速度場velocity_vectorを求めるときに
    !! velocity_vector = flux / mass_density
    !! という割り算が必要である）この計算を
    !! スラッシュ記号一つで書けるように定義
    module procedure operator_vector_divby_scalar
 end interface

 interface operator( * )
    !! 各種構造体に掛け算記号が使えるように定義
    module procedure operator_real_times_vector
    module procedure operator_scalar_times_vector
    module procedure operator_vector_times_real
    module procedure operator_vector_times_scalar
 end interface

 interface assignment( = )
    !! 各種構造体に代入記号が使えるように定義
    module procedure assignment_real_to_vector
 end interface


contains


  subroutine assignment_real_to_vector( vector, real )
    !! ベクトル場に実数を代入。
    !! 初期条件で0にセットするときに使う。
    type(field__vector_t), intent(out) :: vector !! ベクトル場
    real(DR), intent(in)  :: real   !! 代入する実数

    vector%x(:,:,:) = real
    vector%y(:,:,:) = real
    vector%z(:,:,:) = real
  end subroutine assignment_real_to_vector


  subroutine boundary_condition_scalar( scalar )
    !! スカラー場の境界条件設定
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(inout) :: scalar !! スカラー場

    !! 周期境界条件の設定
    call mpiut__exchange( Parallel%comm,            &
                          Parallel%periodic_pair,   &
                          scalar )
    call mpiut__barrier( Parallel%comm )
    !! プロセス間通信で領域境界データを交換
    call mpiut__exchange( Parallel%comm,            &
                          Parallel%rank%next,       &
                          scalar )
    call mpiut__barrier( Parallel%comm )

  end subroutine boundary_condition_scalar


  subroutine boundary_condition_vector( vector )
    !! ベクトル場の境界条件設定
    type(field__vector_t), intent(inout) :: vector !! ベクトル場

    !! 周期境界条件設定
    call mpiut__exchange( Parallel%comm,           &
                          Parallel%periodic_pair,  &
                          vector%x,                &
                          vector%y,                &
                          vector%z )
    call mpiut__barrier( Parallel%comm )
    !! プロセス間通信で領域境界データを交換
    call mpiut__exchange( Parallel%comm,           &
                          Parallel%rank%next,      &
                          vector%x,                &
                          vector%y,                &
                          vector%z )
    call mpiut__barrier( Parallel%comm )
  end subroutine boundary_condition_vector


  function operator_cross_product( a, b )
    !! ベクトル場の外積
    type(field__vector_t), intent(in) :: a, b !! 掛けるベクトル場
    type(field__vector_t) :: operator_cross_product !! 外積

    operator_cross_product%x = (a%y)*(b%z) - (a%z)*(b%y)
    operator_cross_product%y = (a%z)*(b%x) - (a%x)*(b%z)
    operator_cross_product%z = (a%x)*(b%y) - (a%y)*(b%x)
  end function operator_cross_product


  function operator_curl( a )
    !! ベクトル場のcurl
    type(field__vector_t), intent(in) :: a !! これのcurlをとる
    type(field__vector_t) :: operator_curl !! curlした結果

    integer :: i, j, k
    real(DR) :: dx1, dy1, dz1

    dx1 = grid%d1%x  ! x方向の偏微分演算用定数
    dy1 = grid%d1%y  ! y方向の偏微分演算用定数
    dz1 = grid%d1%z  ! z方向の偏微分演算用定数

    do k = 1, NZPP
      ! 境界上の格子点を飛ばして、シミュレーション領域内部
      ! の格子点上で差分法によりcurlを計算する
      do j = 1, NYPP
        do i = 1, NXPP
          operator_curl%x(i,j,k) = dy1*(a%z(i,j+1,k)-a%z(i,j-1,k)) &
                                 - dz1*(a%y(i,j,k+1)-a%y(i,j,k-1))
          operator_curl%y(i,j,k) = dz1*(a%x(i,j,k+1)-a%x(i,j,k-1)) &
                                 - dx1*(a%z(i+1,j,k)-a%z(i-1,j,k))
          operator_curl%z(i,j,k) = dx1*(a%y(i+1,j,k)-a%y(i-1,j,k)) &
                                 - dy1*(a%x(i,j+1,k)-a%x(i,j-1,k))
        end do
      end do
    end do

    call field__boundary_condition( operator_curl )
      ! 境界の格子点は境界条件ルーチンで設定する
  end function operator_curl


  function operator_div( a )
    !! ベクトル場のdivergence
    type(field__vector_t), intent(in) :: a  !! これのdivをとる
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1)        :: operator_div !! 結果

    integer :: i, j, k
    real(DR) :: dx1, dy1, dz1

    dx1 = grid%d1%x   ! x方向の偏微分演算用定数
    dy1 = grid%d1%y   ! y方向の偏微分演算用定数
    dz1 = grid%d1%z   ! z方向の偏微分演算用定数

    do k = 1, NZPP
      ! 境界上の格子点を飛ばして、シミュレーション領域内部
      ! の格子点上で差分法によりdivergenceを計算する
      do j = 1, NYPP
         do i = 1, NXPP
           operator_div(i,j,k) = dx1*(a%x(i+1,j,k)-a%x(i-1,j,k)) &
                               + dy1*(a%y(i,j+1,k)-a%y(i,j-1,k)) &
                               + dz1*(a%z(i,j,k+1)-a%z(i,j,k-1))
         end do
      end do
    end do

    call field__boundary_condition( operator_div )
      ! 境界の格子点は境界条件ルーチンで設定する
  end function operator_div


  function operator_dot_product( a, b )
    !! ベクトル場の内積
    type(field__vector_t), intent(in) :: a, b  !! 内積をとるベクトル場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: operator_dot_product !! 計算結果

    operator_dot_product = a%x*b%x +a%y*b%y + a%z*b%z
      ! 配列演算
      !
      ! 実際にはここで3重do_loopが回っている
      ! OpenMP化するときにはこの簡潔な
      ! 書き方をやめて3重do_loopに書き直す必要がある。
  end function operator_dot_product


  function operator_laplacian_scalar( a )
    !! スカラー場のラプラシアン
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: a  !! 入力スカラー場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: operator_laplacian_scalar !! 計算結果

    integer :: i, j, k
    real(DR) :: dx2, dy2, dz2

    dx2 = grid%d2%x   ! x方向の2階偏微分演算用定数
    dy2 = grid%d2%y   ! y方向の2階偏微分演算用定数
    dz2 = grid%d2%z   ! z方向の2階偏微分演算用定数

    do k = 1, NZPP
      ! 境界上の格子点を飛ばして、シミュレーション領域内部
      ! の格子点上で差分法により計算する
      do j = 1, NYPP
        do i = 1, NXPP
          operator_laplacian_scalar(i,j,k)  &
               = dx2*(a(i+1,j,k)-2*a(i,j,k)+a(i-1,j,k))  &
               + dy2*(a(i,j+1,k)-2*a(i,j,k)+a(i,j-1,k))  &
               + dz2*(a(i,j,k+1)-2*a(i,j,k)+a(i,j,k-1))
        end do
      end do
    end do

    call field__boundary_condition( operator_laplacian_scalar )
      ! 境界の格子点は境界条件ルーチンで設定する
  end function operator_laplacian_scalar


  function operator_laplacian_vector( a )
    !! ベクトル場のラプラシアン
    type(field__vector_t), intent(in) :: a  !! 入力ベクトル場
    type(field__vector_t) :: operator_laplacian_vector !! 計算結果

    integer :: i, j, k
    real(DR) :: dx2, dy2, dz2

    dx2 = grid%d2%x   ! x方向の2階偏微分演算用定数
    dy2 = grid%d2%y   ! y方向の2階偏微分演算用定数
    dz2 = grid%d2%z   ! z方向の2階偏微分演算用定数

    do k = 1, NZPP
      ! 境界上の格子点を飛ばして、シミュレーション領域内部
      ! の格子点上で差分法により計算する
      do j = 1, NYPP
        do i = 1, NXPP
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

    call field__boundary_condition( operator_laplacian_vector )
      ! 境界の格子点は境界条件ルーチンで設定する
  end function operator_laplacian_vector


  function operator_real_times_vector( real, vec )
    !! 実数にベクトル場を掛ける演算子
    real(DR), intent(in) :: real !! 掛ける実数
    type(field__vector_t), intent(in) :: vec  !! 流体場
    type(field__vector_t) :: operator_real_times_vector !! 計算結果

    operator_real_times_vector%x = real*(vec%x)
    operator_real_times_vector%y = real*(vec%y)
    operator_real_times_vector%z = real*(vec%z)
  end function operator_real_times_vector


  function operator_scalar_times_vector( scalar, vec )
    !! スカラー場にベクトル場を掛ける演算子
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: scalar  !! スカラー場
    type(field__vector_t), intent(in) :: vec     !! ベクトル場
    type(field__vector_t) :: operator_scalar_times_vector !! 計算結果

    operator_scalar_times_vector%x = scalar*(vec%x)
    operator_scalar_times_vector%y = scalar*(vec%y)
    operator_scalar_times_vector%z = scalar*(vec%z)
  end function operator_scalar_times_vector


  function operator_scalarintegral( a )
    !! スカラー場の体積積分演算子
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: a  !! スカラー場
    real(DR) :: operator_scalarintegral    !! 体積積分結果

    real(DR) :: dvol

    dvol = (grid%delta%x)*(grid%delta%y)*(grid%delta%z)
         ! 現在のシュミレーションでは格子間隔はx, y, z それぞれに
         ! 一様であることを仮定している。つまりdx, dy, dzは空間位置に
         ! 依存せず一定である。
    operator_scalarintegral = sum( a(1:NXPP,  &
                                     1:NYPP,  &
                                     1:NZPP) ) * dvol
      ! ここで配列演算の添字が0:NXPP1等ではなく1:NXPPなどに
      ! 限定されていることに注意。これは体積積分の範囲を計算領域の
      ! 内部に限定していること、つまり境界上の格子点を除いて
      ! 積分していることを意味する。境界上の格子点まで
      ! 入れると重複してカウントしてしまうからである 
  end function operator_scalarintegral


  function operator_vector_add( a, b )
    !! ベクトル場の和の演算子
    type(field__vector_t), intent(in) :: a, b    !! 和をとるベクトル場
    type(field__vector_t) :: operator_vector_add !! 計算結果

    operator_vector_add%x = a%x + b%x
    operator_vector_add%y = a%y + b%y
    operator_vector_add%z = a%z + b%z
  end function operator_vector_add


  function operator_vector_divby_scalar( vec, scalar )
    !! ベクトル場の各成分をスカラー場で割る
    type(field__vector_t), intent(in) :: vec    !! ベクトル場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: scalar !! スカラー場
    type(field__vector_t) :: operator_vector_divby_scalar !! 計算結果

    operator_vector_divby_scalar%x = (vec%x) / scalar
    operator_vector_divby_scalar%y = (vec%y) / scalar
    operator_vector_divby_scalar%z = (vec%z) / scalar
  end function operator_vector_divby_scalar


  function operator_vector_times_real( vec, real )
    !! ベクトル場の実数倍の演算子
    type(field__vector_t), intent(in) :: vec  !! ベクトル場
    real(DR), intent(in) :: real !! 掛ける実数
    type(field__vector_t) :: operator_vector_times_real  !! 計算結果

    operator_vector_times_real%x = real*(vec%x)
    operator_vector_times_real%y = real*(vec%y)
    operator_vector_times_real%z = real*(vec%z)
  end function operator_vector_times_real


  function operator_vector_times_scalar( vec, scalar )
    !! ベクトル場にスカラー場を掛ける
    type(field__vector_t), intent(in) :: vec    !! ベクトル場
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: scalar !! スカラー場
    type(field__vector_t) :: operator_vector_times_scalar  !! 計算結果

    operator_vector_times_scalar%x = scalar*(vec%x)
    operator_vector_times_scalar%y = scalar*(vec%y)
    operator_vector_times_scalar%z = scalar*(vec%z)
  end function operator_vector_times_scalar

end module field_m

