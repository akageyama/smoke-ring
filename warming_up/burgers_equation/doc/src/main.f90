!*******************************************************************
!> author: Akira Kageyama
!  date: 2020.01.15
!
!  バーガース方程式を差分法+ルンゲ・クッタ積分法で解き、可視化する。
!
!  神戸大学情報知能工学科の講義 "HPC" （B3対象）用サンプルコード
! 
!### 目的
!  この後に説明する3次元Smoke-Ringシミュレーションコードを理解するため。
!
!  3次元Smoke-Ringシミュレーションコードでは
!  基本方程式は違うものの、アルゴリズム（差分法+ルンゲ・クッタ積分法）
!  とシミュレーションコードの構造が同じである。
! 
!### 実行方法
!  cd src ; make      
!     

program main
  use constants_m  ! 定数の読み込み
  use ut_m         ! ユーティリティライブラリ
  use namelist_m   ! namelist機能を使ったパラメータ読み込み
  use rk4_m        ! Runge-Kutta積分法
  implicit none

  real(DR), dimension(:), allocatable :: xpos
           ! 格子点の位置（x座標）の配列  配列サイズはnx
  real(DR), dimension(:), allocatable :: psi
           ! 求める振幅の配列   配列サイズはnx
  real(DR), dimension(:), allocatable :: dpsi01, dpsi02,  &
                                         dpsi03, dpsi04
           ! 4段のRunge-Kutta法に必要となる4つの作業用配列
           ! 1段の作業での被積分関数（振幅psi）の微小な増分量に相当
  integer(SI) :: i, nx
  integer(DI) :: nloop             ! 時間積分の回数
  integer(DI) :: nloop_max = 1000  ! 一度の計算ジョブでの積分回数最大値
  real(DR) :: dx, dt, time, x      ! 格子間隔、時間刻み、時刻、x座標
  real(DR), parameter :: ONE_SIXTH = 1.0_DR / 6.0_DR
                         ! Runge-Kutta法で必要となる定数
                         ! 毎回この割り算をするのは演算の
                         ! 無駄なのでこうして定数として保存する
  real(DR), parameter :: CFL_FACTOR = 0.4_DR
                         ! 時間発展を追跡する計算機シミュレーションでは
                         ! 一般に、時間刻み幅dtが小さいと、効率が悪い。
                         ! 実時間で同じ時刻に到達するまでに必要となる
                         ! 積分回数が多くなるからである。

                         ! だが、ここで採用しているRunge-Kutta法のような
                         ! 陽的な時間積分アルゴリズムではdtを大きく
                         ! しすぎると計算が破綻（発散）する。
                         !
                         ! その発散条件をCFL (Courant-Friedrichs-Lewy)条件
                         ! という。
                         !
                         ! CFL条件はある定数の比例するという形で書ける
                         ! 場合が多い。その定数をCFL_FACTORという。
                         !
                         ! CFL_FACTORは、
                         !   (1) アルゴリズム（ここでは二次精度空間
                         !       中心差分法と4次精度Runge-Kutta時間積分
                         !       の組み合わせ）
                         !   (2) 基本方程式（ここではバーガース方程式）
                         !   (3) 方程式のパラメータ（ここではバーガース
                         !       方程式に含まれる粘性係数）
                         ! で決まり、その値を理論的、または数値的に
                         ! 高精度で求めることが原理的には可能である。
                         ! 
                         ! しかしながら、CFL_FACTORを高精度で求めること
                         ! は実際上、余り意味がない。（その値をたとえば
                         ! 0.4から0.45にしてもシミュレーションの進行
                         ! 速度が(45/4)倍になるだけである。）
                         ! 
                         ! ここでは半経験的にこのCFL_FACTORを設定した。
                         !
                         ! 対象方程式（バーガース方程式）やアルゴリズム
                         ! を変更したらこの値も調整する必要がある。

  !
  !    1    2    3    4                                    nx-1  nx
  !    !----!----!----!----!----!--             --!----!----!----!
  !         !               \  /                                 !
  !         !                dx = TWOPI / (nx-2)                 !
  !    x=-PI!<---------------------- TWOPI --------------------->!x=+PI
  !         !                                                    !
  !         !                                                    !
  !  --!----!                                                !---!--
  !  nx-1  nx                                                1   2
  !
  ! 格子点とその番号割当の模式図
  !   * 格子番号は1で始まり、nxまで。
  !   * 周期境界条件を仮定している。周期の長さは2*pi
  !   * 2番の格子点とnx番の格子点が実際は同じ空間位置を指す。
  !   * 同様に1番の格子点と(nx-1)番の格子点が同じ空間位置を指す。

  call namelist__read
     ! Fortranのnamelistという機能を使って
     ! シミュレーションの数値パラメータを読み込む。
     ! ここで読み込むパラメータは2つ
     !  (1) Nx : 格子間隔
     !  (2) Diffusion_coeff : 粘性率に相当する拡散係数
     ! 
     ! この程度であればわざわざnamelistを使わなくてもいいが、
     ! もっと大規模なシミュレーションになると多数の入力パラメータ
     ! が必要になる。
     ! 
     ! 定数を設定するconstantsモジュールに直接パラメータを
     ! 書き込むのは得策ではない。なぜならばパラメーターを
     ! 変更するたびに毎回コンパイルする必要が生じるからである。
     ! 
     ! 入力パラメーターは人間にとって読み書きのしやすい
     ! テキストデータで渡すべきである。
     ! 
     ! テキストデータをプログラムに渡す時にはパラメーターの
     ! 名前とその値のペアをわかりやすい形で表現できるのが望ましい。
     !
     ! その仕組みがnamelistである。
     ! 

  nx = namelist__get_integer('Nx')  ! 文字列をキーにして値を取り出す
  dx = TWOPI / (nx-2)               ! 格子間隔。上の図を見よ。
  dt = dx**2 / namelist__get_double('Diffusion_coeff') * CFL_FACTOR
                                    ! CFL条件

  allocate(xpos(nx),psi(nx))   ! 配列の割当
  allocate(dpsi01(nx))
  allocate(dpsi02(nx))
  allocate(dpsi03(nx))
  allocate(dpsi04(nx))

  xpos(:) = 0.0_DR    ! ゼロで初期化。allocate後の配列の値は
  psi(:)  = 0.0_DR    ! ゼロになっているとは限らないので注意。
  dpsi01(:) = 0.0_DR  ! 配列の初期化忘れに起因する混乱（ゼロ
  dpsi02(:) = 0.0_DR  ! だと思って計算した配列に実は非ゼロの
  dpsi03(:) = 0.0_DR  ! ランダムな値が入っていて計算がおかしく
  dpsi04(:) = 0.0_DR  ! なること）はシミュレーションではよくある。

  do i = 1 , nx
    xpos(i) = -PI + dx*(i-2)  ! 格子点位置の設定。上の図を参照せよ。
  end do

  do i = 1 , nx
    x = xpos(i)
    psi(i) = 0.8_DR + 0.2_DR*cos(x)  ! 初期振幅。自由に設定してよい。
  end do
    ! バーガース方程式の性質から振幅の大きい（つまりpsiの値が
    ! 大きい）ところほど移流速度が速い。そのため波の「突っ立ち」
    ! 現象が生じる。
    !
    ! 完全に垂直に突っ立った波は（勾配が無限になるので） 
    ! 差分法では表現できないが、バーガース方程式に含まれる
    ! 粘性（拡散）項が急な勾配の発生を抑える。
    !
    ! 粘性係数が大きすぎると波がすぐに拡散して消えてしまう。
    ! 
    ! 粘性は小さい方が（突っ立ちが見えて）面白いが、小さすぎる
    ! と突っ立ちのために計算が発散してしまう。実験するとよい。
    ! （粘性係数はnamelistで渡している。params.namelistファイル。）
    !
    ! できるだけ粘性を小さくするにはどうしたらよいか？
    ! 格子点数（nx）を大きくすれば良い。急な勾配であってもその
    ! 曲線の中に十分多くの格子点があれば差分が計算できるからである。
    ! これも実験するとよい。

  call ut__message('initial check: nx = ', nx)
    ! 標準出力に文字列と整数値を書き出す。
    ! このような出力はよく使うのでutモジュールに入れた。
  call ut__message('initial check: dx = ', dx)
    ! 同じサブルーチン名で呼び出しているが、こちらは倍精度浮動小数点数
    ! であるdxを書き出すことに注意。多重定義である。

  time = 0.0_DR  ! シミュレーション時刻の設定

  call iSave ! 初期条件の振幅（psi）をディスクに書き出す。
             ! 小文字のiで始まるサブルーチン・関数は
             ! 内部副プログラム、という命名規約を使っている。
             ! つまり "call iなんとか" と書かれていたら、
             ! 呼び出されている "iなんとか" はそのすぐ下の
             ! contains文の後ろに定義があることが分かるので
             ! 見つけやすい。

  do nloop = 1 , nloop_max  ! シミュレーションのメインループ
    !--< Runge-Kutta step 1 >--!
    dpsi01(:) = rk4__step('1st',dt,dx,psi)  ! ルンゲ・クッタ1段目
    call iBoundary_condition(dpsi01)        ! 境界条件の設定
                                            ! 小文字のiで始まっ
                                            ! ているので、内部副
                                            ! プログラム、つまり
                                            ! 下のcontains文の
                                            ! 後で定義されている

    !--< Runge-Kutta step 2 >--!
    dpsi02(:) = rk4__step('2nd',dt,dx,psi,dpsi01)    ! 2段目
    call iBoundary_condition(dpsi02)        ! 境界条件の設定

    !--< Runge-Kutta step 3 >--!
    dpsi03(:) = rk4__step('3rd',dt,dx,psi,dpsi02)    ! 3段目
    call iBoundary_condition(dpsi03)        ! 境界条件の設定

    !--< Runge-Kutta step 4 >--!
    dpsi04(:) = rk4__step('4th',dt,dx,psi,dpsi03)    ! 4段目
    call iBoundary_condition(dpsi04)        ! 境界条件の設定

    time = time + dt  ! 時間の更新
                      ! 【注意】バーガース方程式は時刻tに陽には
                      !   依存しない（方程式にtが直接現れない）ので、
                      !   このように4段の積分が終了した後にdtだけ
                      !   時間を進めても問題ない。
                      !   だが、もしも解くべき基本方程式が時刻tに陽に
                      !   依存する場合には、4段の積分過程の途中で
                      !   時刻をdt/2だけ進める必要がある。詳しくは
                      !   ルンゲ・クッタのアルゴリズムを見よ。

    psi(:) = psi(:) + ONE_SIXTH*(dpsi01(:)      &
                              +2*dpsi02(:)      &
                              +2*dpsi03(:)      &
                                +dpsi04(:))
                      ! ここでは教科書に載っている古典的な4段4次の
                      ! ルンゲ・クッタアルゴリズムをそのまま実装
                      ! している。この方法ではこのように作業配列（dpsi）
                      ! が4つ必要である。同じ4次のルンゲ・クッタ法でも
                      ! 作業配列を少なくするアルゴリズムがある。大規模
                      ! なシミュレーションなどでメモリを節約する必要が
                      ! ある場合は、それらの手法を使うべきである。

    if ( mod(nloop,4)==0 ) then
      call iSave  ! Save the profile to the disk.
         ! gnuplotを使ったアニメーション表示のためにディスクに
         ! 振幅を書き出す。毎ステップ書き出すとディスク容量を
         ! 圧迫する割に視覚的な効果はそれほど向上しない。ここで
         ! は4ステップごとに書き出しているがこの数字は適当に決めた。
         ! 格子点数nxの値などを変更する場合はこの数字も調整が
         ! 必要であろう。
    end if
  end do

contains

  subroutine iBoundary_condition(psi)
    real(DR), dimension(nx), intent(inout) :: psi
    ! 周期境界条件  上の説明用コメント図を参照せよ。
    psi(1)    = psi(nx-1)
    psi(nx)   = psi(2)
  end subroutine iBoundary_condition

  subroutine iSave
    integer(SI), save :: counter = 0
    integer(SI) :: i
    ! ディスクへの書き出し
    open(10,file="output.data" // '.' // ut__i2c3(counter))
      ! ファイルオープン
      ! スラッシュ記号2つ連続は文字列連結演算子
      ! ut__i2c3はutモジュールで定義された整数（i）を3文字
      ! に変換する関数
      do i = 1, nx
         write(10,*) xpos(i), psi(i)  ! gnuplotのフォーマット準拠
                                      ! つまりxの値とyの値を空白で区切る
      end do
    close(10)
    counter = counter + 1
    ! ファイル名に整数連番を不可する。その数字のインクリメント
    call ut__message(" Data saved at nloop, time = ", nloop, time)
    ! これも多重定義。文字列に整数、浮動小数点数をつなげて出力
  end subroutine iSave

end program main
