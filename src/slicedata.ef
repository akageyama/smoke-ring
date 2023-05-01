!*******************************************************************
!> author: Akira Kageyama
!  license: MIT
!  date: 2020.01.22
!
!  断面データの作成とディスクへの出力
!
!  書き出されたデータはslice_grapherディレクトリにあるプログラムで
!  可視化される。
!
!  @note 
!    ここで書き出す断面データは単精度浮動小数点数である。可視化には
!    単精度で十分である。
!
!  @note
!    サブルーチンmake_single_precision_fieldでは
!    fieldモジュールで定義されたcurlを計算するルーチンを呼び出している。
!    ここで.curl.などの演算子を使うと記述が簡潔になるが、
!    現在はこれをコメントアウトしている。Fortranコンパイラが
!    自己定義演算子を問題なく使えるのであれば
!    .curl.などを使うようにコメントを外せばよい。
!    詳しくはfieldモジュールを見よ。

module slicedata_m
  use ut_m      ! ユーティリティ。constantsモジュールは間接的にuseされる
  use field_m   ! 流体場
  use params_m  ! パラメータ
  use debug_m   ! デバッグ関係
  use solver_m  ! ナビエ・ストークス方程式ソルバ
  implicit none ! 暗黙の型宣言無効化。必須
  private ! このモジュール内の変数・ルーチン等はデフォルトで非公開
  public :: slicedata__initialize,  &
            slicedata__write

  ! 2次元単精度実数配列
  real(SR), dimension(:,:), allocatable :: Slice_vx  ! 速度のx成分
  real(SR), dimension(:,:), allocatable :: Slice_vy  ! 速度のy成分
  real(SR), dimension(:,:), allocatable :: Slice_vz  ! 速度のz成分
  real(SR), dimension(:,:), allocatable :: Slice_ps  ! 圧力
  real(SR), dimension(:,:), allocatable :: Slice_en  ! エンストロフィ
                             ! Enstrophy とは渦度の二乗
                             ! 渦度は速度のcurl

  logical, save :: Initialize_done = .false.    ! モジュール初期化済か否か
  integer(SI), parameter :: FILE_SLICEDATA = 20 ! 断面データ出力ファイル番号


contains


  subroutine make_single_precision_field(vel,ps)
    !! シミュレーションで計算した速度場と圧力場
    !! のx-z平面での断面データを求める。
    !! その際、倍精度実数から単精度実数に型変換する。
    !!
    !! ここではx-z平面で書き出しているが、
    !! x-y平面での断面データなど、同様にとることが可能である。
    !!
    !! サブルーチンmake_single_precision_fieldでは
    !! x-z面に平行な断面をとっている。その際、
    !! その断面位置は
    !!     j = slice_j = NY / 2
    !! としている。NYが偶数の時、
    !! この断面は厳密に言えばy=0にはなっていない。
    !! なぜならgridモジュールできめたy方向の格子点の
    !! 定義によればy=0の面はj=NY/2とj=NY/2+1の2つの面
    !! の中間にあるからである。正確にy=0面上での可視化
    !! をしたいのであればこの2つの面上でのデータ
    !! の平均値をディスクに書き出すべきである。
    !! 
    !! [バグ]
    !!   NYが奇数のときにちょうどy=0面に対応するのは
    !!     j = slice_j = NY / 2 + 1 
    !!   である。偶数のときにもこれでよい。
    type(field__vector3d_t),       intent(in) :: vel !! 速度場（3D）
    real(DR), dimension(NX,NY,NZ), intent(in) :: ps  !! 圧力場（3D）

    integer(SI) :: slice_j = NY / 2    ! 断面をとるy座標の格子点位置

    type(field__vector3d_t)       :: vor       ! vorticity、渦度
    real(DR), dimension(NX,NY,NZ) :: enstrophy ! 渦度の2乗

!>        vor = .curl.vel
!>  enstrophy = vor.dot.vor
          vor = operator_curl(vel)
    enstrophy = operator_dot_product(vor,vor)

    Slice_vx = real(    vel%x(:,slice_j,:),SR)
    Slice_vy = real(    vel%y(:,slice_j,:),SR)
    Slice_vz = real(    vel%z(:,slice_j,:),SR)
    Slice_ps = real(       ps(:,slice_j,:),SR)
    Slice_en = real(enstrophy(:,slice_j,:),SR)

    call debug__print('called slicedata/make_single_precision_field.')
      ! デバッグモードがonの時には毎回メッセージを標準出力に
      ! 書き出す。コードが完成し、product runの段階に入ったら
      ! デバッグモードをoffにすれば、書き出しは抑制される。
      ! この行をコメントアウトする必要はない。
  end subroutine make_single_precision_field


!
! Private  これより上が非公開の関数・ルーチン
!===================================================
! Public   これより下が公開する関数・ルーチン
!


  subroutine slicedata__initialize
    !! このモジュールの初期化
    !!
    !! 流体の場（fluid）の配列は決め打ち（つまりallocatableにしない）で
    !! 最初から3次元配列を確保している一方、ここではSlice_??配列を
    !! アロケータブルにしているのは、このslicedataモジュールは
    !! シミュレーションには必須ではないためである。
    !! 場合によってはこのslicedataモジュールを使わずに
    !! シュミレーションすることも考えられる。
    !! 例えば断面ではなく三次元の可視化をする時など。
    !! そのような場合は、このslicedataモジュールは実際には
    !! 呼びたさないであろう。そのような時に備えて以下のように
    !! allocatableにしておけばメモリーを無駄遣いしない。
    allocate(Slice_vx(NX,NZ),   &
             Slice_vy(NX,NZ),   &
             Slice_vz(NX,NZ),   &
             Slice_ps(NX,NZ),   &
             Slice_en(NX,NZ))

    call debug__print('Slice data allocated.')
      ! デバッグモードがonの時には毎回メッセージを標準出力に
      ! 書き出す。コードが完成し、product runの段階に入ったら
      ! デバッグモードをoffにすれば、書き出しは抑制される。
      ! この行をコメントアウトする必要はない。

    open(FILE_SLICEDATA,                                &
         file=trim(params__get_string('Slicedata_tag')),  &
         form='unformatted')
      ! 断面データを書き出すファイルをオープン

    Initialize_done = .true.  ! 初期化終了のフラグをonにする

    call debug__print('called slicedata__initlilize')
  end subroutine slicedata__initialize


  subroutine slicedata__write(nloop,time,fluid)
    !! 断面データのディスクへの書き出し
    integer(DI),          intent(in) :: nloop !! 現在のループカウンタ
    real(DR),             intent(in) :: time  !! 現在の時刻
    type(field__fluid_t), intent(in) :: fluid !! 現在の流体データ

    type(field__vector3d_t) :: vel 
      ! 流れ速度ベクトル場。いまはナビエ・ストークス方程式の
      ! 基本変数を質量フラックスにしているのでこの流れ速度
      ! ベクトル場は基本変数ではなく、基本変数から計算される
      ! 二次的な量である。

    if ( params__get_integer('Slicedata_nskip') <= 0 ) return
                                      ! 何らかの理由で断面データの
                                      ! 出力をしないときには
                                      ! parameter中のSlicedata_nskip
                                      ! の値を負にする。

    if ( mod(nloop,params__get_integer('Slicedata_nskip')) /= 0 ) return
                                      ! parameter中のSlicedata_nskip
                                      ! の値が例えば10の時には
                                      ! 10ステップおきに断面データを計算し、
                                      ! ディスクに書き出す。

    call ut__assert(Initialize_done,"<slicedata__write> Forgot init?")
                                      ! モジュールの初期化終了確認

    call solver__get_subfield(fluid,vel)
                                      ! 流体の基本変数から速度場を求める

    call make_single_precision_field(vel,fluid%pressure)
                                      ! 断面データを作成

    write(FILE_SLICEDATA) nloop, real(time,SR),  &
                          Slice_vx, Slice_vy, Slice_vz,  &
                          Slice_ps, Slice_en
                                      ! ディスクへの書き出し
                                      ! 時刻も単精度に変換している

    call ut__message('#slice data saved at ', nloop, time)
                                      ! ここはデバッグモードがoffでも
                                      ! 書き出すようにしている。
                                      ! 断面データはそれほど頻繁に
                                      ! 書き出さない（この行まで到達しない）
                                      ! のでこの出力がたくさんあって
                                      ! 邪魔になることはあまりないと
                                      ! 予想できるためである。
    call debug__print('called slicedata__write.')
  end subroutine slicedata__write

end module slicedata_m
