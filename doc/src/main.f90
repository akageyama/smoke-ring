!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   渦輪 (smoke ring) の形成シミュレーション
! 
!   神戸大学情報知能工学科の講義 "HPC" （B3対象）用サンプルコード
!  
!   ### 形状
!     直方体領域。3次元周期境界条件。カーテシアン座標。
!  
!   ### 計算手法
!     空間離散化は2次中心差分法。時間積分は4次ルンゲ・クッタ法。
!  
!   ### 実行方法
!     (1) cd src
!     (2) make
!     (3) cd ../slice_grapher
!     (4) make
!!<
program main_m
  use constants_m  !! 定数
  use field_m      !! スカラー場・ベクトル場
  use fluid_m      !! 流体場の構造体定義
  use grid_m       !! 格子点情報
  use job_m        !! ジョブ管理
  use kutimer_m    !! 時間測定モジュール ;call kutimer__('main  ','sample')
  use mpiut_m      !! mpi関係ユーティリティ
  use parallel_m   !! MPI並列化
  use params_m     !! パラメータ
  use solver_m     !! ナビエ・ストークス方程式ソルバ
  use ut_m         !! ユーティリティ
  use vis2d_m      !! 断面可視化
  implicit none    !! 暗黙の型宣言無効化。必須

  integer :: nloop   ! シミュレーションのループカウンタ
  real(DR) :: dt, time   ! 時間刻み幅と時刻
  type(fluid_t) :: fluid ! 流体場データの構造体
  type(vis2d_t) :: vis2d

                                                 call kutimer__start('main  ')
  call Params%read                              ;call kutimer__('main  ','params')
    !! パラメーターの読み込み。paramsの後に続くアンダースコア
    !! 二つは、これがparamsモジュールの中にあるサブルーチンの
    !! 呼び出しであることを意味している。paramsモジュールの
    !! 名前はparams_mである。params_mはparams.f90にある。
  call Job%initialize                           ;call kutimer__('main  ','job  i')
    !! シミュレーションジョブの初期化
    !! されている。冒頭のPが大文字なのはこれがグローバル変数
    !! であることを示唆している。（コンパイラは大文字と小文字を
    !! 区別しないが。）
  call Grid%initialize                          ;call kutimer__('main  ','grid i')
    !! gridモジュールの初期化。
    !! パーセント記号はメンバアクセス演算子。
    !! ここでは構造体のメンバー関数の呼び出しをしている。
    !! gridモジュール（grid_m）はgrid.f90で定義されている。
  call Solver%initialize( fluid )               ;call kutimer__('main  ','solv i')
    !! solverジュール（solver_m）の初期化。
  call vis2d%initialize                         ;call kutimer__('main  ','vis  i')
    !! 可視化モジュール（vis2d_m）の初期化。

  time = 0.0_DR  !! 時刻の初期化。単位は秒。
  nloop = 0      !! ループカウンタの初期化。

  call vis2d%draw( time, nloop, fluid )
    !! このモジュールはシミュレーション領域の断面図をSVGで出力する。

  call Solver%diagnosis( nloop, time, fluid )   ;call kutimer__('main  ','solv d')
    !! solverモジュールで定義されているdiagnosis（診断）
    !! サブルーチンを呼び出す。医者が患者を診るのがdiagnosis
    !! である。そこでの診断結果はJob.carte
    !! にセットする。carteはカルテである。

  dt = Solver%set_time_step( nloop, fluid )     ;call kutimer__('main  ','set dt')
    !! 時間刻み幅 dt の決定。dtはCFL条件を満足するように決めるが、
    !! CFL条件は流体の状態に流体の状態に依存して変化する。
    !! たとえば、流体の一部が高温になると、そこでの音速が速くなり、
    !! 音速によって決まるCFL条件が厳しくなる（つまりdtが小さくなる）
    !! ここでは初期状態における流体の状態に基づいてdtが決まる

  do while( Job%karte == "fine" )               ;call kutimer__count
    !! このシミュレーションのメインループである。ジョブカルテが
    !! 「健康 (fine)」状態である限りシミュレーションを続行する。 
    call Solver%advance( time, dt, fluid )      ;call kutimer__('main  ','solv a')
      !! ナビエ・ストークス方程式に基づいて流体 (fluid) の状態を
      !! 一時刻ステップ dt だけ進める。
    dt = Solver%set_time_step( nloop, fluid )   ;call kutimer__('main  ','set dt')
      !! 流体の状態が変わったのでCFL条件に基づき時間刻み幅dt
      !! を設定し直す。
      !! 厳密に言えば毎ステップこの再設定をしているわけではなく、
      !! このsolver__set_time_stepルーチンの冒頭で判断し、
      !! 数十ステップに一度だけ実際には変更を行うようなskip操作
      !! をしている。CFL条件に基づいた計算は時間がかかるが、
      !! 毎ステップdtを精密に調整する必要はないからである。

    nloop = nloop + 1  !! ループカウンタのインクリメント
    call Solver%diagnosis( nloop, time, fluid ) ;call kutimer__('main  ','solv d')
      !! 診断。異常があればjob.carteにセットする。
    call vis2d%draw( time, nloop, fluid )
      !! シミュレーション領域の断面図をSVGで出力する。
    if ( nloop >= Params%get_integer( 'Total_nloop' ) ) then
      Job%karte = "loop_max"
      !! あらかじめparamsモジュールで設定されたループカウンタの
      !! 上限値に達したらジョブを停止する。
    end if
  end do
                                                 call kutimer__end('main  ')
                                                 call kutimer__print
  call Job%finalize( nloop )                    
    !! ジョブの後始末。MPI終了処理を含む。
end program main_m
