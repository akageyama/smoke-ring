!!>
!   author: Akira Kageyama
!   date: 2023.05.05
! 
!   シミュレーションジョブの制御
! 
!   @note 
!      配列演算を多用している。つまり一行で書かれている部分も
!      実際は3重do loopで書かれような大量の演算をしているところが
!      多い。このコードをOpenMP化する時には、そのような部分を
!      3重do loopに展開して書き直す必要がある。
!!<
module job_m
  use constants_m  !! 定数定義
  use ut_m         !! ユーティリティ
  use parallel_m   !! MPI並列化
  implicit none    !! 暗黙の型宣言無効化。必須
  private !! このモジュール内の変数・ルーチン等はデフォルトで非公開

  type :: job_t
    character(len=20) :: karte = "fine"  ! カルテ。初期は「健康」
  contains
    procedure, nopass :: initialize => job__initialize
    procedure, nopass :: finalize => job__finalize
  end type job_t

  type(job_t), public :: Job


contains


  subroutine job__initialize

    call Parallel%initialize
      !! MPI並列化初期化処理。Parallel変数はparallel.efで定義
      !! されている。冒頭のPが大文字なのはこれがグローバル変数
      !! であることを示唆している。（コンパイラは大文字と小文字を
      !! 区別しないが。）
    
  end subroutine job__initialize


  subroutine job__finalize( nloop )
    integer, intent(in) :: nloop !! ループカウンタ
    !! ジョブ終了時の後始末。
    !! (1) 健康状態カルテに応じたメッセージを標準出力に書く
    !! (2) MPI並列化の終了処理

    select case (trim(job%karte))
      case ("fine","loop_max") !! このどちらかであれば、
        call ut__deco_message( "#","Successfully finished." ) 
                               !! # で第2引数の文字列を囲む。
      case ("time out")
        call ut__deco_message( "-","Time out at nloop = ", nloop )
      case ("overflow")
        call ut__deco_message( "%","Overflow at nloop = ", nloop )
      case ("negative anormaly")
        call ut__deco_message( "%","Underflow at nloop = ",nloop )
      case default
        call ut__deco_message( "?","Stopped at nloop = ",  nloop )
    end select

    call Parallel%finalize
      !! MPI並列化終了処理
  end subroutine job__finalize

end module job_m
