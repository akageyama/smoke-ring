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
!    src/job.f90
!-------------------------------------------------------------------

module job_m
  use constants_m
  use ut_m
  implicit none
  private
  public :: & !<variable>
            job__karte
  public :: & !<routines>
            job__finalize

  type, public :: job__karte_t
    character(len=20) :: state = "fine"
  contains
    procedure :: set => job__karte_set
  end type job__karte_t

  type(job__karte_t) :: job__karte


contains


  subroutine job__finalize(nloop)
    integer(DI), intent(in) :: nloop

    select case (trim(job__karte%state))
      case ("fine", "loop_max")
        call ut__message('#',"Successfully finished.")
      case ("time_out")
        call ut__message('-',"Time out at nloop = ", nloop)
      case ("over_flow")
        call ut__message('%',"Overflow at nloop = ", nloop)
      case ("negative_anormaly")
        call ut__message('%',"Underflow at nloop = ",nloop)
      case default
        call ut__message('?',"Stopped at nloop = ",  nloop)
    end select
  end subroutine job__finalize


  subroutine job__karte_set(self, state_)
    class(job__karte_t), intent(out) :: self
    character(len=*), intent(in) :: state_
    select case (trim(state_))
      case ("fine")
        self%state = "fine"
      case ("time_out")
        self%state = "time_out"
      case ("loop_max")
        self%state = "loop_max"
      case ("over_flow")
        self%state = "over_flow"
      case ("negative_anormaly")
        self%state = "negative_anormaly"
      case default
        call ut__fatal("<job__karte_set> case error.")
    end select
  end subroutine job__karte_set

end module job_m
