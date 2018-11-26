!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp
!-----------------------------------------------------------------------------
module job_m
  implicit none
  private
  public :: &
            JOB__ARRIVED_LOOP_MAX, &
            JOB__ARRIVED_TIME_MAX, &
            JOB__ERROR_NEGATIVE, &
            JOB__ERROR_OVERFLOW, &
            JOB__NO_PROBLEM

  integer(SI), parameter :: JOB__NO_PROBLEM       = 0
                           ! Must be zero. Since
                           ! mpi_allreduce_sum or
                           ! ..._max will be taken.
  integer(SI), parameter :: JOB__ARRIVED_LOOP_MAX = 1
                           ! Others are arbitrary
                           ! integer larger than 0.
  integer(SI), parameter :: JOB__ARRIVED_TIME_MAX = 2
  integer(SI), parameter :: JOB__ERROR_NEGATIVE   = 3
  integer(SI), parameter :: JOB__ERROR_OVERFLOW   = 4
end module job_m
