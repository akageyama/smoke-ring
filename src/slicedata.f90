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
!    src/slicedata.f90
!-------------------------------------------------------------------

module slicedata_m
  use ut_m
  use field_m
  use params_m
  use debug_m
  use solver_m
  implicit none
  private
  public :: slicedata__initialize,  &
            slicedata__write

  ! - 2-D single precision real arrays.
  real(SR), dimension(:,:), allocatable :: Slice_vx  ! x-comp. of velocity
  real(SR), dimension(:,:), allocatable :: Slice_vy  ! y-comp.
  real(SR), dimension(:,:), allocatable :: Slice_vz  ! z-comp.
  real(SR), dimension(:,:), allocatable :: Slice_ps  ! Pressure
  real(SR), dimension(:,:), allocatable :: Slice_en  ! Enstrophy

  logical, save :: Initialize_done = .false.
  integer(SI), parameter :: FILE_SLICEDATA = 20

contains


  subroutine make_single_precision_field(vel,ps)
    type(field__vector3d_t),       intent(in) :: vel
    real(DR), dimension(NX,NY,NZ), intent(in) :: ps

    integer(SI) :: slice_j = NY / 2

    type(field__vector3d_t)       :: vor   ! vorticity
    real(DR), dimension(NX,NY,NZ) :: enstrophy

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
  end subroutine make_single_precision_field


!
! Private
!===============
! Public
!


  subroutine slicedata__initialize
    allocate(Slice_vx(NX,NZ),   &
             Slice_vy(NX,NZ),   &
             Slice_vz(NX,NZ),   &
             Slice_ps(NX,NZ),   &
             Slice_en(NX,NZ))

    call debug__print('Slice data allocated.')

    open(FILE_SLICEDATA,                                &
         file=trim(params__get_string('Slicedata_tag')),  &
         form='unformatted')

    Initialize_done = .true.

    call debug__print('called slicedata__initlilize')
  end subroutine slicedata__initialize


  subroutine slicedata__write(nloop,time,fluid)
    integer(DI),          intent(in) :: nloop
    real(DR),             intent(in) :: time
    type(field__fluid_t), intent(in) :: fluid

    type(field__vector3d_t) :: vel

    if ( params__get_integer('Slicedata_nskip') <= 0 ) return
                                      ! Set zero or negative integer
                                      ! when you don't want to
                                      ! save any slice data.

    if ( mod(nloop,params__get_integer('Slicedata_nskip')) /= 0 ) return


    call ut__assert(Initialize_done,"<slicedata__write> Forgot init?")

    call solver__get_subfield(fluid,vel)

    call make_single_precision_field(vel,fluid%pressure)

    write(FILE_SLICEDATA) nloop, real(time,SR),  &
                          Slice_vx, Slice_vy, Slice_vz,  &
                          Slice_ps, Slice_en

    call ut__message('#slice data saved at ', nloop, time)
    call debug__print('called slicedata__write.')
  end subroutine slicedata__write

end module slicedata_m
