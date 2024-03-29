module vis2d_m
  !!>
!   
!    vis2d.ef
!      2023.05.04: Copied from tetrahedral_dynamo project.
!
  !!<
  use constants_m
  use grid_m
  use field_m
  use fluid_m
  use kutimer_m
  use mpiut_m 
  use parallel_m
  use ut_m 
  use vv_color_m
  use vv_sketch_m
  use vv_slice_scalar_m
  use vv_slice_vector_m
  use solver_m
  implicit none
  private
  public :: vis2d__initialize,  &
            vis2d__draw

  integer, parameter :: TAG_NAME_MAX = 100
  integer, parameter :: FILE_NAME_MAX = 200

  type vis2d_plane_xy_t
    integer :: gk_cut
    character(len=TAG_NAME_MAX) :: tag
    type(vv_slice_scalar_t) :: pressure
    type(vv_slice_scalar_t) :: flow_helicity
    type(vv_slice_scalar_t) :: velocity_z
    type(vv_slice_scalar_t) :: enstrophy
    type(vv_slice_vector_t) :: velocity
    type(vv_slice_vector_t) :: vorticity
  contains
    procedure :: initialize => vis2d_plane_xy__initialize
    procedure :: set_data => vis2d_plane_xy__set_data
    procedure :: draw => vis2d_plane_xy__draw
  end type vis2d_plane_xy_t

  type vis2d_plane_xz_t
    integer :: gj_cut
    character(len=TAG_NAME_MAX) :: tag
    type(vv_slice_scalar_t) :: pressure
    type(vv_slice_scalar_t) :: flow_helicity
    type(vv_slice_scalar_t) :: velocity_y
    type(vv_slice_scalar_t) :: enstrophy
    type(vv_slice_vector_t) :: velocity
    type(vv_slice_vector_t) :: vorticity
  contains
    procedure :: initialize => vis2d_plane_xz__initialize
    procedure :: set_data => vis2d_plane_xz__set_data
    procedure :: draw => vis2d_plane_xz__draw
  end type vis2d_plane_xz_t

  type, public :: vis2d_t
    type(vis2d_plane_xy_t) :: z_middle
    type(vis2d_plane_xy_t) :: z_upper
    type(vis2d_plane_xy_t) :: z_lower
    type(vis2d_plane_xz_t) :: y_middle
    type(vis2d_plane_xz_t) :: y_upper
    type(vis2d_plane_xz_t) :: y_lower
  contains
    procedure :: initialize => vis2d__initialize
    procedure :: set_data => vis2d__set_data
    procedure :: draw => vis2d__draw
  end type vis2d_t


  type, private :: vissubfield3d_t
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: enstrophy  !! エンストロフィ（渦度の2乗）
    real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1) :: helicity   !! ヘリシティ
    type(field__vector_t) :: velocity   !! 流れ場
    type(field__vector_t) :: vorticity  !! 渦度（流れ場のcurl）
  contains
    procedure :: update => vissubfield3d__update
  end type vissubfield3d_t


contains

  subroutine vissubfield3d__update( vf, fluid )
    class(vissubfield3d_t), intent(out) :: vf
    type(fluid_t)          , intent(in) :: fluid

    call solver%get_subfield( fluid, vf%velocity )
    vf%vorticity = .curl. vf%velocity              ! curl演算子はfield_mで定義
    vf%helicity = vf%velocity .dot. vf%vorticity   ! ヘリシティ = 流れと渦度の内積
    vf%enstrophy = vf%vorticity .dot. vf%vorticity ! 内積演算子はfield_mで定義
  end subroutine vissubfield3d__update


  subroutine vis2d_plane_xy__draw( plane, t, step )
    class(vis2d_plane_xy_t), intent(inout) :: plane
    real   , intent(in) :: t
    integer, intent(in) :: step

    character(len=TAG_NAME_MAX) :: tas
    character(len=FILE_NAME_MAX) :: filename
    type(vv_sketch__sim_pos_t) :: ll,  &  ! lower left
                                  ur      ! upper right

    ll%u = XMIN
    ll%v = YMIN
    ur%u = XMAX
    ur%v = YMAX
                                               call kutimer__start('vis2xy')
    call make_time_and_step_string( t, step, tas )

!!>
!    call make_filename( tas, plane.tag, plane.pressure.tag, filename )
!    call draw_contour( ll, ur, filename, plane.pressure )       
!                                               call kutimer__('vis2xy','  pres')
!    call make_filename( tas, plane.tag, plane.flow_helicity.tag, filename )
!    call draw_contour( ll, ur, filename, plane.flow_helicity )  
!                                               call kutimer__('vis2xy','f heli')
!    call make_filename( tas, plane.tag, plane.velocity_z.tag, filename )
!    call draw_contour( ll, ur, filename, plane.velocity_z )     
!                                               call kutimer__('vis2xy',' vel z')
!    call make_filename( tas, plane.tag, plane.enstrophy.tag, filename )
!    call draw_contour( ll, ur, filename, plane.enstrophy )     
!                                               call kutimer__('vis2xy','   ens')
!    call make_filename( tas, plane.tag, plane.velocity.tag, filename )
!    call draw_arrows( ll, ur, filename, plane.velocity )       
!                                               call kutimer__('vis2xy','   vel')
!    call make_filename( tas, plane.tag, plane.vorticity.tag, filename )
!    call draw_arrows( ll, ur, filename, plane.vorticity )       
!                                               call kutimer__('vis2xy','   vor')
!!<
    call make_filename( tas, plane%tag, 'enstrophy_and_flow', filename )
    call draw_contour_and_arrows( ll, ur, filename, plane%enstrophy,  &
                                                    plane%velocity  )
                                               call kutimer__('vis2xy','ensvel')
                                               call kutimer__end('vis2xy')
  end subroutine vis2d_plane_xy__draw


  subroutine vis2d_plane_xz__draw( plane, t, step )
    class(vis2d_plane_xz_t), intent(inout) :: plane
    real   , intent(in) :: t
    integer, intent(in) :: step

    character(len=TAG_NAME_MAX) :: tas ! time and step string
    character(len=FILE_NAME_MAX) :: filename
    type(vv_sketch__sim_pos_t) :: ll,  &  ! lower left
                                  ur      ! upper right

    ll%u = XMIN
    ll%v = ZMIN
    ur%u = XMAX
    ur%v = ZMAX
                                               call kutimer__start('vis2xz')
    call make_time_and_step_string( t, step, tas )

!!>
!    call make_filename( tas, plane.tag, plane.pressure.tag, filename )
!    call draw_contour( ll, ur, filename, plane.pressure )       
!                                               call kutimer__('vis2xz','  pres')
!    call make_filename( tas, plane.tag, plane.flow_helicity.tag, filename )
!    call draw_contour( ll, ur, filename, plane.flow_helicity )  
!                                               call kutimer__('vis2xz','f heli')
!    call make_filename( tas, plane.tag, plane.velocity_y.tag, filename )
!    call draw_contour( ll, ur, filename, plane.velocity_y )     
!                                               call kutimer__('vis2xz',' vel z')
!!<
    call make_filename( tas, plane%tag, plane%enstrophy%tag, filename )
    call draw_contour( ll, ur, filename, plane%enstrophy )     
                                               call kutimer__('vis2xz','   ens')
!!>
!    call make_filename( tas, plane.tag, plane.velocity.tag, filename )
!    call draw_arrows( ll, ur, filename, plane.velocity )       
!                                               call kutimer__('vis2xz','   vel')
!    call make_filename( tas, plane.tag, plane.vorticity.tag, filename )
!    call draw_arrows( ll, ur, filename, plane.vorticity )       
!                                               call kutimer__('vis2xz','   vor')
!!<
    call make_filename( tas, plane%tag, 'combined_ens_vel', filename )
    call draw_contour_and_arrows( ll, ur, filename, plane%enstrophy, &
                                                    plane%velocity )
                                               call kutimer__('vis2xz','ensvel')
                                               call kutimer__end('vis2xz')
  end subroutine vis2d_plane_xz__draw


  subroutine make_time_and_step_string( t, step, string )
    real, intent(in) :: t
    integer, intent(in) :: step
    character(len=TAG_NAME_MAX), intent(out) :: string

    character(len=1), parameter :: CHAR_USCORE = "_"
    character(len=10) :: str_time_with_sign
    character(len=8)  :: str_step_with_sign
    character(len=9)  :: str_time_without_sign
    character(len=7)  :: str_step_without_sign

    str_time_with_sign = ut__real_to_str10( t )
    str_step_with_sign = ut__int_to_str8( step )

    call ut__assert( str_time_with_sign(1:1)=="+",  &
                    "vis2d_m/make_time_and_step_string(203): " //    &
                    "str_time_with_sign is strange." )
    call ut__assert( str_step_with_sign(1:1)=="+",  &
                    "vis2d_m/make_time_and_step_string(206): " //    &
                    "str_step_with_sign is strange." )

    str_time_without_sign = str_time_with_sign(2:10)
    str_step_without_sign = str_step_with_sign(2:8)

    string = "s=" // str_step_without_sign  &
                  // CHAR_USCORE //         &
             "t=" // str_time_without_sign

  end subroutine make_time_and_step_string


  subroutine make_filename( time_and_step_string, tag1, tag2, filename )
    character(len=TAG_NAME_MAX), intent(in) :: time_and_step_string
    character(len=*), intent(in) :: tag1, tag2
    character(len=FILE_NAME_MAX), intent(out) :: filename

    character(len=1), parameter :: CHAR_USCORE = "_"
    character(len=1), parameter :: CHAR_SLASH  = "/"
    character(len=*), parameter :: DIR = '../data/vis2d'

    filename = DIR // CHAR_SLASH // &
               trim(tag1) // CHAR_USCORE // &
               trim(tag2) // CHAR_USCORE // &
               trim(time_and_step_string) // '.svg'
  end subroutine make_filename


  subroutine vis2d_plane_xy__initialize( plane,  &
                                         gk_cut, &
                                         tag )
    class(vis2d_plane_xy_t), intent(out) :: plane
    integer, intent(in) :: gk_cut
    character(len=*), intent(in) :: tag

    real(SR),  dimension(:),   allocatable :: grid_u
    real(SR),  dimension(:),   allocatable :: grid_v
    integer :: nu, nv

    nu = NX_GLOBAL
    nv = NY_GLOBAL
    allocate(grid_u(nu),grid_v(nv))
    grid_u(:) = real(Grid%pos_by_global_ijk%x(:),SR)
    grid_v(:) = real(Grid%pos_by_global_ijk%y(:),SR)

    plane%gk_cut = gk_cut
    plane%tag = tag

    call plane%pressure%initialize( 'pressure', &
                                    nu,         &
                                    nv,         &
                                    grid_u,     &
                                    grid_v )
    call plane%flow_helicity%initialize( 'flow_helicity', &
                                         nu,              &
                                         nv,              &
                                         grid_u,          &
                                         grid_v )
    call plane%velocity_z%initialize( 'velocity_z', &
                                      nu,           &
                                      nv,           &
                                      grid_u,       &
                                      grid_v )
    call plane%enstrophy%initialize( 'enstrophy',   &
                                      nu,           &
                                      nv,           &
                                      grid_u,       &
                                      grid_v )
    call plane%velocity%initialize( 'velocity',   &
                                    nu,           &
                                    nv,           &
                                    grid_u,       &
                                    grid_v )
    call plane%vorticity%initialize( 'vorticity',  &
                                    nu,            &
                                    nv,            &
                                    grid_u,        &
                                    grid_v )

  end subroutine vis2d_plane_xy__initialize


  subroutine vis2d_plane_xz__initialize( plane,  &
                                         gj_cut, &
                                         tag )
    class(vis2d_plane_xz_t), intent(out) :: plane
    integer, intent(in) :: gj_cut
    character(len=*), intent(in) :: tag

    real(SR),  dimension(:),   allocatable :: grid_u
    real(SR),  dimension(:),   allocatable :: grid_v
    integer :: nu, nv

    nu = NX_GLOBAL
    nv = NZ_GLOBAL
    allocate(grid_u(nu),grid_v(nv))
    grid_u(:) = real(Grid%pos_by_global_ijk%x(:),SR)
    grid_v(:) = real(Grid%pos_by_global_ijk%z(:),SR)

    plane%gj_cut = gj_cut
    plane%tag = tag

    call plane%pressure%initialize( 'pressure', &
                                    nu,         &
                                    nv,         &
                                    grid_u,     &
                                    grid_v )
    call plane%flow_helicity%initialize( 'flow_helicity', &
                                         nu,              &
                                         nv,              &
                                         grid_u,          &
                                         grid_v )
    call plane%velocity_y%initialize( 'velocity_y', &
                                      nu,           &
                                      nv,           &
                                      grid_u,       &
                                      grid_v )
    call plane%enstrophy%initialize( 'enstrophy',   &
                                      nu,           &
                                      nv,           &
                                      grid_u,       &
                                      grid_v )
    call plane%velocity%initialize( 'velocity',   &
                                    nu,           &
                                    nv,           &
                                    grid_u,       &
                                    grid_v )
    call plane%vorticity%initialize( 'vorticity',  &
                                    nu,            &
                                    nv,            &
                                    grid_u,        &
                                    grid_v )

  end subroutine vis2d_plane_xz__initialize



  subroutine vis2d_plane_xy__set_data( plane, fluid, vf )
    class(vis2d_plane_xy_t) , intent(inout) :: plane
    type(fluid_t)           , intent(in) :: fluid
    type(vissubfield3d_t)        , intent(in) :: vf

    integer :: nu, nv, gk_cut

    nu = plane%pressure%mesh%vert%nu
    nv = plane%pressure%mesh%vert%nv
    gk_cut = plane%gk_cut

    plane%pressure%val_vert      = iCut( nu, nv, gk_cut,  &
                                         fluid%pressure )
    plane%flow_helicity%val_vert = iCut( nu, nv, gk_cut,  &
                                         vf%helicity )
    plane%velocity_z%val_vert    = iCut( nu, nv, gk_cut,  &
                                         vf%velocity%z )
    plane%enstrophy%val_vert     = iCut( nu, nv, gk_cut,  &
                                         vf%enstrophy )
    plane%velocity%comp_u        = iCut( nu, nv, gk_cut,  &
                                         vf%velocity%x )
    plane%velocity%comp_v        = iCut( nu, nv, gk_cut,  &
                                         vf%velocity%y )
    plane%vorticity%comp_u       = iCut( nu, nv, gk_cut,  &
                                         vf%vorticity%x )
    plane%vorticity%comp_v       = iCut( nu, nv, gk_cut,  &
                                         vf%vorticity%y )

  contains

    function iCut( nu, nv, gk_cut, field3d1, field3d0 ) result(field2d)
      integer, intent(in) :: nu, nv, gk_cut 
      real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: field3d1
      real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in), optional :: field3d0
      real, dimension(nu,nv) :: field2d

      !!>
!          When NXPP=3, NPROC_X=2, NX_GLOBAL=8
!
!         [rank 0]
!                        li=0   1    2    3    4 
!             field3d1&0...o----o----o----o----o
!                          |    |    |    |     
!             field2d......o----o----o----o----x----x----x----x
!                        gi=1   2    3    4    5    6    7    8
!
!         [rank 1]
!                                       li=0   1    2    3    4
!             field3d1&0..................o----o----o----o----o
!                                              |    |    |    |      
!             field2d......x----x----x----x----o----o----o----o
!                        gi=1   2    3    4    5    6    7    8
!
!
!         <<all_reduce_sum>>
!                        gi=1   2    3    4    5    6    7    8
!                 [rank 0] o----o----o----o----x----x----x----x
!                 [rank 1] x----x----x----x----o----o----o----o
!                          |    |    |    |    |    |    |    |
!             field2d......o----o----o----o----o----o----o----o
!                 
      !!<

      integer :: li, lj, lk_cut, gi, gj
      integer :: li_from, li_to, lj_from, lj_to

      field2d(:,:) = 0.0

      if ( Grid%i_have_gk( gk_cut ) ) then
        lk_cut = Grid%local_ijk_from_global_ijk%lk(gk_cut)
        li_from = 1     ! default
        li_to   = NXPP  ! default
        lj_from = 1     ! default
        lj_to   = NYPP  ! default
        if ( Parallel%i_have_xmin ) li_from = 0
        if ( Parallel%i_have_xmax ) li_to   = NXPP1
        if ( Parallel%i_have_ymin ) lj_from = 0
        if ( Parallel%i_have_ymax ) lj_to   = NYPP1

        if ( present(field3d0) ) then
          do lj = lj_from, lj_to
            do li = li_from, li_to
              gj = Grid%global_ijk%gj(lj)
              gi = Grid%global_ijk%gi(li)
              field2d(gi,gj) = real(field3d1(li,lj,lk_cut)   &
                                   -field3d0(li,lj,lk_cut),SR) 
            end do
          end do
        else
          do lj = lj_from, lj_to
            do li = li_from, li_to
              gj = Grid%global_ijk%gj(lj)
              gi = Grid%global_ijk%gi(li)
              field2d(gi,gj) = real(field3d1(li,lj,lk_cut),SR)
            end do
          end do
        end if
      end if

      call mpiut__allreduce_sum( Parallel%comm, field2d )
    end function iCut

  end subroutine vis2d_plane_xy__set_data


  subroutine vis2d_plane_xz__set_data( plane, fluid, vf )
    class(vis2d_plane_xz_t) , intent(inout) :: plane
    type(fluid_t)           , intent(in) :: fluid
    type(vissubfield3d_t)        , intent(in) :: vf

    integer :: nu, nv, gj_cut

    nu = plane%pressure%mesh%vert%nu
    nv = plane%pressure%mesh%vert%nv
    gj_cut = plane%gj_cut

    plane%pressure%val_vert      = iCut( nu, nv, gj_cut,  &
                                         fluid%pressure )
    plane%flow_helicity%val_vert = iCut( nu, nv, gj_cut,  &
                                         vf%helicity )
    plane%velocity_y%val_vert    = iCut( nu, nv, gj_cut,  &
                                         vf%velocity%y )
    plane%enstrophy%val_vert     = iCut( nu, nv, gj_cut,  &
                                         vf%enstrophy )
    plane%velocity%comp_u        = iCut( nu, nv, gj_cut,  &
                                         vf%velocity%x )
    plane%velocity%comp_v        = iCut( nu, nv, gj_cut,  &
                                         vf%velocity%z )
    plane%vorticity%comp_u       = iCut( nu, nv, gj_cut,  &
                                         vf%vorticity%x )
    plane%vorticity%comp_v       = iCut( nu, nv, gj_cut,  &
                                         vf%vorticity%z )

  contains

    function iCut( nu, nv, gj_cut, field3d1, field3d0 ) result(field2d)
      integer, intent(in) :: nu, nv, gj_cut 
      real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in) :: field3d1
      real(DR), dimension(0:NXPP1,0:NYPP1,0:NZPP1), intent(in), optional :: field3d0
      real, dimension(nu,nv) :: field2d

      integer :: li, lk, lj_cut, gi, gk
      integer :: li_from, li_to, lk_from, lk_to

      field2d(:,:) = 0.0

      if ( Grid%i_have_gj( gj_cut ) ) then
        lj_cut = Grid%local_ijk_from_global_ijk%lj(gj_cut)
        li_from = 1     ! default
        li_to   = NXPP  ! default
        lk_from = 1     ! default
        lk_to   = NZPP  ! default
        if ( Parallel%i_have_xmin ) li_from = 0
        if ( Parallel%i_have_xmax ) li_to   = NXPP1
        if ( Parallel%i_have_zmin ) lk_from = 0
        if ( Parallel%i_have_zmax ) lk_to   = NZPP1

        if ( present(field3d0) ) then
          do lk = lk_from, lk_to
            do li = li_from, li_to
              gk = Grid%global_ijk%gk(lk)
              gi = Grid%global_ijk%gi(li)
              field2d(gi,gk) = real(field3d1(li,lj_cut,lk)   &
                                   -field3d0(li,lj_cut,lk),SR) 
            end do
          end do
        else
          do lk = lk_from, lk_to
            do li = li_from, li_to
              gk = Grid%global_ijk%gk(lk)
              gi = Grid%global_ijk%gi(li)
              field2d(gi,gk) = real(field3d1(li,lj_cut,lk),SR)
            end do
          end do
        end if
      end if

      call mpiut__allreduce_sum( Parallel%comm, field2d )
    end function iCut

  end subroutine vis2d_plane_xz__set_data


  subroutine vis2d__initialize( vis2d )
    class(vis2d_t), intent(out) :: vis2d

    integer :: gk_cut, gj_cut

    gk_cut = int(NZ_GLOBAL*0.5)
    call vis2d%z_middle%initialize( gk_cut, 'z_middle' )
    gk_cut = int(NZ_GLOBAL*0.75)
    call vis2d%z_upper%initialize( gk_cut, 'z_upper' )
    gk_cut = int(NZ_GLOBAL*0.25)
    call vis2d%z_lower%initialize( gk_cut, 'z_lower' )

    gj_cut = int(NY_GLOBAL*0.5)
    call vis2d%y_middle%initialize( gj_cut, 'y_middle' )
    gj_cut = int(NY_GLOBAL*0.75)
    call vis2d%y_upper%initialize( gj_cut, 'y_upper' )
    gj_cut = int(NY_GLOBAL*0.25)
    call vis2d%y_lower%initialize( gj_cut, 'y_lower' )
  end subroutine vis2d__initialize


  subroutine vis2d__set_data( vis2d, fluid )
    class(vis2d_t)          , intent(inout) :: vis2d
    type(fluid_t)           , intent(in) :: fluid

    type(vissubfield3d_t) :: vissubfield3d

    call vissubfield3d%update( fluid )
   
    call vis2d%z_middle%set_data( fluid, vissubfield3d )
    call vis2d%z_upper%set_data ( fluid, vissubfield3d )
    call vis2d%z_lower%set_data ( fluid, vissubfield3d )
    call vis2d%y_middle%set_data( fluid, vissubfield3d )
    call vis2d%y_upper%set_data ( fluid, vissubfield3d )
    call vis2d%y_lower%set_data ( fluid, vissubfield3d )
  end subroutine vis2d__set_data


  function min_max_string( vmin, vmax ) result(string)
      real, intent(in) :: vmin, vmax
      character(len=TAG_NAME_MAX) :: string

      character(len=10) :: str10_vmax, str10_vmin
      str10_vmin = ut__real_to_str10( vmin )
      str10_vmax = ut__real_to_str10( vmax )
      string = 'vmin = '//str10_vmin//' vmax = '//str10_vmax
  end function min_max_string


  function vec_amp_max_string( vmax ) result(string)
    real, intent(in) :: vmax
    character(len=TAG_NAME_MAX) :: string

    character(len=10) :: str10_vmax
    str10_vmax = ut__real_to_str10( vmax )
    string = 'vmax = ' // str10_vmax
  end function vec_amp_max_string


  subroutine draw_contour( ll, ur, filename, scalar )
    type(vv_sketch__sim_pos_t), intent(in) :: ll,  &  ! lower left
                                       ur      ! upper right
    character(len=*), intent(in) :: filename
    type(vv_slice_scalar_t), intent(inout) :: scalar

    type(vv_sketch_t) :: vv_sketch
    real :: level_min, level_max, dlevel, level, level_nrm
    real :: level_range
    type(vv_color_t) :: color, color_black
    integer :: l
    character(len=TAG_NAME_MAX) :: message
    real, parameter :: ALMOST_ZERO = 1.e-20_SR
    real, parameter :: TOO_SMALL_DIFF_FOR_CONTOUR = 1.e-3_SR
    real :: max_min_diff_normalized 
    real :: level_amplitude
    logical :: flag_skip_contour

    color_black   = VV_COLOR__CONST%black

    flag_skip_contour = .false. !! default

    call vv_sketch%initialize( ll, ur, &
                               screen_width_in_pixel=1000.0,  &
                               title=scalar%tag,              &
                               filename=trim(filename) )
    ! --<boundary curve>--
    call scalar%mesh%draw( vv_sketch, &
                           line_color=VV_COLOR__CONST%black,  &
                           width_in_pixels=1.0 )

    level_min = minval(scalar%val_vert)
    level_max = maxval(scalar%val_vert)
    message = min_max_string( level_min, level_max )
    call iText_in_header_and_footer( message )
    level_amplitude = max(abs(level_max),abs(level_min))

    if ( level_amplitude <= ALMOST_ZERO ) then
      call vv_sketch%finalize                 
      return
    end if

    level_range = level_max - level_min
    max_min_diff_normalized = level_range / level_amplitude
    if ( max_min_diff_normalized <= TOO_SMALL_DIFF_FOR_CONTOUR )  then
      call vv_sketch%finalize                 
      return
    end if

    call vv_sketch%group_push( line_color=VV_COLOR__CONST%magenta,  &
                               line_width_in_pixel=1.0 )
      dlevel = level_range / 10
      do l = 1, 10
        level = level_min + dlevel*(real(l)-0.5)
        level_nrm = (level-level_min) / level_range
        color = vv_color__normalized_real_to_color( level_nrm )
        call scalar%vis_contour( vv_sketch,              &
                                 level,                  &
                                 line_color=color_black, &
                                 fill_color=color )
                               ! debug_print=.true. ) ! for debug.
      end do

    call vv_sketch%group_pop

    call vv_sketch%finalize                 

  contains

    subroutine iText_in_header_and_footer( message )
      character(len=TAG_NAME_MAX), intent(in) :: message

      real :: text_v_pos_in_physical_unit
      real :: tsep ! text v position separation betwen lines

      call vv_sketch%group_push( line_color=color_black,  &
                                 line_width_in_pixel=1.0 )

      tsep = (ur%v-ll%v)*0.1

      ! --<figure title in the header>--
      text_v_pos_in_physical_unit = ur%v + tsep
      call vv_sketch%text( (ll%u+ur%u)/2,  &
                           text_v_pos_in_physical_unit,  &
                           vv_sketch%title,  &
                           font_size_in_pixel=30.0,  &
                           text_anchor='middle' )
      ! --<simple text in the footer>--
      text_v_pos_in_physical_unit = ll%v - tsep
      call vv_sketch%text( ll%u,  &
                           text_v_pos_in_physical_unit,  &
                           trim(filename),  &
                           font_size_in_pixel=15.0,  &
                           text_anchor='start' )
      text_v_pos_in_physical_unit = text_v_pos_in_physical_unit - tsep
      call vv_sketch%text( ll%u,  &
                           text_v_pos_in_physical_unit,    &
                           message,  &
                           font_size_in_pixel=15.0,  &
                           text_anchor='start' )
      call vv_sketch%group_pop
    end subroutine iText_in_header_and_footer
    
  end subroutine draw_contour


  subroutine draw_arrows( ll, ur, filename, vector )
    type(vv_sketch__sim_pos_t), intent(in) :: ll,  &  ! lower left
                                       ur      ! upper right
    character(len=*), intent(in) :: filename
    type(vv_slice_vector_t), intent(inout) :: vector

    real :: vec_amp_max
    type(vv_sketch_t) :: vv_sketch
    type(vv_color_t) :: color_black
    type(vv_color_t) :: color_limegreen, color_darkgreen
    character(len=TAG_NAME_MAX) :: message
    real, parameter :: ALMOST_ZERO = 1.e-20_SR

    color_black   = VV_COLOR__CONST%black
    color_limegreen = VV_COLOR__CONST%limegreen
    color_darkgreen = VV_COLOR__CONST%darkgreen

    call vv_sketch%initialize( ll, ur, &
                               screen_width_in_pixel=1000.0, &
                               title=vector%tag,             &
                               filename=trim(filename),      &
                               unit_arrow_in_pixel=20.0,     &
                               write_arrow_template=.true. )
    ! --<boundary curve>--
    call vector%mesh%draw( vv_sketch, &
                           line_color=VV_COLOR__CONST%black,  &
                           width_in_pixels=1.0 )

    vec_amp_max = sqrt(maxval(vector%comp_u(:,:)**2  &
                            + vector%comp_v(:,:)**2))
    message = vec_amp_max_string( vec_amp_max )
    call iText_in_header_and_footer( message )

    if ( vec_amp_max <= ALMOST_ZERO ) then
      call vv_sketch%finalize
      return
    end if

    call vv_sketch%group_push( line_color=color_limegreen,  &
                               fill_color=color_darkgreen,  &
                               line_width_in_pixel=1.0 )
      vector%comp_u(:,:) = vector%comp_u(:,:) / vec_amp_max
      vector%comp_v(:,:) = vector%comp_v(:,:) / vec_amp_max
      call vector%vis_arrows( vv_sketch, arrow_template="#arrow02" )
    call vv_sketch%group_pop

    call vv_sketch%finalize                 

  contains
    
    subroutine iText_in_header_and_footer( message )
      character(len=TAG_NAME_MAX), intent(in) :: message

      real :: text_v_pos_in_physical_unit
      real :: tsep ! text v position separation betwen lines

      call vv_sketch%group_push( line_color=color_black,  &
                                 line_width_in_pixel=1.0 )

      tsep = (ur%v-ll%v)*0.1

      ! --<figure title in the header>--
      text_v_pos_in_physical_unit = ur%v + tsep
      call vv_sketch%text( (ll%u+ur%u)/2,  &
                           text_v_pos_in_physical_unit,  &
                           vv_sketch%title,  &
                           font_size_in_pixel=30.0,  &
                           text_anchor='middle' )
      ! --<simple text in the footer>--
      text_v_pos_in_physical_unit = ll%v - tsep
      call vv_sketch%text( ll%u,  &
                           text_v_pos_in_physical_unit,  &
                           trim(filename),  &
                           font_size_in_pixel=15.0,  &
                           text_anchor='start' )
      text_v_pos_in_physical_unit = text_v_pos_in_physical_unit - tsep
      call vv_sketch%text( ll%u,  &
                           text_v_pos_in_physical_unit,  &
                           message,  &
                           font_size_in_pixel=15.0,  &
                           text_anchor='start' )
      call vv_sketch%group_pop
    end subroutine iText_in_header_and_footer

  end subroutine draw_arrows


  subroutine draw_contour_and_arrows( ll, ur, filename, scalar, vector )
    type(vv_sketch__sim_pos_t), intent(in) :: ll,  &  ! lower left
                                       ur      ! upper right
    character(len=*), intent(in) :: filename
    type(vv_slice_scalar_t), intent(inout) :: scalar
    type(vv_slice_vector_t), intent(inout) :: vector

    real :: vec_amp_max
    type(vv_sketch_t) :: vv_sketch
    real :: level_min, level_max, dlevel, level, level_nrm
    real :: max_min_diff_normalized 
    real :: level_range
    real :: level_amplitude
    type(vv_color_t) :: color, color_black
    type(vv_color_t) :: color_brown, color_sandybrown
    integer :: l
    character(len=TAG_NAME_MAX) :: message1, message2
    character(len=TAG_NAME_MAX*2) :: figure_title
    real, parameter :: ALMOST_ZERO = 1.e-20_SR
    real, parameter :: TOO_SMALL_DIFF_FOR_CONTOUR = 1.e-3_SR

    color_black       = VV_COLOR__CONST%black
    color_brown = VV_COLOR__CONST%brown
    color_sandybrown  = VV_COLOR__CONST%sandybrown

    figure_title = trim(scalar%tag) // ' (contour) and ' // &
                   trim(vector%tag) // ' (arrows)'
    call vv_sketch%initialize( ll, ur,  &
                               screen_width_in_pixel=1000.0, &
                               title=figure_title,           &
                               filename=trim(filename),      &
                               unit_arrow_in_pixel=20.0,     &
                               write_arrow_template=.true. )
    ! --<boundary curve>--
    call scalar%mesh%draw( vv_sketch, &
                           line_color=VV_COLOR__CONST%black,  &
                           width_in_pixels=1.0 )

    level_min = minval(scalar%val_vert)
    level_max = maxval(scalar%val_vert)
    message1 = min_max_string( level_min, level_max )

    vec_amp_max = sqrt(maxval(vector%comp_u(:,:)**2  &
                            + vector%comp_v(:,:)**2))
    message2 = vec_amp_max_string( vec_amp_max )

    call iText_in_header_and_footer( message1, message2 )

    !!>
!        Contours 
    !!<
    level_amplitude = max(abs(level_max),abs(level_min))
    if ( level_amplitude > ALMOST_ZERO ) then
      call vv_sketch%group_push( line_color=color_black,  &
                                 line_width_in_pixel=1.0 )
        level_range = level_max - level_min
        max_min_diff_normalized = level_range / level_amplitude
        if ( max_min_diff_normalized >= TOO_SMALL_DIFF_FOR_CONTOUR )  then
          dlevel = level_range / 10
          do l = 1, 10
            level = level_min + dlevel*(real(l)-0.5)
            level_nrm = (level-level_min) / level_range
            color = vv_color__normalized_real_to_color( level_nrm )
            call scalar%vis_contour( vv_sketch,              &
                                     level,                  &
                                     line_color=color_black, &
                                     fill_color=color )
          end do
        end if
      call vv_sketch%group_pop
    end if

    !!>
!        Arrows
    !!<
    if ( vec_amp_max > ALMOST_ZERO ) then
      call vv_sketch%group_push( line_color=color_brown,  &
                                 fill_color=color_sandybrown, &
                                 line_width_in_pixel=1.0 )
          vector%comp_u(:,:) = vector%comp_u(:,:) / vec_amp_max
          vector%comp_v(:,:) = vector%comp_v(:,:) / vec_amp_max
          call vector%vis_arrows( vv_sketch, arrow_template="#arrow02" )
      call vv_sketch%group_pop
    end if

    call vv_sketch%finalize                 

  contains
    
    subroutine iText_in_header_and_footer( message1, message2 )
      character(len=TAG_NAME_MAX), intent(in) :: message1, message2

      real :: text_v_pos_in_physical_unit
      real :: tsep ! text v position separation betwen lines
      
      call vv_sketch%group_push( line_color=color_black,  &
                                 line_width_in_pixel=1.0 )

      tsep = (ur%v-ll%v)*0.1

      ! --<figure title in the header>--
      text_v_pos_in_physical_unit = ur%v + tsep
      call vv_sketch%text( (ll%u+ur%u)/2,                &
                           text_v_pos_in_physical_unit,  &
                           vv_sketch%title,              &
                           font_size_in_pixel=30.0,      &
                           text_anchor='middle' )
      ! --<simple text in the footer>--
      text_v_pos_in_physical_unit = ll%v - tsep
      call vv_sketch%text( ll%u,                         &
                           text_v_pos_in_physical_unit,  &
                           trim(filename),               &
                           font_size_in_pixel=15.0,      &
                           text_anchor='start' )
      text_v_pos_in_physical_unit = text_v_pos_in_physical_unit - tsep
      call vv_sketch%text( ll%u,                           &
                           text_v_pos_in_physical_unit,    &
                           'Contour: ' // trim(message1),  &
                           font_size_in_pixel=15.0,        &
                           text_anchor='start' )
      text_v_pos_in_physical_unit = text_v_pos_in_physical_unit - tsep
      call vv_sketch%text( ll%u,                         &
                           text_v_pos_in_physical_unit,  &
                           'Arrow: ' // trim(message2),  &
                           font_size_in_pixel=15.0,      &
                           text_anchor='start' )
      call vv_sketch%group_pop
    end subroutine iText_in_header_and_footer

  end subroutine draw_contour_and_arrows


  subroutine vis2d__draw( vis2d, t_double, nloop, fluid )
    class(vis2d_t), intent(inout) :: vis2d
    real(DR)      , intent(in) :: t_double
    integer       , intent(in) :: nloop
    type(fluid_t) , intent(in) :: fluid

    real :: t 
    integer :: ctr = 0

    t = real(t_double,SR)

                                                 call kutimer__start('vis2  ')
    if(mod(ctr,100)==0) then
      call vis2d%set_data( fluid )              !{vis2  }{set d}
      if ( Parallel%rank%me == 0 ) then
        call vis2d%z_upper%draw ( t, nloop )
        call vis2d%z_middle%draw( t, nloop )    !{vis2  }{   xy}
!       call vis2d.z_lower.draw ( t, nloop )
!       call vis2d.y_upper.draw ( t, nloop )
!       call vis2d.y_middle.draw( t, nloop )
!       call vis2d.y_lower.draw ( t, nloop )    !{vis2  }{   xz}
      end if
      call mpiut__barrier( Parallel%comm )
    end if; ctr = ctr + 1 
                                                 call kutimer__end('vis2  ')
  end subroutine vis2d__draw

end module vis2d_m
