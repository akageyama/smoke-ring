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
!    slice_grapher/turtle.f90
!-------------------------------------------------------------------
!
!-------------------------------------------------------------------
! turtle.f90
!
! - A simple 2D draw library for
!         - points
!         - lines
!         - curves
!         - contours
!         - vector arrows
!   in the cartesian and polar coordinate systems.
!
! - This library does not produce images, instead, it writes
!   points and line segments in a file as a text data.
!
! - Use gnuplot, for example, to convert the text data into
!   images.
!
!       Akira Kageyama (sgks@mac.com)
!-------------------------------------------------------------------
! 2005.11.22: Development start.
!             Most part of contour comes from my old "glib".
! 2005.11.23: Finished. Beautiful!
! 2005.11.24: Added select_file_for_lines and _points.
! 2005.11.24: Minor change in vector_polar.
! 2007.08.12: Minor revision for Les Houches Summer School "Dynamos".
! 2018.11.28: Minor revision for "smoke-ring" for the class "HPC".
!-------------------------------------------------------------------

module turtle_m
  use constants_m
  use ut_m
  implicit none
  private
  public :: & !<< routines >>!
            turtle__arrow,  &
            turtle__broken_line,  &
            turtle__circle,  &
            turtle__contour_cartesian,  &
            turtle__contour_polar,  &
            turtle__coords_shift,  &
            turtle__distance,  &
            turtle__filename_for_lines,  &
            turtle__initialize,  &
            turtle__line,  &
            turtle__move,  &
            turtle__point,  &
            turtle__rectangle,  &
            turtle__vector_cartesian,  &
            turtle__vector_polar

  logical, save :: Initialization_done = .false.

  type, public :: turtle__scalar2d_cartesian_t
     integer :: nx, ny
     real(SR), dimension(:) ,  pointer :: xpos, ypos
     real(SR), dimension(:,:), pointer :: f
  end type turtle__scalar2d_cartesian_t

  type, public :: turtle__scalar2d_polar_t
     integer :: nr, nt
     real(SR), dimension(:),   pointer :: rpos
     real(SR), dimension(:),   pointer :: tpos
     real(SR), dimension(:,:), pointer :: f
  end type turtle__scalar2d_polar_t

  type, public :: turtle__vector2d_polar_t
     integer :: nr, nt
     real(SR), dimension(:),   pointer :: rpos, tpos
     real(SR), dimension(:,:), pointer :: x, y
     real(SR) :: vmax, vmin
  end type turtle__vector2d_polar_t

  type, public :: turtle__vector2d_cartesian_t
     integer :: nx, ny
     real(SR), dimension(:),   pointer :: xpos, ypos
     real(SR), dimension(:,:), pointer :: x, y
     real(SR) :: vmax, vmin
  end type turtle__vector2d_cartesian_t

  type, public :: turtle__pos_t
     real(SR) :: x, y
  end type turtle__pos_t

  interface operator (*)
     module procedure operator_real_times_typepos
  end interface

  interface operator (/)
     module procedure operator_typepos_div_real
  end interface

  interface operator (+)
     module procedure operator_typepos_plus
  end interface

  interface operator (-)
     module procedure operator_typepos_minus
  end interface

  interface turtle__arrow
     module procedure arrow_structure
  end interface

  interface turtle__broken_line
     module procedure broken_line,  &
                      broken_line_structure
  end interface

  interface turtle__circle
     module procedure circle,  &
                      circle_structure
  end interface


  interface turtle__distance
     module procedure distance,  &
                      distance_structure
  end interface

  interface turtle__line
     module procedure line,  &
                      line_structure

  end interface

  interface turtle__point
     module procedure point,  &
                      point_structure
  end interface

  interface turtle__move
     module procedure move,  &
                      move_other_coords,  &
                      move_structure
  end interface

  interface turtle__rectangle
     module procedure rectangle,  &
                      rectangle_structure
  end interface


  type draw_area_
     type(turtle__pos_t) :: corner_north_west
     type(turtle__pos_t) :: corner_north_east
     type(turtle__pos_t) :: corner_south_west
     type(turtle__pos_t) :: corner_south_east
     type(turtle__pos_t) :: origin
     real(SR)           :: diag_length
  end type draw_area_

  type(draw_area_) :: Draw_area

  !<< Output file for the plot data >>!
  !   e.g., line data file = "Workfiles/lines_000.tt"
  character(len=*), parameter :: DIR_FOR_PLOT_DATA = "Workfiles/"
  character(len=*), parameter :: EXT_FOR_PLOT_DATA = "tt"

  integer :: Unit_num_for_turtle  ! will be set in initialize

  character, parameter :: CHAR_SLASH = '/'
  character, parameter :: CHAR_DOT   = '.'


contains


!_______________________________________________________________private__
!
  subroutine arrow_structure(center,vec)
    type(turtle__pos_t), intent(in) :: center
    type(turtle__pos_t), intent(in) :: vec
!________________________________________________________________________
!
!                             p
!                              \
!                               \
!                                \
!            stt -------------q---- end
!                                /
!                               /
!                              /
!                             r
!
    real(SR), parameter :: ALPHA = 3.0_SR / 4.0_SR
    real(SR), parameter :: BETA  = 1.0_SR/12.0_SR
!
    type(turtle__pos_t)  :: stt, end, p, q, r

!!$    if ( (vec%x)**2+(vec%y)**2 < 1.e-3_SR ) return   ! too short

    stt = center - (0.5_SR*vec)
    end = center + (0.5_SR*vec)

    q = stt + (ALPHA*vec)

    p%x = q%x + BETA*(vec%y)
    p%y = q%y - BETA*(vec%x)
    r%x = q%x - BETA*(vec%y)
    r%y = q%y + BETA*(vec%x)

    call turtle__move(stt)
    call turtle__line(end)
    call turtle__move(p)
    call turtle__line(end)
    call turtle__line(r)

  end subroutine arrow_structure


!_______________________________________________________________private__
!
  subroutine broken_line(x,y,new)
    real(SR), intent(in) :: x, y
    logical, optional, intent(in) :: new
!________________________________________________________________________
!
    type(turtle__pos_t) :: pos

    pos%x = x
    pos%y = y

    if (present(new)) then
       call broken_line_structure(pos,new)
    else
       call broken_line_structure(pos)
    end if

  end subroutine broken_line


!_______________________________________________________________private__
!
  subroutine broken_line_structure(pos_in,new)
    type(turtle__pos_t), intent(in) :: pos_in
    logical, optional, intent(in) :: new
!________________________________________________________________________
!
!     Original code developed by A. Kageyama in early 1990s in f77.
!
!     1994.06.07: modified
!     2005.11.23: re-written in f90 by A. Kageyama.
!
      real(SR), parameter :: UNIT1_FACTOR = 0.01_SR
      real(SR), parameter :: UNIT2_FACTOR = 0.0025_SR
      real(SR), save :: amari = 0.0_SR  ! i know it's automatic save.
      type(turtle__pos_t) :: pos, pos0, pos1, vnormd
      real(SR) :: dist, rmaind
      integer :: i

      real(SR) :: unit1, unit2, unit
      type(turtle__pos_t), save :: pos_prev

      if ( Draw_area%diag_length < 1.e-5_SR )   &
         call ut__fatal('<turtle/broken_line> Draw_area%diag_length = 0 ?')

      unit1 = UNIT1_FACTOR*Draw_area%diag_length
      unit2 = UNIT2_FACTOR*Draw_area%diag_length
      unit  = unit1 + unit2

      if ( present(new) ) then
         pos_prev = pos_in
         amari = 0.0_SR
         return
      end if

      pos0 = pos_prev
      pos  = pos_in

      dist = turtle__distance(pos0,pos)

      if( dist < 1.e-5_SR ) return

      vnormd = (pos-pos0)/dist
!
!
!  (gx0,gy0)                                         (gx,gy)
!      !                                                !
!      !<-------------------d-i-s-t-------------------->!
!      !-----!-----------------!-----------------!------!---------!
!      !amari!      unit       !       unit      !rmaind!  amari  !
!      !                                                !
!
!            !--unit1--!-unit2-!
!            !<-----unit------>!
!

      if( amari > dist ) then
        if( amari < unit2 ) then
          call turtle__move(pos)
        else if( amari-unit2 < dist ) then
          pos1 = pos0 + (amari-unit2)*vnormd
          call turtle__line(pos1)
          call turtle__move(pos)
        else
          call turtle__line(pos)
        end if
        amari = amari - dist
        pos_prev = pos_in
        return
      end if

      if( amari > unit2 ) then
        pos1 = pos0 + (amari-unit2)*vnormd
        pos0 = pos0 + amari*vnormd
        call turtle__line(pos1)
        call turtle__move(pos0)
      else
        pos0 = pos0 + amari*vnormd
        call turtle__move(pos0)
      end if

      do i = 1 , int((dist-amari)/unit)
        pos1 = pos0 + unit1*vnormd
        pos0 = pos0 +  unit*vnormd
        call turtle__line(pos1)
        call turtle__move(pos0)
      end do

      rmaind = mod(dist-amari,unit)

      if( rmaind > unit1 ) then
        pos1 = pos0 + unit1*vnormd
        call turtle__line(pos1)
        call turtle__move(pos)
      else
        call turtle__line(pos)
      end if

      amari = unit - rmaind

      pos_prev = pos_in

    end subroutine broken_line_structure

!_______________________________________________________________private__
!
    subroutine check_and_draw_cartesian(i,j,iside,ifound,karte,lineid,  &
                                        field,flagx,flagy)
      integer, intent(in) :: i,j,iside
      integer, intent(inout) :: ifound, karte
      character(len=5), intent(in) :: lineid
      type(turtle__scalar2d_cartesian_t), intent(in) :: field
      integer, dimension(:,:), intent(inout) :: flagx, flagy
!________________________________________________________________________
!
!     purpose : check and draw a contour line segment
!               in cartesian corrdinate.
!     note    :
!     called  : lnfolw_cartesian
!
!     originally developed by Akira Kageyama in early 1990s in f77.
!     2005.11.23: Converted into f90 by Akira Kageyama.
!________________________________________________________________________
!
      real(SR) :: xxx, yyy

      if ( iside==1 .or. iside==4 ) then
        if ( field%f(i,j)*field%f(i+1,j) <= 0. ) then
           ! you've found a line.
           ifound = 1
           if ( field%f(i,j) == field%f(i+1,j) ) then
              xxx = ( field%xpos(i)+field%xpos(i+1) ) / 2.
           else
              xxx = field%xpos(i)  &
                  - field%f(i,j)*(field%xpos(i+1)-field%xpos(i))  &
                                /(field%f(i+1,j)-field%f(i,j))
           end if
           yyy = field%ypos(j)

           if ( lineid=='solid' ) then
              call turtle__line(xxx,yyy)
           else if ( lineid=='brokn' ) then
              call turtle__broken_line(xxx,yyy)
          end if

          if ( iside==1 .and. j==field%ny )   karte = 9
          if ( iside==4 .and. j==1  )         karte = 9
          !  line closing check
          if ( flagx(i,j)==1 ) karte = 8
        end if

        flagx(i,j) = 1

      else if ( iside==2 .or. iside==3 ) then
        if ( field%f(i,j)*field%f(i,j+1) <= 0. ) then
           !  you've found a line.
           ifound = 1
           if ( field%f(i,j)==field%f(i,j+1) ) then
              yyy = ( field%ypos(j)+field%ypos(j+1) ) / 2.
           else
              yyy = field%ypos(j)  &
                  - field%f(i,j)*(field%ypos(j+1)-field%ypos(j))  &
                                /(field%f(i,j+1)-field%f(i,j))
           end if
           xxx = field%xpos(i)

           if( lineid=='solid' ) then
              call turtle__line(xxx,yyy)
           else if ( lineid=='brokn' ) then
              call turtle__broken_line(xxx,yyy)
           end if
           !   regeion check
           if ( iside==2 .and. i==field%nx )   karte = 9
           if ( iside==3 .and. i==1  )         karte = 9
           !  line closing check
           if ( flagy(i,j)==1 )  karte = 8
        end if

        flagy(i,j) = 1

     end if

   end subroutine check_and_draw_cartesian


!_______________________________________________________________private__
!
    subroutine check_and_draw_polar(i,j,iside,ifound,karte,lineid,  &
                                    field,flagr,flagt)
      integer, intent(in)    :: i,j,iside
      integer, intent(inout) :: ifound, karte
      character(len=5), intent(in)    :: lineid
      type(turtle__scalar2d_polar_t), intent(in) :: field
      integer, dimension(:,:), intent(inout) :: flagr, flagt
!________________________________________________________________________
!
!     purpose : check and draw a contour line segment
!               in the polar-corrdinates.
!     note    :
!     called  : lnfolw_polar
!
!     originally developed by Akira Kageyama in early 1990s in f77.
!     2005.11.24: Converted into f90 by Akira Kageyama.
!________________________________________________________________________
!
      real(SR) :: rad, tht
      type(turtle__pos_t) :: pos

      if ( iside==1 .or. iside==4 ) then
        if ( field%f(i,j)*field%f(i+1,j) <= 0. ) then
           ! you've found a line.
           ifound = 1
           if ( field%f(i,j) == field%f(i+1,j) ) then
              rad = ( field%rpos(i)+field%rpos(i+1) ) / 2.
           else
              rad = field%rpos(i)  &
                  - field%f(i,j)*(field%rpos(i+1)-field%rpos(i))  &
                                /(field%f(i+1,j)-field%f(i,j))
           end if
           tht = field%tpos(j)

           pos = coords_trans_polar_to_cartesian(rad,tht)

           if ( lineid=='solid' ) then
              call turtle__line(pos)
           else if ( lineid=='brokn' ) then
              call turtle__broken_line(pos)
          end if

          if ( iside==1 .and. j==field%nt )   karte = 9
          if ( iside==4 .and. j==1  )         karte = 9
          !  line closing check
          if ( flagr(i,j)==1 ) karte = 8
        end if

        flagr(i,j) = 1

      else if ( iside==2 .or. iside==3 ) then

        if ( field%f(i,j)*field%f(i,j+1) <= 0. ) then
           !  you've found a line.
           ifound = 1
           if ( field%f(i,j)==field%f(i,j+1) ) then
              tht = ( field%tpos(j)+field%tpos(j+1) ) / 2.
           else
              tht = field%tpos(j)  &
                  - field%f(i,j)*(field%tpos(j+1)-field%tpos(j))  &
                                /(field%f(i,j+1)-field%f(i,j))
           end if
           rad = field%rpos(i)

           pos = coords_trans_polar_to_cartesian(rad,tht)

           if( lineid=='solid' ) then
              call turtle__line(pos)
           else if ( lineid=='brokn' ) then
              call turtle__broken_line(pos)
           end if
           !   regeion check
           if ( iside==2 .and. i==field%nr )   karte = 9
           if ( iside==3 .and. i==1  )         karte = 9
           !  line closing check
           if ( flagt(i,j)==1 )  karte = 8
        end if

        flagt(i,j) = 1

     end if

   end subroutine check_and_draw_polar


!_______________________________________________________________private__
!
  subroutine circle(center_x,center_y,radius,angle_from,angle_to)
    real(SR), intent(in) :: center_x, center_y, radius
    real(SR), intent(in), optional :: angle_from, angle_to
!________________________________________________________________________
!
    integer,  parameter :: NDIV = 100
    real(SR) :: x, y, tht
    integer  :: i
    real(SR) :: tht_from, tht_to, dtht

    if ( present(angle_from) .and. present(angle_to) ) then
       tht_from = angle_from
       tht_to   = angle_to
    else
       tht_from = 0.0_SR
       tht_to   = TWOPI
    end if

    dtht = (tht_to-tht_from) / NDIV

    x = center_x + radius*cos(tht_from)
    y = center_y + radius*sin(tht_from)
    call move(x,y)

    do i = 1 , NDIV
       tht = tht_from + dtht*i
       x = center_x + radius*cos(tht)
       y = center_y + radius*sin(tht)
       call line(x,y)
    end do

  end subroutine circle


!_______________________________________________________________private__
!
  subroutine circle_structure(center,radius,tht_from,tht_to)
    type(turtle__pos_t), intent(in) :: center
    real(SR), intent(in) :: radius
    real(SR), intent(in), optional :: tht_from, tht_to
!________________________________________________________________________
!
    if (present(tht_from) .and. present(tht_to)) then
       call circle(center%x,center%y,radius,tht_from,tht_to)
    else
       call circle(center%x,center%y,radius)
    end if

  end subroutine circle_structure


!_______________________________________________________________private__
!
  subroutine cont0_cartesian(field,lineid)
    type(turtle__scalar2d_cartesian_t), intent(in) :: field
    character(len=5), intent(in) :: lineid
!________________________________________________________________________
!
!     purpose : draw a contour line of height 0 in cartesian coordinate.
!
!     note    :
!
!     calls   : lnfolw_cartesian, cplot
!
!     called  : contr
!
!     by a. kageyama    91.06.18
!
!     2005.11.23: Converted into f90 by Akira Kageyama.
!_______________________________________________________________________/
!
    integer :: nx, ny, i, j
    integer, dimension(:,:), allocatable :: flagx, flagy
    real(SR) :: xxx, yyy
    integer :: ienter, inow, jnow, karte

    real(SR) :: x, y, rsq

    nx = field%nx
    ny = field%ny

    allocate(flagx(nx,ny),flagy(nx,ny))
    !
    !  reset the segment marker or flag.
    !  whether the line is already drawn on this side or not.
    !
    flagx(:,:) = 0
    flagy(:,:) = 0

    do  i = 1 , nx
       do  j = 1 , ny - 1
          if ( flagy(i,j)==1 ) cycle
          !  unexplored segment.
          flagy(i,j) = 1

          if ( field%f(i,j)*field%f(i,j+1)<=0. ) then
             ! you've found a line.
             if ( field%f(i,j)==field%f(i,j+1) ) then
                ! 0 on both points.
                yyy = ( field%ypos(j)+field%ypos(j+1) ) / 2.
             else
                yyy = field%ypos(j)  &
                     - field%f(i,j)*(field%ypos(j+1)-field%ypos(j))  &
                     /(field%f(i,j+1)-field%f(i,j))
             end if
             xxx = field%xpos(i)

             call turtle__move(xxx,yyy)
             if (lineid=='brokn') call turtle__broken_line(xxx,yyy,new=.true.)

             if ( i/=nx ) then
                !    2-direction
                ienter = 2
                inow = i
                jnow = j
                karte = 0
                do while ( karte==0 )
                   call lnfolw_cartesian(inow,jnow,ienter,karte,lineid, &
                                         field,flagx,flagy)
                end do
             end if

             call turtle__move(xxx,yyy)
             if (lineid=='brokn') call turtle__broken_line(xxx,yyy,new=.true.)

             if ( i/=1 ) then
                !    3-direction
                ienter = 3
                inow = i
                jnow = j
                karte = 0
                do while ( karte==0 )
                   call lnfolw_cartesian(inow,jnow,ienter,karte,lineid, &
                                         field,flagx,flagy)
                end do
             end if
          end if
       end do
    end do

    do j = 1 , ny
       do i = 1 , nx-1
          if ( flagx(i,j)==1 ) cycle
          !   unexplored segment.
          flagx(i,j) = 1

          if ( field%f(i,j)*field%f(i+1,j)<=0. ) then
             ! you've found a line.
             if ( field%f(i,j)==field%f(i+1,j) ) then
                !    0 on both points.
                xxx = ( field%xpos(i)+field%xpos(i+1) ) / 2.
             else
                xxx = field%xpos(i)  &
                     - field%f(i,j)*(field%xpos(i+1)-field%xpos(i))  &
                                   /(field%f(i+1,j)-field%f(i,j))
             end if
             yyy = field%ypos(j)
             call turtle__move(xxx,yyy)
             if (lineid=='brokn') call turtle__broken_line(xxx,yyy,new=.true.)

             if ( j/=ny ) then
                ienter = 1
                inow = i
                jnow = j
                karte = 0
                do while ( karte==0 )
                   call lnfolw_cartesian(inow,jnow,ienter,karte,lineid,  &
                                         field,flagx,flagy)
                end do
             end if

             call turtle__move(xxx,yyy)
             if (lineid=='brokn') call turtle__broken_line(xxx,yyy,new=.true.)

             if( j/=1 ) then
                ienter = 4
                inow = i
                jnow = j
                karte = 0
                do while ( karte==0 )
                   call lnfolw_cartesian(inow,jnow,ienter,karte,lineid,  &
                                         field,flagx,flagy)
                end do
             end if
          end if
       end do
    end do

    deallocate(flagx,flagy)

  end subroutine cont0_cartesian


!_______________________________________________________________private__
!
  subroutine cont0_polar(field,lineid)
    type(turtle__scalar2d_polar_t), intent(in) :: field
    character(len=5), intent(in) :: lineid
!________________________________________________________________________
!
!     purpose : draw a contour line of height 0 in the polar-coords.
!     note    :
!     calls   : lnfolw_polar, cplot
!     called  : contr
!
!     by a. kageyama    91.06.18
!
!     2005.11.24: Converted into f90 by Akira Kageyama.
!_______________________________________________________________________/
!
    integer :: nr, nt, i, j
    integer, dimension(:,:), allocatable :: flagr, flagt
    real(SR) :: rad, tht
    integer :: ienter, inow, jnow, karte

    type(turtle__pos_t) :: pos

    nr = field%nr
    nt = field%nt

    allocate(flagr(nr,nt),flagt(nr,nt))
    !
    !  reset the segment marker or flag.
    !  whether the line is already drawn on this side or not.
    !
    flagr(:,:) = 0
    flagt(:,:) = 0

    do  i = 1 , nr
       do  j = 1 , nt - 1
          if ( flagt(i,j)==1 ) cycle
          !  unexplored segment.
          flagt(i,j) = 1

          if ( field%f(i,j)*field%f(i,j+1)<=0. ) then
             ! you've found a line.
             if ( field%f(i,j)==field%f(i,j+1) ) then
                ! 0 on both points.
                tht = ( field%tpos(j)+field%tpos(j+1) ) / 2.
             else
                tht = field%tpos(j)  &
                     - field%f(i,j)*(field%tpos(j+1)-field%tpos(j))  &
                                   /(field%f(i,j+1)-field%f(i,j))
             end if
             rad = field%rpos(i)

             pos = coords_trans_polar_to_cartesian(rad,tht)

             call turtle__move(pos)
             if (lineid=='brokn') call turtle__broken_line(pos,new=.true.)

             if ( i/=nr ) then
                !    2-direction
                ienter = 2
                inow = i
                jnow = j
                karte = 0
                do while ( karte==0 )
                   call lnfolw_polar(inow,jnow,ienter,karte,lineid, &
                                     field,flagr,flagt)
                end do
             end if

             call turtle__move(pos)
             if (lineid=='brokn') call turtle__broken_line(pos,new=.true.)

             if ( i/=1 ) then
                !    3-direction
                ienter = 3
                inow = i
                jnow = j
                karte = 0
                do while ( karte==0 )
                   call lnfolw_polar(inow,jnow,ienter,karte,lineid, &
                                     field,flagr,flagt)
                end do
             end if
          end if
       end do
    end do

    do j = 1 , nt
       do i = 1 , nr-1
          if ( flagr(i,j)==1 ) cycle
          !   unexplored segment.
          flagr(i,j) = 1

          if ( field%f(i,j)*field%f(i+1,j)<=0. ) then
             ! you've found a line.
             if ( field%f(i,j)==field%f(i+1,j) ) then
                !    0 on both points.
                rad = ( field%rpos(i)+field%rpos(i+1) ) / 2.
             else
                rad = field%rpos(i)  &
                     - field%f(i,j)*(field%rpos(i+1)-field%rpos(i))  &
                                   /(field%f(i+1,j)-field%f(i,j))
             end if
             tht = field%tpos(j)

             pos = coords_trans_polar_to_cartesian(rad,tht)

             call turtle__move(pos)
             if (lineid=='brokn') call turtle__broken_line(pos,new=.true.)

             if ( j/=nt ) then
                ienter = 1
                inow = i
                jnow = j
                karte = 0
                do while ( karte==0 )
                   call lnfolw_polar(inow,jnow,ienter,karte,lineid,  &
                                     field,flagr,flagt)
                end do
             end if

             call turtle__move(pos)
             if (lineid=='brokn') call turtle__broken_line(pos,new=.true.)

             if( j/=1 ) then
                ienter = 4
                inow = i
                jnow = j
                karte = 0
                do while ( karte==0 )
                   call lnfolw_polar(inow,jnow,ienter,karte,lineid,  &
                                     field,flagr,flagt)
                end do
             end if
          end if
       end do
    end do

    deallocate(flagr,flagt)

  end subroutine cont0_polar



!_______________________________________________________________private__
!
  function coords_trans_polar_to_cartesian(rad,tht)
    real(SR), intent(in) :: rad, tht
    type(turtle__pos_t) :: coords_trans_polar_to_cartesian
!________________________________________________________________________
!
    coords_trans_polar_to_cartesian%x = rad*cos(tht)
    coords_trans_polar_to_cartesian%y = rad*sin(tht)

  end function coords_trans_polar_to_cartesian


!_______________________________________________________________private__
!
  function distance(x1,y1,x2,y2)
    real(SR), intent(in) :: x1, y1, x2, y2
    real(SR) :: distance
!________________________________________________________________________
!
    type(turtle__pos_t) :: pos1, pos2

    pos1%x = x1
    pos1%y = y1
    pos2%x = x2
    pos2%y = y2

    distance = distance_structure(pos1,pos2)

  end function distance


!_______________________________________________________________private__
!
  function distance_structure(pos1, pos2)
    type(turtle__pos_t), intent(in) :: pos1, pos2
    real(SR) :: distance_structure
!________________________________________________________________________
!
    distance_structure = sqrt((pos1%x-pos2%x)**2+(pos1%y-pos2%y)**2)

  end function distance_structure


!_______________________________________________________________private__
!
  subroutine lnfolw_cartesian(i,j,ienter,karte,lineid,field,flagx,flagy)
    integer, intent(inout) :: i, j, ienter
    integer, intent(inout) :: karte
    character(len=5), intent(in) :: lineid
    type(turtle__scalar2d_cartesian_t), intent(in) :: field
    integer, dimension(:,:), intent(inout) :: flagx, flagy
!________________________________________________________________________
!
!     purpose : elementally step for draw a contour line in
!               cartesian coordinanate.
!     note    : effectively recursive routine.
!     called  : cont0_cartesian
!     calls   : check_and_draw_cartesian
!
!     originally developed by Akira Kageyama in early 1990s in f77.
!     2005.11.23: Converted into f90 by Akira Kageyama.
!_______________________________________________________________________/
!
    integer, dimension(4,4), save :: iadd, jadd
    logical :: first = .true.
    integer :: i1, j1, iside, ifound

    if (first) then
       call iInit
       first = .false.
    end if

    do iside = 1 , 4
       if( ienter+iside == 5 ) cycle

       i1 = i + iadd(ienter,iside)
       j1 = j + jadd(ienter,iside)

       ifound = 0

       call check_and_draw_cartesian(i1,j1,iside,ifound,karte,lineid, &
                                     field,flagx,flagy)

       if ( ifound==1 ) then
          i = i1
          j = j1
          ienter = iside
          return
       end if
    end do

    karte = 99

  contains

    subroutine iInit
       iadd(1,1) = 0
       jadd(1,1) = 1
       iadd(1,2) = 1
       jadd(1,2) = 0
       iadd(1,3) = 0
       jadd(1,3) = 0

       iadd(2,1) = 0
       jadd(2,1) = 1
       iadd(2,2) = 1
       jadd(2,2) = 0
       iadd(2,4) = 0
       jadd(2,4) = 0

       iadd(3,1) = -1
       jadd(3,1) = 1
       iadd(3,3) = -1
       jadd(3,3) = 0
       iadd(3,4) = -1
       jadd(3,4) = 0

       iadd(4,2) = 1
       jadd(4,2) = -1
       iadd(4,3) = 0
       jadd(4,3) = -1
       iadd(4,4) = 0
       jadd(4,4) = -1
     end subroutine iInit

   end subroutine lnfolw_cartesian


!_______________________________________________________________private__
!
  subroutine lnfolw_polar(i,j,ienter,karte,lineid,field,flagr,flagt)
    integer, intent(inout) :: i, j, ienter
    integer, intent(inout) :: karte
    character(len=5), intent(in) :: lineid
    type(turtle__scalar2d_polar_t), intent(in) :: field
    integer, dimension(:,:), intent(inout) :: flagr, flagt
!________________________________________________________________________
!
!     purpose : elementally step for draw a contour line in
!               the polar-coordinanates.
!     note    : effectively recursive routine.
!     called  : cont0_polar
!     calls   : check_and_draw_polar
!
!     originally developed by Akira Kageyama in early 1990s in f77.
!
!     2005.11.24: Converted into f90 by Akira Kageyama.
!_______________________________________________________________________/
!
    integer, dimension(4,4), save :: iadd, jadd
    logical :: first = .true.
    integer :: i1, j1, iside, ifound

    if (first) then
       call iInit
       first = .false.
    end if

    do iside = 1 , 4
       if( ienter+iside == 5 ) cycle

       i1 = i + iadd(ienter,iside)
       j1 = j + jadd(ienter,iside)

       ifound = 0

       call check_and_draw_polar(i1,j1,iside,ifound,karte,lineid, &
                                 field,flagr,flagt)

       if ( ifound==1 ) then
          i = i1
          j = j1
          ienter = iside
          return
       end if
    end do

    karte = 99

  contains

    subroutine iInit
       iadd(1,1) = 0
       jadd(1,1) = 1
       iadd(1,2) = 1
       jadd(1,2) = 0
       iadd(1,3) = 0
       jadd(1,3) = 0

       iadd(2,1) = 0
       jadd(2,1) = 1
       iadd(2,2) = 1
       jadd(2,2) = 0
       iadd(2,4) = 0
       jadd(2,4) = 0

       iadd(3,1) = -1
       jadd(3,1) = 1
       iadd(3,3) = -1
       jadd(3,3) = 0
       iadd(3,4) = -1
       jadd(3,4) = 0

       iadd(4,2) = 1
       jadd(4,2) = -1
       iadd(4,3) = 0
       jadd(4,3) = -1
       iadd(4,4) = 0
       jadd(4,4) = -1
     end subroutine iInit

   end subroutine lnfolw_polar


!_______________________________________________________________private__
!
  subroutine line(xp,yp)
    real(SR), intent(in) :: xp, yp
!________________________________________________________________________
!
    real(SR) :: ox, oy

    if (.not.Initialization_done)  &
       call ut__fatal("turtle: You forgot initialization.")

    ox = Draw_area%origin%x
    oy = Draw_area%origin%y

    write(Unit_num_for_turtle,*) ox+xp, oy+yp

  end subroutine line


!_______________________________________________________________private__
!
  subroutine line_structure(pos)
    type(turtle__pos_t), intent(in) :: pos
!________________________________________________________________________
!
    call line(pos%x,pos%y)

  end subroutine line_structure


!_______________________________________________________________private__
!
  subroutine move(xp,yp)
    real(SR), intent(in) :: xp, yp
!________________________________________________________________________
!
    real(SR) :: ox, oy

    if (.not.Initialization_done)  &
       call ut__fatal("turtle: You forgot initialization.")

    ox = Draw_area%origin%x
    oy = Draw_area%origin%y

    write(Unit_num_for_turtle,*) ""
    write(Unit_num_for_turtle,*) ox+xp, oy+yp

  end subroutine move


!_______________________________________________________________private__
!
  subroutine move_other_coords(rad,tht,coords)
    real(SR), intent(in) :: rad, tht
    character(len=*), intent(in) :: coords
!________________________________________________________________________
!
    real(SR) :: x, y

    if (coords=='polar') then
       x = rad*cos(tht)
       y = rad*sin(tht)

       call move(x,y)
    else
       call ut__fatal('<turtle/move(_other_coords)>'//  &
                      ' This coordinate system is not supported:'//  &
                      coords)
    end if

  end subroutine move_other_coords


!_______________________________________________________________private__
!
  subroutine move_structure(pos)
    type(turtle__pos_t), intent(in) :: pos
!________________________________________________________________________
!
    call move(pos%x,pos%y)

  end subroutine move_structure


!_______________________________________________________________private__
!
  function operator_real_times_typepos(realsp,typepos)
    real(SR), intent(in) :: realsp
    type(turtle__pos_t), intent(in) :: typepos
    type(turtle__pos_t) :: operator_real_times_typepos
!________________________________________________________________________
!
    operator_real_times_typepos%x = realsp*(typepos%x)
    operator_real_times_typepos%y = realsp*(typepos%y)

  end function operator_real_times_typepos


!_______________________________________________________________private__
!
  function operator_typepos_div_real(pos,div)
    type(turtle__pos_t), intent(in) :: pos
    real(SR), intent(in) :: div
    type(turtle__pos_t) :: operator_typepos_div_real
!________________________________________________________________________
!
    operator_typepos_div_real%x = pos%x / div
    operator_typepos_div_real%y = pos%y / div

  end function operator_typepos_div_real


!_______________________________________________________________private__
!
  function operator_typepos_minus(a,b)
    type(turtle__pos_t), intent(in) :: a, b
    type(turtle__pos_t) :: operator_typepos_minus
!________________________________________________________________________
!
    operator_typepos_minus%x = a%x - b%x
    operator_typepos_minus%y = a%y - b%y

  end function operator_typepos_minus


!_______________________________________________________________private__
!
  function operator_typepos_plus(a,b)
    type(turtle__pos_t), intent(in) :: a, b
    type(turtle__pos_t) :: operator_typepos_plus
!________________________________________________________________________
!
    operator_typepos_plus%x = a%x + b%x
    operator_typepos_plus%y = a%y + b%y

  end function operator_typepos_plus


!_______________________________________________________________private__
!
  subroutine point(xp,yp)
    real(SR), intent(in) :: xp, yp
!________________________________________________________________________
!
    real(SR) :: ox, oy

    if (.not.Initialization_done)  &
       call ut__fatal("turtle: You forgot initialization.")

    ox = Draw_area%origin%x
    oy = Draw_area%origin%y

    write(Unit_num_for_turtle,*) ox+xp, oy+yp

  end subroutine point


!_______________________________________________________________private__
!
  subroutine point_structure(pos)
    type(turtle__pos_t), intent(in) :: pos
!________________________________________________________________________
!
    call point(pos%x,pos%y)

  end subroutine point_structure


!_______________________________________________________________private__
!
  subroutine rectangle_structure(pos_sw,pos_ne)
    type(turtle__pos_t), intent(in) :: pos_sw, pos_ne
!________________________________________________________________________
!
!                               pos_ne
!           +--------------------+
!           |                    |
!           |                    |
!           |                    |
!           |                    |
!           |                    |
!           +--------------------+
!        pos_sw
!
!
    call rectangle(pos_sw%x,pos_sw%y,pos_ne%x,pos_ne%y)

  end subroutine rectangle_structure


!_______________________________________________________________private__
!
  subroutine rectangle(xsw,ysw,xne,yne)         ! sw means south-west
    real(SR), intent(in) :: xsw, ysw, xne, yne  ! ne means north-east
!________________________________________________________________________
!
!                               (xne,yne)
!           +--------------------+
!           |                    |
!           |                    |
!           |                    |
!           |                    |
!           |                    |
!           +--------------------+
!      (xsw,ysw)
!
!
!
    type(turtle__pos_t) :: nw, ne, sw, se

    sw%x = xsw
    sw%y = ysw
    nw%x = xsw
    nw%y = yne
    ne%x = xne
    ne%y = yne
    se%x = xne
    se%y = ysw

    call move_structure(sw)
    call line_structure(nw)
    call line_structure(ne)
    call line_structure(se)
    call line_structure(sw)

  end subroutine rectangle


!
! Private
!===============
! Public
!


!________________________________________________________________public__
!
  subroutine turtle__contour_cartesian(field,nlevels,  &
                                       vmax_,vmin_)
    type(turtle__scalar2d_cartesian_t), intent(in) :: field
    integer, intent(in) :: nlevels
    real(SR), intent(in), optional :: vmax_, vmin_
!________________________________________________________________________
!
!
!     purpose : draw a contour line in x-y coordinate.
!
!     note    : nh     = number of equi-contour lines. (no bounded)
!               if nh < 0  define height here to be equi-incretmented.
!
!               height = levels of the drawing lines
!
!               output : vmax,vmin, height(if nh<0)
!
!
!     calls   : cont0_cartesian
!
!
!     by a. kageyama    92.03.07 (modified from pcontr)
!
!-----
!     2005.11.23: Converted into f90 by Akira Kageyama
!_______________________________________________________________________/
!
    real(SR) :: vmax, vmin
    integer  :: l
    type(turtle__scalar2d_cartesian_t) :: field0
    real(SR) :: dval
    real(SR), dimension(:), allocatable :: level
    integer :: nx, ny

    nx = field%nx
    ny = field%ny

    field0%nx = nx
    field0%ny = ny

    allocate(field0%f(nx,ny))
    allocate(field0%xpos(nx))
    allocate(field0%ypos(ny))

    if ( nlevels==0 ) then
       deallocate(field0%f)
       deallocate(field0%xpos)
       deallocate(field0%ypos)
       return
    end if

    allocate(level(nlevels))

    !
    !---- max,min check ----
    !
    if ( present(vmax_) .and. present(vmin_) ) then
       vmax = vmax_
       vmin = vmin_
    else
       vmax = maxval(abs(field%f(:,:)))
       vmin = -vmax
    end if

    call ut__message('* in contr_cartesian ....   vmax = ',vmax)
    call ut__message('*                           vmin = ',vmin)

    if ( vmax==vmin ) then
       call ut__message(' #######  in contr_cartesian, vmax = vmin. #######')
       return
    end if

    if ( nlevels==1) then
       call ut__message('#','turtle/contr: nlevels==1 is not allowed.')
       deallocate(field0%f)
       deallocate(field0%xpos)
       deallocate(field0%ypos)
       deallocate(level)
       return
    end if

    dval = (vmax-vmin)/real(nlevels)
    do l = 1 , nlevels
       level(l) = vmin + dval*(real(l)-0.5)
    end do

    !
    !---- main part ----
    !
    field0%xpos(:) = field%xpos(:)
    field0%ypos(:) = field%ypos(:)

    do l = 1 , nlevels
       field0%f(:,:) = field%f(:,:) - level(l)

       if ( level(l) >= 0. ) then
          call cont0_cartesian(field0,'solid')
       else
          call cont0_cartesian(field0,'brokn')
       end if
    end do

    deallocate(field0%f)
    deallocate(field0%xpos)
    deallocate(field0%ypos)
    deallocate(level)

  end subroutine turtle__contour_cartesian


!________________________________________________________________public__
!
  subroutine turtle__contour_polar(field,nlevels,vmax_,vmin_)
    type(turtle__scalar2d_polar_t), intent(in) :: field
    integer, intent(in) :: nlevels
    real(SR), intent(in), optional :: vmax_, vmin_
!________________________________________________________________________
!
!     purpose : draw a contour line in the polar-coordinates.
!     note    :
!     calls   : cont0_polar
!
!
!     by a. kageyama    92.03.07 (modified from pcontr)
!
!-----
!     2005.11.24: Converted into f90 by Akira Kageyama
!________________________________________________________________________
!
    real(SR) :: vmax, vmin
    integer  :: l
    type(turtle__scalar2d_polar_t) :: field0
    real(SR) :: dval
    real(SR), dimension(:), allocatable :: level
    integer :: nr, nt

    nr = field%nr
    nt = field%nt

    field0%nr = nr
    field0%nt = nt

    allocate(field0%f(nr,nt))
    allocate(field0%rpos(nr))
    allocate(field0%tpos(nt))

    if ( nlevels==0 ) then
       deallocate(field0%f)
       deallocate(field0%rpos)
       deallocate(field0%tpos)
       return
    end if

    allocate(level(nlevels))

    !
    !---- max,min check ----
    !
    if ( present(vmax_) .and. present(vmin_) ) then
       vmax = vmax_
       vmin = vmin_
    else
       vmax = maxval(field%f(:,:))
       vmin = minval(field%f(:,:))
    end if

    call ut__message('* in contr ....   vmax = ',vmax)
    call ut__message('*                 vmin = ',vmin)

    if ( vmax==vmin ) then
       call ut__message(' #######  in contr, vmax = vmin. #######')
       return
    end if

    if ( nlevels==1) then
       call ut__message('#','turtle/contr: nlevels==1 is not allowed.')
       deallocate(field0%f)
       deallocate(field0%rpos)
       deallocate(field0%tpos)
       deallocate(level)
       return
    end if
    dval = 0.96*(vmax-vmin)/real(nlevels-1)

    do l = 1 , nlevels
       level(l) = vmin + 0.02*(vmax-vmin) + dval*real(l-1)
    end do
    !
    !---- main part ----
    !
    field0%rpos(:) = field%rpos(:)
    field0%tpos(:) = field%tpos(:)

    do l = 1 , nlevels
       field0%f(:,:) = field%f(:,:) - level(l)

       if ( level(l) >= 0. ) then
          call cont0_polar(field0,'solid')
       else
          call cont0_polar(field0,'brokn')
       end if
    end do

    deallocate(field0%f)
    deallocate(field0%rpos)
    deallocate(field0%tpos)
    deallocate(level)

  end subroutine turtle__contour_polar


!_______________________________________________________________private__
!
  subroutine turtle__coords_shift(shift)
    type(turtle__pos_t), intent(in) :: shift
!________________________________________________________________________
!
    Draw_area%origin%x = Draw_area%origin%x + shift%x
    Draw_area%origin%y = Draw_area%origin%y + shift%y

    print *,' Draw_area%origin%x = ', Draw_area%origin%x
    print *,' Draw_area%origin%y = ', Draw_area%origin%y

  end subroutine turtle__coords_shift


!________________________________________________________________public__
!
  function turtle__filename_for_lines(tag)
    character(len=*), intent(in) :: tag
    character(len=200) :: turtle__filename_for_lines
!________________________________________________________________________
!
    !<< e.g., file = "./lines.temp.tt" >>!
    turtle__filename_for_lines = trim(DIR_FOR_PLOT_DATA)  &
                               //CHAR_SLASH  &
                               //"lines"  &
                               //CHAR_DOT  &
                               //trim(tag)  &
                               //CHAR_DOT  &
                               //EXT_FOR_PLOT_DATA

  end function turtle__filename_for_lines


!________________________________________________________________public__
!
  subroutine turtle__initialize(unit_num,  &
                                window_lower_left, &
                                window_upper_rite)
    integer, intent(in) :: unit_num
    type(turtle__pos_t), intent(in) :: window_lower_left,  &
                                       window_upper_rite
!________________________________________________________________________
!
    real(SR) :: xmin, xmax, ymin, ymax

    Unit_num_for_turtle = unit_num

    xmin = window_lower_left%x
    xmax = window_upper_rite%x
    ymin = window_lower_left%y
    ymax = window_upper_rite%y

    Draw_area%corner_north_west%x = xmin
    Draw_area%corner_north_west%y = ymax
    Draw_area%corner_north_east%x = xmax
    Draw_area%corner_north_east%y = ymax
    Draw_area%corner_south_west%x = xmin
    Draw_area%corner_south_west%y = ymin
    Draw_area%corner_south_east%x = xmax
    Draw_area%corner_south_east%y = ymin

    Draw_area%origin%x = (xmax+xmin)/2
    Draw_area%origin%y = (ymax+ymin)/2

    Draw_area%diag_length = turtle__distance(xmin,ymin,xmax,ymax)

    call ut__message('Draw_area:xmin = ', xmin)
    call ut__message('Draw_area:ymin = ', ymin)
    call ut__message('Draw_area:xmax = ', xmax)
    call ut__message('Draw_area:ymax = ', ymax)
    call ut__message('Draw_area:diag_length = ', Draw_area%diag_length)

    Initialization_done = .true.

  end subroutine turtle__initialize


!________________________________________________________________public__
!
  subroutine turtle__vector_cartesian(vector,norm)
    type(turtle__vector2d_cartesian_t), intent(in) :: vector
    real(SR), optional, intent(in) :: norm
!________________________________________________________________________
!
    integer  :: i, j
    real(SR) :: max_vec_amp
    type(turtle__pos_t) :: place, arrow
    real(SR) :: factor

    if (present(norm)) then
       factor = 1.0_SR / norm
    else
       max_vec_amp = iCalc_amplitude()
       call ut__message("# max vel = ", max_vec_amp)
       factor = 0.1_SR / max_vec_amp
    end if

    do i = 1 , vector%nx 
       do j = 1 , vector%ny 
          place%x = vector%xpos(i)
          place%y = vector%ypos(j)
          arrow%x = factor*vector%x(i,j)
          arrow%y = factor*vector%y(i,j)
          call turtle__arrow(place,arrow)
       end do
    end do

  contains

    function iCalc_amplitude()
      real(SR) :: iCalc_amplitude

      iCalc_amplitude = maxval(sqrt(vector%x(:,:)**2 + vector%y(:,:)**2))
    end function iCalc_amplitude

  end subroutine turtle__vector_cartesian


!________________________________________________________________public__
!
  subroutine turtle__vector_polar(vector,norm)
    type(turtle__vector2d_polar_t), intent(in) :: vector
    real(SR), optional, intent(in) :: norm
!________________________________________________________________________
!
    integer  :: i, j
    real(SR) :: rad, tht, max_vec_amp
    type(turtle__pos_t) :: place, arrow
    real(SR) :: factor

    if (present(norm)) then
       if ( norm > 0.0_SR ) then
          factor = 1.0_SR / norm
       else
          max_vec_amp = iCalc_amplitude()
          call ut__message("# max vel = ", max_vec_amp)
          factor = 0.1_SR / max_vec_amp
       end if
    else
       factor = 1.0_SR
    end if

    do i = 5 , vector%nr , 5
       do j = 5 , vector%nt , 5
          rad = vector%rpos(i)
          tht = vector%tpos(j)
          place%x = rad*cos(tht)
          place%y = rad*sin(tht)
          arrow%x = factor*vector%x(i,j)
          arrow%y = factor*vector%y(i,j)
          call turtle__arrow(place,arrow)
       end do
    end do

  contains

    function iCalc_amplitude()
      real(SR) :: iCalc_amplitude

      iCalc_amplitude = maxval(sqrt(vector%x(:,:)**2 + vector%y(:,:)**2))
    end function iCalc_amplitude

  end subroutine turtle__vector_polar

end module turtle_m
