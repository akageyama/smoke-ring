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
!    src/solver.f90
!-------------------------------------------------------------------

module solver_m
  use constants_m
  use grid_m
  use ut_m
  use params_m
  use field_m
  use debug_m
  use job_m
  implicit none

  private
  public :: solver__advance,  &
            solver__diagnosis,  &
            solver__get_subfield,  &
            solver__initialize,  &
            solver__set_time_step

  interface solver__get_subfield
    module procedure subfield_vel,  &
                     subfield_vel_tm,  &
                     subfield_vel_tm_divv
  end interface

  real(DR), parameter :: GAMMA = 5.0_DR / 3.0_DR ! ratio of specific heats.
  real(DR), parameter :: GASS_CONST_FOR_AIR = 2.87e2_DR
  !  Equation of state for the air:
  !     Pressure = 287 * Mass_density * Temperature

  logical,  save :: Initialize_done = .false.
  real(DR), save :: Viscosity
  real(DR), save :: Gamma1_kappa   ! (gamma-1)*kappa
  type(field__vector3d_t), save :: Drive_force


contains


  function drive_force_factor(time)
    real(DR), intent(in) :: time
    real(DR)             :: drive_force_factor
    !
    !                                      factor
    !              ___________               |
    !             /|         |\              |
    !      ______/ |         | \______       +--------> time
    !            | |         |  |
    !            | |         |  |
    !            | t0        t1 |
    !          t_start         t_end
    !
    real(DR), parameter :: T_START =  0.0_DR
!   real(DR), parameter :: T_END   =  5.0_DR
    real(DR), parameter :: T_END   =  0.01_DR
                                   ! Find proper value by trials & erros.
    real(DR), parameter :: T0 = T_START + (T_END-T_START)/4
    real(DR), parameter :: T1 = T_END   - (T_END-T_START)/4

    real(DR), parameter :: ONE  = 1.0_DR
    real(DR), parameter :: ZERO = 0.0_DR

    call ut__assert( T_START < T0 .and. T0 < T1 .and. T1 < T_END,  &
                    "<solver/drive_force_factor> Time inconsistent.")

    if ( time <= T_START ) then
      drive_force_factor = ZERO
    else if ( time <= T0 ) then
      drive_force_factor = (time-T_START) / (T0-T_START)
    else if ( time <= T1 ) then
      drive_force_factor = ONE
    else if ( time <= T_END ) then
      drive_force_factor = ONE - (time-T1) / (T_END-T1)
    else
      drive_force_factor = ZERO
    end if

    call ut__assert(drive_force_factor >=0.0_DR  &
                              .and.  &
                    drive_force_factor <=1.0_DR,  &
                    "<solver/drive_force_factor> strange value.")
  end function drive_force_factor


  subroutine set_drive_force_field
    integer(SI) :: i, j, k
    real(DR) :: xx, yy, zz
    real(DR) :: force_region_x_min, force_region_x_max
    real(DR) :: force_center_y, force_center_z
    real(DR) :: force_cylinder_diameter, force_cylinder_radius_sq

    real(DR), parameter :: THE_FORCE = 3.e3_DR
                                     ! Find proper value by trials & erros.
    !
    !     +--------------------------------------+ ZMAX
    !     |                                      |
    !     |    +-------+                         |
    !     |    | Force |                         |
    !     |    +-------+                         |
    !     |                                      |
    !     +--------------------------------------+ ZMIN
    !    XMIN                                   XMAX
    !
    force_region_x_min = XMIN + (XMAX-XMIN)/5
    force_region_x_max = force_region_x_min + (XMAX-XMIN)/10
    force_center_y = (YMAX + YMIN) / 2
    force_center_z = (ZMAX + ZMIN) / 2
    force_cylinder_diameter  = min(YMAX-YMIN, ZMAX-ZMIN) / 4
    force_cylinder_radius_sq = (force_cylinder_diameter/2)**2

    do k = 2 , NZ-1
      zz = grid%pos%z(k) - force_center_z
      do j = 2 , NY-1
        yy = grid%pos%y(j) - force_center_y
        do i = 2 , NX-1
          xx = grid%pos%x(i)
          if ( (yy**2+zz**2) < force_cylinder_radius_sq  &
                        .and.  &
               (xx > force_region_x_min)  &
                        .and.  &
               (xx < force_region_x_max) ) then
            Drive_force%x(i,j,k) = THE_FORCE
            Drive_force%y(i,j,k) = 0.0_DR
            Drive_force%z(i,j,k) = 0.0_DR
          else
            Drive_force%x(i,j,k) = 0.0_DR
            Drive_force%y(i,j,k) = 0.0_DR
            Drive_force%z(i,j,k) = 0.0_DR
          end if
        end do
      end do
    end do

    call field__boundary_condition(Drive_force)

    call ut__assert(maxval(Drive_force%x)==THE_FORCE,  &
                    "<solver/set_drive_force_field> something is wrong.")

    call debug__print("called solver/set_drive_force_field.")
  end subroutine set_drive_force_field


  subroutine subfield_vel(fluid,vel)
    type(field__fluid_t),    intent(in)  :: fluid
    type(field__vector3d_t), intent(out) :: vel

!>  vel = fluid%flux / fluid%density     ! operator defined in field.
    vel = operator_vector_divby_scalar(fluid%flux, fluid%density)

    call debug__print("called solver/subfield_vel.")
  end subroutine subfield_vel


  subroutine subfield_vel_tm(fluid,vel,tm)
    type(field__fluid_t),          intent(in)  :: fluid
    type(field__vector3d_t),       intent(out) :: vel
    real(DR), dimension(NX,NY,NZ), intent(out) :: tm

!>   vel = fluid%flux     / fluid%density ! operator defined in field.f90.
     vel = operator_vector_divby_scalar(fluid%flux, fluid%density)
      tm = fluid%pressure / (GASS_CONST_FOR_AIR*fluid%density)

    call debug__print("called solver/subfield_vel_tm.")
  end subroutine subfield_vel_tm


  subroutine subfield_vel_tm_divv(fluid,vel,tm,divv)
    type(field__fluid_t),          intent(in)  :: fluid
    type(field__vector3d_t),       intent(out) :: vel
    real(DR), dimension(NX,NY,NZ), intent(out) :: tm
    real(DR), dimension(NX,NY,NZ), intent(out) :: divv

!>   vel = fluid%flux     / fluid%density ! operator defined in field.f90.
     vel = operator_vector_divby_scalar(fluid%flux, fluid%density)
      tm = fluid%pressure / fluid%density
!>  divv = .div.vel
    divv = operator_div(vel)

    call debug__print("called solver/subfield_vel_tm_divv.")
  end subroutine subfield_vel_tm_divv


  function the_equation(t,dt,vx,vy,vz,tm,divv,fx,fy,fz,ps)
    real(DR),                      intent(in) :: t, dt
    real(DR), dimension(NX,NY,NZ), intent(in) :: vx, vy, vz
    real(DR), dimension(NX,NY,NZ), intent(in) :: tm, divv
    real(DR), dimension(NX,NY,NZ), intent(in) :: fx, fy, fz
    real(DR), dimension(NX,NY,NZ), intent(in) :: ps
    type(field__fluid_t) :: the_equation
    !
    !  Here we ignore the viscous heating term in the presssure equation.
    !
    integer(SI) :: i, j, k
    real(DR), parameter :: ONE_THIRD = 1.0_DR / 3.0_DR
    real(DR) :: gradpx, gradpy, gradpz
    real(DR) :: gdivvx, gdivvy, gdivvz
    real(DR) :: divfvx, divfvy, divfvz
    real(DR) :: lapvx, lapvy, lapvz, laptm
    real(DR) :: divf
    real(DR) :: factor

    call ut__assert(Initialize_done, "<solver/the_equation> Forgot init?")

    factor = drive_force_factor(t)
    !
    ! Since the following do-loops are the most time consuming
    ! part in this simulation, we do not use fancy operators
    ! such as .div., for the easiy optimization in future.
    !
    do k = 2 , NZ-1
      do j = 2 , NY-1
        do i = 2 , NX-1
          gradpx = ( ps(i+1,j,k)-ps(i-1,j,k) ) * grid%d1%x
          gradpy = ( ps(i,j+1,k)-ps(i,j-1,k) ) * grid%d1%y
          gradpz = ( ps(i,j,k+1)-ps(i,j,k-1) ) * grid%d1%z

          gdivvx = ( divv(i+1,j,k)-divv(i-1,j,k) ) * grid%d1%x
          gdivvy = ( divv(i,j+1,k)-divv(i,j-1,k) ) * grid%d1%y
          gdivvz = ( divv(i,j,k+1)-divv(i,j,k-1) ) * grid%d1%z

          divfvx = ( fx(i+1,j,k)*vx(i+1,j,k)  &
                    -fx(i-1,j,k)*vx(i-1,j,k) ) * grid%d1%x  &
                 + ( fx(i,j+1,k)*vy(i,j+1,k)  &
                    -fx(i,j-1,k)*vy(i,j-1,k) ) * grid%d1%y  &
                 + ( fx(i,j,k+1)*vz(i,j,k+1)  &
                    -fx(i,j,k-1)*vz(i,j,k-1) ) * grid%d1%z
          divfvy = ( fy(i+1,j,k)*vx(i+1,j,k)  &
                    -fy(i-1,j,k)*vx(i-1,j,k) ) * grid%d1%x  &
                 + ( fy(i,j+1,k)*vy(i,j+1,k)  &
                    -fy(i,j-1,k)*vy(i,j-1,k) ) * grid%d1%y  &
                 + ( fy(i,j,k+1)*vz(i,j,k+1)  &
                    -fy(i,j,k-1)*vz(i,j,k-1) ) * grid%d1%z
          divfvz = ( fz(i+1,j,k)*vx(i+1,j,k)  &
                    -fz(i-1,j,k)*vx(i-1,j,k) ) * grid%d1%x  &
                 + ( fz(i,j+1,k)*vy(i,j+1,k)  &
                    -fz(i,j-1,k)*vy(i,j-1,k) ) * grid%d1%y  &
                 + ( fz(i,j,k+1)*vz(i,j,k+1)  &
                    -fz(i,j,k-1)*vz(i,j,k-1) ) * grid%d1%z

          lapvx = ( vx(i+1,j,k)-2*vx(i,j,k)+vx(i-1,j,k) )*grid%d2%x &
                + ( vx(i,j+1,k)-2*vx(i,j,k)+vx(i,j-1,k) )*grid%d2%y &
                + ( vx(i,j,k+1)-2*vx(i,j,k)+vx(i,j,k-1) )*grid%d2%z
          lapvy = ( vy(i+1,j,k)-2*vy(i,j,k)+vy(i-1,j,k) )*grid%d2%x &
                + ( vy(i,j+1,k)-2*vy(i,j,k)+vy(i,j-1,k) )*grid%d2%y &
                + ( vy(i,j,k+1)-2*vy(i,j,k)+vy(i,j,k-1) )*grid%d2%z
          lapvz = ( vz(i+1,j,k)-2*vz(i,j,k)+vz(i-1,j,k) )*grid%d2%x &
                + ( vz(i,j+1,k)-2*vz(i,j,k)+vz(i,j-1,k) )*grid%d2%y &
                + ( vz(i,j,k+1)-2*vz(i,j,k)+vz(i,j,k-1) )*grid%d2%z

          laptm = ( tm(i+1,j,k)-2*tm(i,j,k)+tm(i-1,j,k) )*grid%d2%x &
                + ( tm(i,j+1,k)-2*tm(i,j,k)+tm(i,j-1,k) )*grid%d2%y &
                + ( tm(i,j,k+1)-2*tm(i,j,k)+tm(i,j,k-1) )*grid%d2%z

          divf = ( fx(i+1,j,k)-fx(i-1,j,k) ) * grid%d1%x  &
               + ( fy(i,j+1,k)-fy(i,j-1,k) ) * grid%d1%y  &
               + ( fz(i,j,k+1)-fz(i,j,k-1) ) * grid%d1%z

          the_equation%density(i,j,k) = -divf*dt

          the_equation%flux%x(i,j,k) =  &
               ( - divfvx  &
                 - gradpx  &
                 + Drive_force%x(i,j,k)*factor  &
                 + Viscosity * ( lapvx + ONE_THIRD*gdivvx )  &
               ) * dt
          the_equation%flux%y(i,j,k) =  &
               ( - divfvy  &
                 - gradpy  &
                 + Drive_force%y(i,j,k)*factor  &
                 + Viscosity * ( lapvy + ONE_THIRD*gdivvy )  &
               ) * dt
          the_equation%flux%z(i,j,k) =  &
               ( - divfvz  &
                 - gradpz  &
                 + Drive_force%z(i,j,k)*factor  &
                 + Viscosity * ( lapvz + ONE_THIRD*gdivvz )  &
               ) * dt

          the_equation%pressure(i,j,k) =  &
              ( - ( vx(i,j,k)*gradpx  &
                  + vy(i,j,k)*gradpy  &
                  + vz(i,j,k)*gradpz  &
                  )  &
                + Gamma1_kappa * laptm  &
                - GAMMA * ps(i,j,k) * divv(i,j,k)  &
              ) * dt
        end do
      end do
    end do

    call field__boundary_condition(the_equation)
    call debug__print("called solver/the_equation.")
  end function the_equation



!
! Private
!===============
! Public
!


  subroutine solver__advance(t,dt,fluid)
    real(DR), intent(inout) :: t
    real(DR), intent(in)    :: dt
    type(field__fluid_t), intent(inout)  :: fluid
    !
    !   The classical 4-step, 4-th order Runge-Kutta method.
    !
    real(DR), parameter :: ONE_SIXTH = 1.0_DR / 6.0_DR
    real(DR), parameter :: ONE_THIRD = 1.0_DR / 3.0_DR

    type(field__vector3d_t)       :: vel
    real(DR), dimension(NX,NY,NZ) :: tm
    real(DR), dimension(NX,NY,NZ) :: divv

    type(field__fluid_t) :: dfluid01, dfluid02, dfluid03, dfluid04
    type(field__fluid_t) :: gluid  ! work variable


    !--< step 1 >--!
    call subfield_vel_tm_divv(fluid,vel,tm,divv)
    dfluid01 = the_equation(t, dt,  &
                            vel%x, vel%y, vel%z, tm, divv,  &
                            fluid%flux%x, fluid%flux%y, fluid%flux%z,  &
                            fluid%pressure)

    t = t + dt/2
    !--< step 2 >--!
!>  gluid = fluid + dfluid01*0.5_DR
    dfluid01 = operator_fluid_times_real(dfluid01,0.5_DR)
    gluid    = operator_fluid_add(fluid,dfluid01)

    call subfield_vel_tm_divv(gluid,vel,tm,divv)
    dfluid02 = the_equation(t, dt,  &
                            vel%x, vel%y, vel%z, tm, divv,  &
                            gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                            gluid%pressure)

    !--< step 3 >--!
!>  gluid = fluid + dfluid02*0.5_DR
    dfluid02 = operator_fluid_times_real(dfluid02,0.5_DR)
    gluid    = operator_fluid_add(fluid,dfluid02)

    call subfield_vel_tm_divv(gluid,vel,tm,divv)
    dfluid03 = the_equation(t, dt,  &
                            vel%x, vel%y, vel%z, tm, divv,  &
                            gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                            gluid%pressure)

    t = t + dt/2
    !--< step 4 >--!
!>  gluid = fluid + dfluid03
    gluid = operator_fluid_add(fluid,dfluid03)

    call subfield_vel_tm_divv(gluid,vel,tm,divv)
    dfluid04 = the_equation(t, dt,  &
                            vel%x, vel%y, vel%z, tm, divv,  &
                            gluid%flux%x, gluid%flux%y, gluid%flux%z,  &
                            gluid%pressure)

    !--< resuts >--!

!>  fluid = fluid  &
!>        + ONE_SIXTH*( dfluid01 + 2*dfluid02 + 2*dfluid03 + dfluid04 )
    dfluid01 = operator_fluid_times_real(dfluid01,ONE_SIXTH)
    dfluid02 = operator_fluid_times_real(dfluid02,ONE_THIRD)
    dfluid03 = operator_fluid_times_real(dfluid03,ONE_THIRD)
    dfluid04 = operator_fluid_times_real(dfluid04,ONE_SIXTH)
    fluid = operator_fluid_add(fluid,dfluid01)
    fluid = operator_fluid_add(fluid,dfluid02)
    fluid = operator_fluid_add(fluid,dfluid03)
    fluid = operator_fluid_add(fluid,dfluid04)

    call debug__print("called solver__advance.")
  end subroutine solver__advance


  subroutine solver__diagnosis(nloop,time,fluid)
    integer(DI),          intent(in)    :: nloop
    real(DR),             intent(in)    :: time
    type(field__fluid_t), intent(in)    :: fluid

    integer(SI), parameter :: SKIP = 100
    real(DR), parameter :: ABNORMALLY_LARGE = 1.e20_DR

    type(field__vector3d_t) :: vel

    if ( mod(nloop,SKIP) /= 0 ) return

    if ( job__karte%state /= "fine" ) return ! Already in error state.

    if ( maxval(fluid%flux%x) > ABNORMALLY_LARGE ) then
      call ut__message("<solver__diagnosis> Massflux_x overflow.")
      call job__karte%set("over_flow")
      return
    end if

    if ( maxval(fluid%flux%y) > ABNORMALLY_LARGE ) then
      call ut__message("<solver__diagnosis> Massflux_y overflow.")
      call job__karte%set("over_flow")
      return
    end if

    if ( maxval(fluid%flux%z) > ABNORMALLY_LARGE ) then
      call ut__message("<solver__diagnosis> Massflux_z overflow.")
      call job__karte%set("over_flow")
      return
    end if

    if ( maxval(fluid%density) > ABNORMALLY_LARGE ) then
      call ut__message("<solver__diagnosis> Density overflow.")
      call job__karte%set("over_flow")
      return
    end if

    if ( maxval(fluid%pressure) > ABNORMALLY_LARGE ) then
      call ut__message("<solver__diagnosis> Pressure overflow.")
      call job__karte%set("over_flow")
      return
    end if

    if ( minval(fluid%pressure) < 0.0_DR ) then
      call ut__message("<solver__diagnosis> Negative pressure.")
      call job__karte%set("negative_anormaly")
      return
    end if

    if ( minval(fluid%density) < 0.0_DR ) then
      call ut__message("<solver__diagnosis> Negative density.")
      call job__karte%set("negative_anormaly")
      return
    end if


    call subfield_vel(fluid,vel)

    call ut__message('#max vel:',      nloop, time,  &
                     sqrt(maxval(vel%x**2+vel%y**2+vel%z**2)))
    call ut__message('#flow energy: ', nloop, time,  &
!>                                    .energyintegral.fluid)
                                       operator_energyintegral(fluid))
    call ut__message('#total mass: ',  nloop, time,  &
!>                                    .scalarintegral.(fluid%density))
                               operator_scalarintegral(fluid%density))

    call debug__print('called solver__diagnosis.')
  end subroutine solver__diagnosis


  subroutine solver__initialize(fluid)
    type(field__fluid_t), intent(out) :: fluid

    real(DR) :: kappa

    !<< Physical parameters >>!
    Viscosity = params__get_double('Viscosity')
    kappa     = params__get_double('Kappa')         ! Thermal diffusivity

    Gamma1_kappa = (Gamma-1)*kappa

    !<< Initial condition of the fluid >>!
    fluid%pressure = 1.013e5_DR  ! 1013 hPa (air pressure)
    fluid%density  = 1.293_DR    ! kg/m^3 (air density)
!>  fluid%flux     = 0.0_DR      ! no flow at t=0
    fluid%flux%x   = 0.0_DR      ! no flow at t=0
    fluid%flux%y   = 0.0_DR      ! no flow at t=0
    fluid%flux%z   = 0.0_DR      ! no flow at t=0

    !<< Define drive force field >>!
    call set_drive_force_field

    Initialize_done = .true.

    call debug__print("called solver__initialize.")
  end subroutine solver__initialize


  function solver__set_time_step(nloop,fluid)
    integer(DI),          intent(in) :: nloop
    type(field__fluid_t), intent(in) :: fluid
    real(DR) :: solver__set_time_step
    !
    !   set dt by the CFL condition.
    !
    type(field__vector3d_t)       :: vel
    real(DR), dimension(NX,NY,NZ) :: tm

    real(DR) :: vmax, sound_v
    real(DR) :: dt_vel, dt_sound, dt_viscous, dt_kappa
    real(DR), parameter :: ALMOST_ZERO = 1.e-20_DR
    real(DR), parameter :: ABNORMAL_VALUE = -999.999_DR
    real(DR), save :: dt = ABNORMAL_VALUE  ! To detect error.
    integer(SI), parameter :: SKIP = 20            ! Recalc dt every this cycle.

    call ut__assert(Initialize_done,"<solver__set_tim_step> Forgot init?")

    if ( mod(nloop,SKIP)==0 ) then              ! Otherwise, we recycle
                                                !       previous value.
      call subfield_vel_tm(fluid,vel,tm)

      vmax = maxval(sqrt(vel%x**2+vel%y**2+vel%z**2))
      vmax = max(vmax,ALMOST_ZERO)             ! For case of no flow, at t=0.

      sound_v = GAMMA*maxval(sqrt(GASS_CONST_FOR_AIR*tm)) ! Speed of sound.

      call ut__assert(sound_v > ALMOST_ZERO,"<solver__time_step> sound_v=0?")

      dt_vel     = 0.8_DR*grid%delta_min/vmax
      dt_sound   = 0.8_DR*grid%delta_min/sound_v
      dt_viscous = 0.2_DR*(grid%delta_min**2)/Viscosity
      dt_kappa   = 0.2_DR*(grid%delta_min**2)/Gamma1_kappa    ! A rough estimate.

      dt = min(dt_vel, dt_sound, dt_viscous, dt_kappa)

      if ( params__get_logical('Debug') ) then
        call ut__message('<solver__time_step> vmax = ', vmax       )
        call ut__message('                  dt_vel = ', dt_vel     )
        call ut__message('                dt_sound = ', dt_sound   )
        call ut__message('                dt_kappa = ', dt_kappa   )
        call ut__message('              dt_viscous = ', dt_viscous )
        call ut__message('               -->    dt = ', dt         )
      end if

      if ( mod(nloop,SKIP*50)==0 )  call ut__message("> dt = ", dt)

    end if

    call ut__assert(dt /= ABNORMAL_VALUE, "<solver__time_step> dt init failed?")

    solver__set_time_step = dt    ! dt of the prev calc is saved.
  end function solver__set_time_step

end module solver_m
