!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! params.f90
!     2008.06.02: Developed by Akira Kageyama. Copied from kindanb.
!     2018.04.12: Copied from boxfluid.
!-----------------------------------------------------------------------------

module params_m
  use constants_m
  use ut_m
  implicit none
  private
  public :: & !< routines >!
            params__get_double,   &
            params__get_integer,  &
            params__get_logical,  &
            params__read,         &
            params__get_string

  logical, save :: Read_done = .false.

  integer(SI), parameter :: STRING_LENGTH_MAX = 200

  integer(SI) :: Total_nloop
  integer(SI) :: Slicedata_nskip
  character(len=STRING_LENGTH_MAX) :: Slicedata_tag
  real(DR) :: Viscosity, Kappa
  logical  :: Debug

  namelist /simulation/     Total_nloop
  namelist /visualization/  Slicedata_nskip,  Slicedata_tag
  namelist /fluid_property/ Viscosity, Kappa
  namelist /flags/          Debug


contains

  function params__get_double(variable)
    character(len=*), intent(in) :: variable
    real(DR) :: params__get_double

    call ut__assert(Read_done, &
                    '<params__get_double> Read params file first.')

    select case (variable)
      case                 ('Kappa')
        params__get_double = Kappa
      case                 ('Viscosity')
        params__get_double = Viscosity
      case default
        call ut__message('? arg = ', variable)
        call ut__fatal('<params__get_double> not in the params?')
    end select
  end function params__get_double


  function params__get_integer(variable)
    character(len=*), intent(in) :: variable
    integer(SI) :: params__get_integer

    call ut__assert(Read_done, &
                    '<params__get_integer> Read params file first.')

    select case (variable)
      case                  ('Slicedata_nskip')
        params__get_integer = Slicedata_nskip
      case                  ('Total_nloop')
        params__get_integer = Total_nloop
      case default
        call ut__message('? arg = ', variable)
        call ut__fatal('<params__get_integer> not in the params?')
    end select
  end function params__get_integer


  function params__get_logical(variable)
    character(len=*), intent(in) :: variable
    logical :: params__get_logical

    call ut__assert(Read_done, &
                    '<params__get_logical> Read params file first.')

    select case (variable)
      case                  ('Debug')
        params__get_logical = Debug
      case default
        call ut__message('? arg = ', variable)
        call ut__fatal('<params__get_logical> not in the params?')
    end select
  end function params__get_logical


  subroutine params__read
    character(len=STRING_LENGTH_MAX) :: params_file

    call ut__assert(command_argument_count()==1, &
                    "Usage: smoke_ring param_file")
    call get_command_argument(1,params_file)

    open(10,file=trim(params_file))
      read(10,nml=simulation)
      read(10,nml=visualization)
      read(10,nml=fluid_property)
      read(10,nml=flags)
    close(10)

    write(6,nml=simulation)
    write(6,nml=visualization)
    write(6,nml=fluid_property)
    write(6,nml=flags)

    Read_done = .true.
  end subroutine params__read


  function params__get_string(variable)
    character(len=*), intent(in) :: variable
    character(len=STRING_LENGTH_MAX) :: params__get_string

    call ut__assert(Read_done, &
                    '<params__get_string> Read params file first.')

    select case         (variable)
      case                ('Slicedata_tag')
        params__get_string = Slicedata_tag
      case default
        call ut__message('? arg = ', variable)
        call ut__fatal('<params__get_string> not in the params?')
    end select
  end function params__get_string

end module params_m
