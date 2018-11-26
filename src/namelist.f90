!-----------------------------------------------------------------------------
! smoke-ring: A simple 3-D Fluid Solver by FDM on Cartesian Grid.
!
!    by Akira Kageyama,
!       Department of Computational Science,
!       Kobe University, Japan.
!       email: kage@port.kobe-u.ac.jp or sgks@mac.com
!-----------------------------------------------------------------------------
! namelist.f90
!     2008.06.02: Developed by Akira Kageyama. Copied from kindanb.
!     2018.04.12: Copied from boxfluid.
!-----------------------------------------------------------------------------

module namelist_m
  use constants_m
  use ut_m
  implicit none
  private
  public :: & !< routines >!
            namelist__get_double,   &
            namelist__get_integer,  &
            namelist__get_logical,  &
            namelist__read,         &
            namelist__get_string

  logical, save :: Read_done = .false.

  integer(SI), parameter :: STRING_LENGTH_MAX = 200

  integer(SI) :: Total_nloop
  integer(SI) :: Slicedata_nskip
  character(len=STRING_LENGTH_MAX) :: Slicedata_tag
  real(DR) :: Viscosity, Kappa
  logical  :: Debug

  namelist /data00/ Total_nloop
  namelist /data01/ Slicedata_nskip,  Slicedata_tag
  namelist /data02/ Viscosity
  namelist /data03/ Kappa
  namelist /data04/ Debug


contains

  function namelist__get_double(variable)
    character(len=*), intent(in) :: variable
    real(DR) :: namelist__get_double

    call ut__assert(Read_done, &
                    '<namelist__get_double> Read namelist file first.')

    select case (variable)
      case                ('Kappa')
        namelist__get_double = Kappa
      case                ('Viscosity')
        namelist__get_double = Viscosity
      case default
        call ut__message('? arg = ', variable)
        call ut__fatal('<namelist__get_double> not in the namelist?')
    end select
  end function namelist__get_double


  function namelist__get_integer(variable)
    character(len=*), intent(in) :: variable
    integer(SI) :: namelist__get_integer

    call ut__assert(Read_done, &
                    '<namelist__get_integer> Read namelist file first.')

    select case (variable)
      case                 ('Slicedata_nskip')
        namelist__get_integer = Slicedata_nskip
      case                 ('Total_nloop')
        namelist__get_integer = Total_nloop
      case default
        call ut__message('? arg = ', variable)
        call ut__fatal('<namelist__get_integer> not in the namelist?')
    end select
  end function namelist__get_integer


  function namelist__get_logical(variable)
    character(len=*), intent(in) :: variable
    logical :: namelist__get_logical

    call ut__assert(Read_done, &
                    '<namelist__get_logical> Read namelist file first.')

    select case (variable)
      case                 ('Debug')
        namelist__get_logical = Debug
      case default
        call ut__message('? arg = ', variable)
        call ut__fatal('<namelist__get_logical> not in the namelist?')
    end select
  end function namelist__get_logical


  subroutine namelist__read
    character(len=STRING_LENGTH_MAX) :: namelist_file

    call ut__assert(command_argument_count()==1, &
                    "Usage: smoke_ring param_file")
    call get_command_argument(1,namelist_file)

    open(10,file=trim(namelist_file))
      read(10,nml=data00)
      read(10,nml=data01)
      read(10,nml=data02)
      read(10,nml=data03)
      read(10,nml=data04)
    close(10)

    write(6,nml=data00)
    write(6,nml=data01)
    write(6,nml=data02)
    write(6,nml=data03)
    write(6,nml=data04)

    Read_done = .true.
  end subroutine namelist__read


  function namelist__get_string(variable)
    character(len=*), intent(in) :: variable
    character(len=STRING_LENGTH_MAX) :: namelist__get_string

    call ut__assert(Read_done, &
                    '<namelist__get_string> Read namelist file first.')

    select case         (variable)
      case                ('Slicedata_tag')
        namelist__get_string = Slicedata_tag
      case default
        call ut__message('? arg = ', variable)
        call ut__fatal('<namelist__get_string> not in the namelist?')
    end select
  end function namelist__get_string

end module namelist_m
