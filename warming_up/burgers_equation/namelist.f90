!-----------------------------------------------------------------------------
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
!    warming_up/bergers_equation/namelist.f90
!-----------------------------------------------------------------------------

module namelist_m
  use constants_m
  use ut_m
  implicit none
  private
  public :: & !< routines >!
            namelist__get_double, &
            namelist__get_integer, &
            namelist__read

  logical, save :: Read_done = .false.

  integer(SI) :: Nx
  real(DR) :: Diffusion_coeff

  namelist /data00/ Nx
  namelist /data01/ Diffusion_coeff


contains


  function namelist__get_double(variable)
    character(len=*), intent(in) :: variable
    real(DR) :: namelist__get_double

    call ut__assert(Read_done,  &
                    '<namelist__get_double> Read namelist first.')

    select case (variable)
    case                ('Diffusion_coeff')
       namelist__get_double = Diffusion_coeff
    case default
       call ut__message('? arg = ', variable)
       call ut__fatal('<namelist__get_double> not in the namelist?')
    end select
  end function namelist__get_double


  function namelist__get_integer(variable)
    character(len=*), intent(in) :: variable
    integer(SI) :: namelist__get_integer

    call ut__assert(Read_done,  &
                    '<namelist__get_integer> Read namelist first.')

    select case (variable)
    case                 ('Nx')
       namelist__get_integer = Nx
    case default
       call ut__message('? arg = ', variable)
       call ut__fatal('<namelist__get_integer> not in the namelist?')
    end select
  end function namelist__get_integer


  subroutine namelist__read
    integer(SI), parameter :: FILENAME_MAX_LENGTH = 200
    character(len=FILENAME_MAX_LENGTH) :: filename

    call ut__assert(command_argument_count()==1, &
                    "Usage: berguers param_file")
    call get_command_argument(1, filename)
    open(10,file=trim(filename))
      read(10,nml=data00)
      read(10,nml=data01)
    close(10)

    write(6,nml=data00)
    write(6,nml=data01)

    Read_done = .true.
  end subroutine namelist__read

end module namelist_m
