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
!    slice_grapher/extract_nloop_list.f90
!-------------------------------------------------------------------
program main
  use constants_m
  implicit none

  integer(DI) :: nloop
  integer :: io
  character(len=*), parameter :: FILE_NAME_SLICE = "../src/_data_slice"

  open(10, file=trim(FILE_NAME_SLICE),  &
       form='unformatted',  &
       status='old')
    do
      read(10,iostat=io) nloop
      if ( io/=0 ) exit
      print *, '  nloop', nloop
    end do
  close(10)
end program main
