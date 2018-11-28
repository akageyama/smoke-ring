program main
  use constants_m
  implicit none

  integer :: nloop, io
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
