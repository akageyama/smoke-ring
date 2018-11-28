program main
  use constants_m
  implicit none

  integer :: nloop, io
  character(len=*), parameter :: INPUT = "../src/_data_slice"
                               ! output is stdout

  open(10, file=trim(INPUT),  &
       form='unformatted',  &
       status='old')
    do
      read(10,iostat=io) nloop
      if ( io/=0 ) exit
      print *, '  nloop', nloop
    end do
  close(10)
end program main
