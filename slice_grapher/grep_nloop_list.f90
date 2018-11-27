program main
  use target_m
  use constants_m
  implicit none

  integer :: nloop, io

  open(10, file=trim(TARGET__FILENAME),  &
       form='unformatted',  &
       status='old')
    do
       read(10,iostat=io) nloop
       if ( io/=0 ) exit
       print *, '  nloop', nloop
    end do
  close(10)
end program main
