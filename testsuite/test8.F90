 program test1
 use variables
 implicit none

 real:: mag, dir
 real :: uw,vw
 logical :: continua
 mag=4.0
  call logs("Testing subroutine viento")
  do dir=0.,360.,45.
 call viento(31,dir,continua,uw,vw)
 call viento(32,mag,continua,uw,vw)
  print '(f4.0,f5.1,2(f7.3))',dir,mag,uw,vw
  end do
 end program test1
