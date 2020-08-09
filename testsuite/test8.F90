 program test1
 use variables
 implicit none
 integer :: dir
 real:: mag
 real :: uw,vw
 logical :: continua
 mag=4.0
  call logs("Testing subroutine viento")
  do dir=0,360,45
 call viento(31,real(dir),continua,uw,vw)
 call viento(32,mag,continua,uw,vw)
  print '(I4,f5.1,2(f7.3))',dir,mag,uw,vw
  end do
 end program test1
