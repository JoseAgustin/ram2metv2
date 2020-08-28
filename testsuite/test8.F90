!>  @brief Program to test subroutine viento
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/27/2020
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
 program test8
 use variables
 implicit none
 integer :: dir,i
 real:: mag
 real :: uw,vw
 character(len=32)::arg
 logical :: continua

  do i = 1, iargc()
     call getarg(i, arg)
     if(arg .eq."--version") print *,"3.0"
  end do
  mag=4.0
  call logs("Testing subroutine viento")
  do dir=0,360,45
 call viento(31,real(dir),continua,uw,vw)
 call viento(32,mag,continua,uw,vw)
  print '(I4,f5.1,2(f7.3))',dir,mag,uw,vw
  end do
 end program test8
