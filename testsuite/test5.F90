!>  @brief Program to test subroutine lee
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/27/2020
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
program test5
use variables
integer ::status,i
character(len=32)::arg

  do i = 1, iargc()
     call getarg(i, arg)
     if(arg .eq."--version") print *,"3.0"
  end do

  call logs("Testing lee est_rama.txt")
  call lee

end program test5
