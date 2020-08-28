!>  @brief Program to test subroutine lee_nml
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/27/2020
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
program test2
use variables
character(len=32)::arg
integer :: i
  do i = 1, iargc()
     call getarg(i, arg)
     if(arg .eq."--version") print *,"3.0"
  end do

	call logs("Testing subroutine lee_nml")
	call lee_nml

end program test2
