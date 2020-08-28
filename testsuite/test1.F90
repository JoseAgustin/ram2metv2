!>  @brief Program to test subroutine logs
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/27/2020
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
program test1
use variables
implicit none
character(len=32)::arg
integer :: i
  do i = 1, iargc()
     call getarg(i, arg)
     if(arg .eq."--version") print *,"3.0"
  end do
	call logs("Testing ")
	call logs("Subroutine logs        ")
	call logs("123456789012345678901234567890")
	call logs("1234567890123456789012345678901234567890123")
end program
