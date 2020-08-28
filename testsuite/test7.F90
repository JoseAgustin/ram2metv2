!>  @brief Program to test function cuenta
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/27/2020
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
program test7
  use variables
  integer :: i
  character(len=32)::arg

  do i = 1, iargc()
     call getarg(i, arg)
     if(arg .eq."--version") print *,"3.0"
  end do
    call logs("testintg function cuenta")
    open(Unit=10,file="est_rama.txt",status="OLD",action="READ")
    print *,cuenta(10)
end program test7
