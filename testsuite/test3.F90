!>  @brief Program to test function fconvert
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/27/2020
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
program test3
use variables
implicit none
integer :: jmes,jhora,i
character(len=10):: cfecha
character(len=5):: chora
character(len=32)::arg

  do i = 1, iargc()
     call getarg(i, arg)
     if(arg .eq."--version") print *,"3.0"
  end do

call logs("Testing function fconvert")
cfecha="02-05-2020"
chora="00:23"
  do jmes=1,12
    write(cfecha(4:5),'(I2.2)') jmes
    do jhora =0,23,5
      write(chora(1:2),'(I2.2)') jhora
      call logs(fconvert(cfecha,chora))
    end do
  end do
end program test3
