! For function fconvert
program test3
use variables
implicit none
integer :: jmes,jhora
character(len=10):: cfecha
character(len=5):: chora
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
