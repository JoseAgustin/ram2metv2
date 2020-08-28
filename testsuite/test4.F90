!>  @brief Program to test function vconvert
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/27/2020
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
program test4
  use variables
  implicit none
  integer,parameter::val=16
  integer :: i
  integer,dimension(val):: verificar
  character(len=3),dimension(val):: variable
  character(len=32)::arg
   variable=(/"PBa","TMP","WDR","WSP","RH ","O3 ","CO ","SO2","NOX",&
  "NO ","NO2","PM1","PM2","PMC","UWN","VWN"/)
  verificar=[1,11,31,32,52,180,148,232,140,141,142,156,157,249,33,34]
  do i = 1, iargc()
     call getarg(i, arg)
     if(arg .eq."--version") print *,"3.0"
  end do
  call logs("Testing function vconvert")
  do i=1,16
     !print *,verificar(i),vconvert(variable(i))
    if (verificar(i).ne. vconvert(variable(i))) stop "Error in value"
  end do
end program test4
