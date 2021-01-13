!>  @brief Program to test subroutine guarda
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/27/2020
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
program  test6
     use variables
    integer :: i
    character(len=32)::arg

    do i = 1, iargc()
       call getarg(i, arg)
       if(arg .eq."--version") print *,"3.0"
    end do
	call logs("Testing subroutine guarda")
    anio='2020'
    imes='01'
    fmes='03'
    idia='18'
    fdia='23'
    ihr='01'
    fhr='07'
    call lee
    call guarda

end program
