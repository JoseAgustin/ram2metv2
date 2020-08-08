!>  @brief Program to convert measured data into METv5 data format
!>
!>   Read contaminantes_2016.csv
!>
!>         meteorologia_2016.csv
!>
!>  and converts to a METv5 ascii format
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
program  rama2metv2
     use variables
	call logs("Testing subroutine guarda")
    imes='01'
    anio='2020'
    call lee

    call guarda

end program
