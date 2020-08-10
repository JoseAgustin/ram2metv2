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
    anio='2020'
    imes='01'
    fmes='03'
    idia='18'
    fdia='23'
    ihr='01'
    fhr='07'
    call guarda

end program
