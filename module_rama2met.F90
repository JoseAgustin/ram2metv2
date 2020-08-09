!
!   rama2metv2.f90
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!  ifort -O2 -o rama2met.exe rama2metv2.f90
!
module variables
!> No. stations in localization file
integer :: n_rama ;!>  value for missing value
real,parameter:: rnulo=-9999. ;!>  localization logitude coordinate
real,allocatable  :: lon(:) ;!>  localization latitude coordinate
real,allocatable  :: lat(:) ;!>  meters above sea level of station
real,allocatable  :: msn(:) ;!> station short name for ID
character(len=3),allocatable :: id_name(:);!> year from input data
character(len=4):: anio ;!> start day for output
character(len=2):: idia ;!> start month for output
character(len=2):: imes;!> end day for output
character(len=2):: fdia ;!> end month for output
character(len=2):: fmes ;!> start hour for output
character(len=2):: ihr ;!> end hour for output
character(len=2):: fhr ;!>  message type
character(len=6):: Message_type

NAMELIST /FECHA/ anio,ihr, idia, imes,fhr, fdia, fmes
common /STATIONS/n_rama,Message_type
contains
!>  @brief read namelist input file for selecting specific days
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
subroutine lee_nml
    integer ::unit_nml
    logical :: existe
    existe = .FALSE.
    call logs('Start reading file - namelist.met')
    inquire ( FILE = 'namelist.met' , EXIST = existe )
        if ( existe ) then
        !  Opening the file.
            open ( FILE   = 'namelist.met' ,      &
            UNIT   =  unit_nml        ,      &
            STATUS = 'OLD'            ,      &
            FORM   = 'FORMATTED'      ,      &
            ACTION = 'READ'           ,      &
            ACCESS = 'SEQUENTIAL'     )
            !  Reading the file
            READ (unit_nml , NML = FECHA )
            else
            stop '***** No namelist.met'
        ENDIF
end subroutine lee_nml
!>  @brief fromwind direction and magnitude obtains wind vector in _y_ and _x_
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!>  @param ival variable code
!>  @param rval magnitud or direction value
!>  @param sigue .true. finish computing .false. direction input
!>  @param uw wind vector in _x_ axis (W to W)
!>  @param vw wind vector in _y_ axis( N-S)
subroutine viento(ival,rval,sigue,uw,vw)
    integer ival
    logical sigue
    real rval,uw,vw,deg2rad
    real, save::  dir
    deg2rad =4*ATAN(1.0)/180.0
    if(rval.eq.rnulo) return
    if(ival.eq.31) dir=rval
    if(ival.eq.32) then
     uw=-rval*sin(deg2rad*(dir))
     vw=-rval*cos(deg2rad*(dir))
     sigue=.true.
    end if
    return
end subroutine viento
!>  @brief reformat date and time from DD-MM-YYYY HH:MM to YYYYMMDD_HHMMSS
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
character(len=15) function fconvert(fecha,hora)
integer,parameter :: isf=5
integer,parameter :: if2=24-isf
integer :: ih,idia,imes,ianio
character(len=10),intent(IN):: fecha
character (len=5),intent(IN):: hora
character (len=2) dia,mes,chora
character (len=4) anio

anio=fecha(7:10)
dia =fecha(1:2)
mes = fecha(4:5)
chora=hora(1:2)
READ (anio, '(I4)'), ianio
READ (dia, '(I2)'), idia
READ (mes, '(I2)'), imes
READ (hora, '(I2)'), ih
  select case (imes)
  case (1,3,5,7,8,10)
    if(ih+isf.gt.23) then
        ih=ih-if2
        if(idia+1.ge.32) then
         idia=1
         imes=imes+1
        else
         idia=idia+1
        end if
    else
        ih=ih+isf
    end if
  case (4,6,9,11)
   if(ih+isf.gt.23) then
        ih=-if2+ih
        if(idia+1.ge.31) then
          idia=1
          imes=imes+1
        else
          idia=idia+1
        end if
    else
        ih=ih+isf
   end if
  case(2)
    if(ih+isf.gt.23) then
        ih=-if2+ih
        if(idia+1.ge.29) then
          idia=1
          imes=imes+1
        else
          idia=idia+1
        end if
    else
        ih=ih+isf
    end if
  case(12)
    if(ih+isf.gt.23) then
        ih=-if2+ih
        if(idia+1.ge.32) then
          idia=1
          imes=1
         ianio=ianio+1
        else
          idia=idia+1
        end if
    else
        ih=ih+isf
    end if
  case DEFAULT
  end select
WRITE(fconvert,'(I4,I2.2,I2.2,"_",I2.2,"0000")')ianio,imes,idia,ih
!print *,anio//mes//dia//"_"//chora//"0000"
!print *,fconvert
return
end function fconvert
!>  @brief converts text variable to its Grib_Code
!>  from: https://www.nco.ncep.noaa.gov/pmb/docs/on388/table2.html
!>   |Variables | Table|
!>   | --- | ---|
!>   | Meterological variables PBa, TMP, WDR, WSP, RH|  128 |
!>   | Particlulate Matter (coarse and fine) and ozone | 129|
!>   | NO, NO2, CO, OC (PMCO) and SO2 | 141|
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
integer function vconvert(var)
character (len=3),intent(IN) ::var
character (len=3):: cvar
   cvar=trim(var)
   select case (cvar)
    case ("PBa")   !Table 128
        vconvert=1
    case ("TMP")
        vconvert=11
    case ("WDR")
        vconvert=31
    case ("WSP")
        vconvert=32
    case ("RH")
        vconvert=52
    case ("O3")     !Table 129
        vconvert=180
    case("PM1")
        vconvert=156
    case("PM2")
        vconvert=157
    case ("CO")     !Tablw 141
        vconvert=148
    case("SO2")
        vconvert=232
    case("NOX")
        vconvert=140
    case("NO")
        vconvert=141
    case("NO2")
        vconvert=142
    case("PMC")
        vconvert=249
    case DEFAULT
        vconvert=-99
   end select
return
end function
!>  @brief read stations file
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
subroutine lee
implicit none
integer i,j
character(len=13) :: fname, cdum
fname='est_rama.txt'
call logs("Start reading file - "//fname)
open (unit=11,file=fname,status='OLD',action='read')
n_rama=cuenta(11)-1  !encabezado se contabiliza en cuenta
allocate(id_name(n_rama),lat(n_rama),lon(n_rama),msn(n_rama))
read (11,'(A)')cdum
do i=1,n_rama
    read (11,*) id_name(i),lat(i),lon(i),msn(i)
    !print *,id_name(i),lat(i),lon(i),msn(i)
end do
close(11)
end subroutine lee
!>  @brief read and stores specific time period for the variables
!>  @author Jose Agustin Garcia Reynoso
!>  @date 08/02/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
subroutine guarda
    implicit none
    integer i,j
    integer ivar
    logical salir,sigue
    real :: rval,uw,vw,dir
    character(len=22) :: fname, fname2, cdum
    character(len=10)::fecha
    character(len=5) hora
    character(len=15) ::cfecha
    character(len=3):: c_id,cvar
    salir=.true.
    sigue=.false.
    Message_type='ADPSFC'
    call logs("Start doing process")
    fname ='meteorologia_'//anio//'.csv'
    fname2='contaminantes_'//anio//'.csv'
    open (unit=12,file=fname ,status='old',action='read')
    open (unit=13,file=fname2,status='old',action='read')
    do i=1,11
       read(12,*) cdum
       read(13,*) cdum
    end do
    open (unit=20,file='rama'//anio//'_met.txt')
    open (unit=21,file='rama'//anio//'_pol.txt')
     I=0
    do while (salir)
    rval=rnulo
    read(12,*,END=200)fecha,hora,c_id,cvar,rval
    if (fecha(4:5).eq.imes) then
      ivar = vconvert(cvar)
    if(rval.ne.rnulo.and.ivar.eq.1 ) rval=rval*101325/760 ! conversion de mmHg a Pa
    if(rval.ne.rnulo.and.ivar.eq.11) rval=rval+273.15 ! conversion de C a K
      cfecha= fconvert(fecha,hora)
      do j=1,n_rama
        if(c_id.eq.id_name(j).and.rval.ne.rnulo)then
          write(20,120)Message_type,c_id,cfecha,lat(j),lon(j),msn(j),ivar,776.,10.,1,rval
        ! vientos
        if(ivar.eq.31 .or. ivar.eq.32)then
            call viento(ivar,rval,sigue,uw,vw)
            if (uw.ne.rnulo.and.sigue) then
                write(20,120)Message_type,c_id,cfecha,lat(j),lon(j),msn(j),33,776.,10.,1,uw
                write(20,120)Message_type,c_id,cfecha,lat(j),lon(j),msn(j),34,776.,10.,1,vw
                uw=rnulo
                sigue=.false.
            end if
        end if
        ! vientos fin
         exit
        end if ! per station
      end do
    end if ! fecha
   if(fecha(4:5).eq.fmes.and. hora(1:2).eq.fhr.and.trim(cvar).eq."PBa") salir=.false.
   end do  !salir
200 continue
  salir=.true.
  do while (salir)
    rval=rnulo
    read(13,*,END=300)fecha,hora,c_id,cvar,rval
       if (fecha(4:5).eq.imes) then
         cfecha= fconvert(fecha,hora)
         ivar = vconvert(cvar)
         !print *,ivar,cvar
       if(rval.ne.rnulo.and.ivar.eq.148) rval=rval*1000! conversion de ppm a ppb
         do j=1,n_rama
            if(c_id.eq.id_name(j).and.rval.ne.rnulo)then
             write(21,121)Message_type,c_id,cfecha,lat(j),lon(j),msn(j),ivar,776.,10.,1,rval
             exit
            end if
         end do ! n_rama
        end if ! fecha
    if(fecha(4:5).eq.fmes .and. hora(1:2).eq.fhr.and.trim(cvar).eq."SO2") salir=.false.
    end do  !while salir
    call logs("END PROGRAM")
    close(12)
    close(13)
    close(20)
    close(21)
120   format(A6,x,A5,x,A15,x,f7.4,x,f9.4,x,f6.0,x,I3,x,f4.0,x,f6.0,x,I2,x,f10.1)
121   format(A6,x,A5,x,A15,x,f7.4,x,f9.4,x,f6.0,x,I3,x,f4.0,x,f6.0,x,I2,x,f10.1)
!>  MET format has the following columns \cite brown2009model
!>
!> 1. Message_type
!> 2. Station_ID
!> 3. Valid_Time in YYYYMMDD_HHMMSS format
!> 4. Lat in degrees North
!> 5. lon in degrees East
!> 6. Elevation in meters above sea level
!> 7. Grib_Code as the integer GRIB code value or variable name corresponding to this observation type
!> 8. Level as the pressure level in hPa or accumulation interval in hours
!> 9. Height in msl or agl of the observation value
!> 10. QC_String Quality character(len=*) :: corresponding to the quality control value
!> 11. Observation_Value in the units prescribed for the grib code
!>
100 format (A10,a5,A3,A,F)
300 continue
end subroutine guarda
!>  @brief count the number of rowns in a file
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  07/13/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param  iunit file unit where the count has to be made
integer function  cuenta(iunit)
    implicit none
    integer,intent(IN) :: iunit
    integer :: io
    cuenta = 0
    DO
        READ(iunit,*,iostat=io)
        IF (io/=0) EXIT
        cuenta = cuenta + 1
    END DO
    rewind(iunit)
    return
end
!>  @brief display log during different program stages
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  08/08/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param texto text to be displayed
subroutine logs(texto)
    implicit none
    character(len=*),intent(in):: texto
    write(6,333) texto
333 format(3x,5("*"),x,A35,x,"******")
end subroutine
end module variables
