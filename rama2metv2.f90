!
!   rama2metv2.f90
!   
!
!   Created by Agustin Garcia on 08/08/16.
!   Copyright 2016 Centro de Ciencias de la Atmosfera, UNAM. All rights reserved.
!
!   Changes:
!   29/06/2018  namelit reading addition
!
!   Read contaminantes_2016.csv 
!         meteorologia_2016.csv
!  and converts to a METv5 ascii format
!
!  ifort -O2 -o rama2met.exe rama2metv2.f90
!
module variables
integer n_rama
parameter (n_rama=59)! No. stations in localization file
parameter (rnulo=-9999.)
real,dimension(n_rama) :: lon,lat,msn
character(len=6) :: Message_type
character(len=3),dimension(n_rama)    :: id_name
character(len=2)::idia, imes, fdia, fmes, ihr, fhr

NAMELIST /FECHA/ ihr, idia, imes,fhr, fdia, fmes
common /STATIONS/ lon,lat,msn,id_name,Message_type
end module variables

program  rama2metv2
     use variables

    call lee_nml

    call lee

    call guarda

contains
!
subroutine lee_nml
    logical existe
    existe = .FALSE.
    write(6,*)' >>>> Reading file - namelist.met'
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
!
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
    character(len=3) c_id,cvar
    salir=.true.
    sigue=.false.
    Message_type='ADPSFC'
    fname ='meteorologia_2011.csv'
    fname2='contaminantes_2011.csv'
    open (unit=12,file=fname ,status='old',action='read')
    open (unit=13,file=fname2,status='old',action='read')
    do i=1,11
       read(12,*) cdum
       read(13,*) cdum
    end do
    open (unit=20,file='rama2011_met.txt')
    open (unit=21,file='rama2011_pol.txt')
     I=0
    do while (salir)
    rval=rnulo
    read(12,*,END=200)fecha,hora,c_id,cvar,rval
    if (fecha(4:5).eq.imes.and. hora(1:2).eq.ihr) then
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
        end if
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
    close(12)
    close(13)
    close(20)
    close(21)
120   format(A6,x,A5,x,A15,x,f7.4,x,f9.4,x,f6.0,x,I3,x,f4.0,x,f6.0,x,I2,x,f10.1)
121   format(A6,x,A5,x,A15,x,f7.4,x,f9.4,x,f6.0,x,I3,x,f4.0,x,f6.0,x,I2,x,f10.1)

!  1 Message_type
!  2 Station_ID
!  3 Valid_Time in YYYYMMDD_HHMMSS format
!  4 Lat in degrees North
!  5 lon in degrees East
!  6 Elevation in meters above sea level
!  7 Grib_Code corresponding to this observation type
!  8 Level as the pressure level in hPa or accumulation interval in hours
!  9 Height in msl or agl of the observation value
! 10 QC_String Quality control value
! 11 Observation_Value in the units prescribed for the grib code

100 format (A10,a5,A3,A,F)
300 continue
end subroutine guarda

subroutine lee
implicit none
integer i,j
character(len=13) :: fname, cdum
fname='est_rama.txt'
print *,"   Lee ",fname
open (unit=11,file=fname,status='OLD',action='read')
read (11,'(A)')cdum
do i=1,n_rama
    read (11,*) id_name(i),lat(i),lon(i),msn(i)
    !print *,id_name(i),lat(i),lon(i),msn(i)
end do
close(11)
end subroutine lee

character(len=15) function fconvert(fecha,hora)
integer,parameter :: isf=5
integer,parameter :: if2=24-isf
integer :: ih,idia,imes,ianio
character(len=10) fecha
character (len=5) hora
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
        if(idiar+1.ge.31) then
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

integer function vconvert(cvar)
character (len=3) ::cvar
   cvar=trim(cvar)
   select case (cvar)
    case ("PBa")
        vconvert=1
    case ("TMP")
        vconvert=11
    case ("WDR")
        vconvert=31
    case ("WSP")
        vconvert=32
    case ("RH")
        vconvert=52
    case ("O3")
        vconvert=180
    case ("CO")
        vconvert=148
    case("SO2")
        vconvert=232
    case("NOX")
        vconvert=140
    case("NO")
        vconvert=141
    case("NO2")
        vconvert=142
    case("PM1")
        vconvert=156
    case("PM2")
        vconvert=157
    case DEFAULT
        vconvert=-99
   end select
return
end function
subroutine viento(ival,rval,sigue,uw,vw)
    integer ival
    logical sigue
    real rval,uw,vw,deg2rad
    real, save::  dir
    deg2rad =4*ATAN(1.0)/180.0
    if(rval.eq.rnulo) return
    if(ival.eq.31) dir=rval
    if(ival.eq.32) then
     uw=rval*sin(deg2rad*(180+dir))
     vw=rval*cos(deg2rad*(180+dir))
     sigue=.true.
    end if
    return
end subroutine viento
end program
