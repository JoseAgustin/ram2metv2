!   rama2metv2.f90
!>  @author Jose Agustin Garcia Reynoso
!>  @date 02/08/2020
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
!>  @date 02/08/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!   _                               _
!  | | ___  ___     _ __  _ __ ___ | |
!  | |/ _ \/ _ \   | '_ \| '_ ` _ \| |
!  | |  __/  __/   | | | | | | | | | |
!  |_|\___|\___|___|_| |_|_| |_| |_|_|
!             |_____|
subroutine lee_nml
    integer,dimension(12):: month=(/31,28,31,30,31,30,31,31,30,31,30,31/)
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
! Evaluating leap year
    read(anio,'(I4)')lanio
    if (mod(lanio,4).eq.0) month(2)=29
    ! Evaluating valid dates
    read(imes,'(I2)')lmes
    if(lmes.lt.1 .or. 12.lt.lmes)  then
      print*, "xxxxx Error imes namelist xxxxx";stop
    end if
    read(idia,'(I2)')ldia
    if(ldia.gt.month(lmes)) then
      print*, "xxxxx Error idia namelist xxxxx";stop
    end if
    read(fmes,'(I2)')lmes
    if(lmes.lt.1 .or. 12.lt.lmes) then
      print *, "<<<<< Error fmes namelist >>>>>";stop
    end if
    read(fdia,'(I2)')ldia
    if(ldia.gt.month(lmes)) then
      print *, "<<<<< Error fdia namelist >>>>> ";stop
    end if
end subroutine lee_nml
!>  @brief fromwind direction and magnitude obtains wind vector in _y_ and _x_
!>  @author Jose Agustin Garcia Reynoso
!>  @date 02/08/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!>  @param ival variable code
!>  @param rval magnitud or direction value
!>  @param sigue .true. finish computing .false. direction input
!>  @param uw wind vector in _x_ axis (W to W)
!>  @param vw wind vector in _y_ axis( N-S)
!         _            _
!  __   _(_) ___ _ __ | |_ ___
!  \ \ / / |/ _ \ '_ \| __/ _ \
!   \ V /| |  __/ | | | || (_) |
!    \_/ |_|\___|_| |_|\__\___/
!
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
!>  @date 02/08/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!   __                               _
!  / _| ___ ___  _ ____   _____ _ __| |_
! | |_ / __/ _ \| '_ \ \ / / _ \ '__| __|
! |  _| (_| (_) | | | \ V /  __/ |  | |_
! |_|  \___\___/|_| |_|\_/ \___|_|   \__|
character(len=15) function fconvert(id,im,ia,hora)
integer,parameter :: isf=5
integer,parameter :: if2=24-isf
integer :: idia,imes,ianio
integer,intent(IN) :: id,im,ia
character (len=5),intent(IN):: hora
character (len=2) dia,mes,chora
character (len=4) anio
    idia=id
    imes=im
    ianio=ia
  chora=hora(1:2)
  READ (chora, '(I2)') ih
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
!>   | ---      |   ---|
!>   | Meterological variables PBa, TMP, WDR, WSP, RH|  128 |
!>   | Particlulate Matter (coarse and fine) and ozone | 129|
!>   | NO, NO2, CO, OC (PMCO) and SO2 | 141|
!>  @author Jose Agustin Garcia Reynoso
!>  @date 23/08/2020
!>  @version  3.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!>  @param var Variable abbreviation to be mapped to its Grib code
!                                      _
!  __   _____ ___  _ ____   _____ _ __| |_
!  \ \ / / __/ _ \| '_ \ \ / / _ \ '__| __|
!   \ V / (_| (_) | | | \ V /  __/ |  | |_
!    \_/ \___\___/|_| |_|\_/ \___|_|   \__|
!
integer function vconvert(var)
integer,parameter :: num_vars=16
character (len=3),intent(IN) ::var
character (len=3):: cvar
character (len=3),dimension(num_vars):: cmap
integer ,dimension(num_vars):: imap
integer :: i
    cmap=["PBa","TMP","WDR","WSP","UWN","VWN","RH ",&      ! Table 128
          "PM1","PM2","O3 ",&                 ! Table 129
          "NOX","NO ","NO2","CO ","SO2","PMC"] ! Table 141
    imap=[1,11,31,32,33,34,52,   &   ! Wind uwnd 33 and vwnd 34
          156,157,180,     &
          140,141,142,148,232,249]
   cvar=trim(var)
   do i=1,num_vars
       if(trim(cvar).eq.trim(cmap(i))) then
           vconvert=imap(i)
           return
       end if
   end do
   vconvert=-99
   return
end function
!>  @brief read stations file
!>  @author Jose Agustin Garcia Reynoso
!>  @date 02/08/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!  _
! | | ___  ___
! | |/ _ \/ _ \
! | |  __/  __/
! |_|\___|\___|
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
!>  @date 02/08/2020
!>  @version  2.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!                             _
!    __ _ _   _  __ _ _ __ __| | __ _
!   / _` | | | |/ _` | '__/ _` |/ _` |
!  | (_| | |_| | (_| | | | (_| | (_| |
!   \__, |\__,_|\__,_|_|  \__,_|\__,_|
!   |___/
subroutine guarda
    implicit none
    integer i,j
    integer ivar
    integer dy
    integer mh
    integer yr
    integer es
    logical salir,sigue
    real :: rval,uw,vw,dir
    character(len=22) :: fname, fname2, cdum
    character(len=10)::fecha
    character(len=5) hora
    character(len=15) ::cfecha
    character(len=3):: c_id,cvar
    character(len=1) :: sep
    character(len=2) :: mes
    salir=.true.
    sigue=.false.
    Message_type='ADPSFC'
    call logs("Start doing process")
    fname ='meteorologia_'//anio//'.csv'
    fname2='contaminantes_'//anio//'.csv'
    open (unit=12,file=fname ,status='old',action='read')
    call logs('Opened '//fname)
    open (unit=13,file=fname2,status='old',action='read')
    call logs('Opened '//fname2)
    do i=1,11
       read(12,*) cdum
       read(13,*) cdum
    end do
    open (unit=20,file='rama'//anio//'_met.txt')
    open (unit=21,file='rama'//anio//'_pol.txt')
     I=0
    do while (salir)
    rval=rnulo
    read(12,125,advance="no",IOSTAT=es)dy,sep,mh,sep,yr,hora,c_id
    if (es>0) stop 'FILE met problem reading'
    read(12,*,IOSTAT=es) cvar,rval
    if (es>0) stop 'File met problem reading part 2'
    !read(12,*,END=200)fecha,hora,c_id,cvar,rval
    write(mes,'(I2.2)') mh
    if (mes.eq.imes) then
      ivar = vconvert(cvar)
    if(rval.ne.rnulo.and.ivar.eq.1 ) rval=rval*101325/760 ! conversion from mmHg to Pa
    if(rval.ne.rnulo.and.ivar.eq.11) rval=rval+273.15 ! conversion from C to K
    if(rval.gt.11 .and. ivar .eq.32) then
        print *,dy,sep,mh,sep,yr," ",hora,c_id, rval
        stop
    end if
      cfecha= fconvert(dy,mh,yr,hora)
      do j=1,n_rama
        if(c_id.eq.id_name(j).and.rval.ne.rnulo)then
          write(20,120)Message_type,c_id,cfecha,lat(j),lon(j),msn(j),ivar,776.,10.,1,rval
        ! wind conversion from dir,mag to u,v
        if(ivar.eq.31 .or. ivar.eq.32)then
            call viento(ivar,rval,sigue,uw,vw)
            if (uw.ne.rnulo.and.sigue) then
                write(20,120)Message_type,c_id,cfecha,lat(j),lon(j),msn(j),33,776.,10.,1,uw
                write(20,120)Message_type,c_id,cfecha,lat(j),lon(j),msn(j),34,776.,10.,1,vw
                uw=rnulo
                sigue=.false.
            end if
        end if
        ! wind finish
         exit
        end if ! per station
      end do
    end if ! dare
   if(mes.eq.fmes.and. hora(1:2).eq.fhr.and.trim(cvar).eq."PBa") salir=.false.
   end do  !salir
200 continue
  salir=.true.
  do while (salir)
    rval=rnulo
    read(13,125,advance="no",IOSTAT=es)dy,sep,mh,sep,yr,hora,c_id
    if (es>0) stop 'FILE poll problem reading'
    read(13,*,IOSTAT=es) cvar,rval
    if (es>0) stop 'File poll problem reading part 2'
    write(mes,'(I2.2)') mh
       if (mes.eq.imes) then
         cfecha= fconvert(dy,mh,yr,hora)
         ivar = vconvert(cvar)
         !print *,ivar,cvar
       if(rval.ne.rnulo.and.ivar.eq.148) rval=rval*1000! conversion from ppm a ppb
         do j=1,n_rama
            if(c_id.eq.id_name(j).and.rval.ne.rnulo)then
             write(21,121)Message_type,c_id,cfecha,lat(j),lon(j),msn(j),ivar,776.,10.,1,rval
             exit
            end if
         end do ! n_rama
        end if ! fecha
    if(mes.eq.fmes .and. hora(1:2).eq.fhr.and.trim(cvar).eq."SO2") salir=.false.
    end do  !while salir
    call logs("END PROGRAM")
    close(12)
    close(13)
    close(20)
    close(21)
120 format(A6,x,A5,x,A15,x,f7.4,x,f9.4,x,f6.0,x,I3,x,f4.0,x,f6.0,x,I2,x,f10.1)
121 format(A6,x,A5,x,A15,x,f7.4,x,f9.4,x,f6.0,x,I3,x,f4.0,x,f6.0,x,I2,x,f10.1)
125 format(I2,A,I2,A,I4,x,A5,x,A3,x,A)

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
300 continue
end subroutine guarda
!>  @brief count the number of rowns in a file
!>   @author  Jose Agustin Garcia Reynoso
!>   @date  13/07/2020
!>   @version  2.2
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param  iunit file unit where the count has to be made
!                        _
!   ___ _   _  ___ _ __ | |_ __ _
!  / __| | | |/ _ \ '_ \| __/ _` |
! | (__| |_| |  __/ | | | || (_| |
!  \___|\__,_|\___|_| |_|\__\__,_|
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
!>   @date  25/08/2020
!>   @version  2.3
!>   @copyright Universidad Nacional Autonoma de Mexico 2020
!>   @param texto text to be displayed
!  _
! | | ___   __ _ ___
! | |/ _ \ / _` / __|
! | | (_) | (_| \__ \
! |_|\___/ \__, |___/
!          |___/
subroutine logs(texto)
    implicit none
    character(len=*),intent(in):: texto
    character(len=50):: FMT
    integer :: lef
    lef=(40-len(trim(texto)))/2
    if(lef.lt.1) lef=1
    write(FMT,"('(3x,7(''*''),',I0,'x,A,',I0,'X,7(''*''))')") lef,lef
    write(6,FMT) trim(texto)
end subroutine
end module variables
