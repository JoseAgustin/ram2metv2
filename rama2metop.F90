!   rama2metop.F90
!>  @brief Program to convert measured data into METv9 data format
!>  @author Jose Agustin Garcia Reynoso
!>  @date 28/04/2021
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
!  ifort -O2 -o rama2metop.exe module_rama2met.F90 rama2metop.F90
!
program rama2metop
use variables

call  lee_nml

call lee

call lee_simat("O3 ",45)
call lee_simat("PM2",39)

!call guarda

contains
!>  @brief reads data downloaded from web page
!>  @author Jose Agustin Garcia Reynoso
!>  @date 28/04/2021
!>  @version  1.0
!>  @copyright Universidad Nacional Autonoma de Mexico
subroutine lee_simat (cvar,nstat)
implicit none
character(len=3),intent(IN)::cvar
integer,intent(IN) :: nstat
integer :: dy
integer :: mh
integer :: yr
integer :: i,j
integer :: ivar
integer :: ihora
integer :: es
real,allocatable :: rval(:)
character(len=5) :: hora
character(len=22) :: fname,cdum
character(len=15) ::cfecha
character(len=3),allocatable :: c_id(:)
allocate(rval(nstat),c_id(nstat))
    fname =trim(cvar)//'_opera'//anio//'.csv'
    Message_type='ADPSFC'
    ivar= vconvert(cvar)
    call logs("READING DATA"//cvar)
    open (unit=12,file="consulta_"//trim(cvar)//".csv",status='old',action='read')
    open (unit=21,file=fname)
    read(12,*)cdum,cdum,(c_id(i),i=1,nstat)
    do
        read(12,125,advance="no",IOSTAT=es,END=300)dy,mh,yr
        if (es>0) stop 'FILE consulta_ problem reading'
        read(12,*,IOSTAT=es) ihora,(rval(i),i=1,nstat)
        if (es>0) stop 'File consulta_ problem reading part 2'
        write(hora,'(I2.2,3A)') ihora,':00'
        cfecha= fconvert(dy,mh,yr,hora)
        !print *,cfecha
        where(rval.lt.0)rval=rnulo
        do j=1,n_rama
            do i=1,nstat
                if(c_id(i).eq.id_name(j).and.rval(i).ne.rnulo)then
write(21,120)Message_type,c_id(i),cfecha,lat(j),lon(j),msn(j),ivar,776.,10.,1,rval(i)
                end if
            end do
        end do
    end do
300 continue
close (12)
close (21)
deallocate(rval,c_id)
call logs("Fishing process "//cvar)
120 format(A6,x,A5,x,A15,x,f7.4,x,f9.4,x,f6.0,x,I3,x,f4.0,x,f6.0,x,I2,x,f10.1)
125 format(I2,x,I2,x,I4)
end subroutine
end program rama2metop
