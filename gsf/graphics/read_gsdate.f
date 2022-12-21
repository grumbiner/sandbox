program read_gsdate
!----------------------------------------------------
! This fortran 90 program reads a Navy Gulf Stream 
! north-south wall file, extracts the calendar date, 
! reformates this date to YYYYMMDD, and writes it to a file. 
! March 2004 William O'Connor 
!----------------------------------------------------
implicit none
integer i,iday,iyear,imon 
character*80 line1
character*3  cmon 
character*2  c2mon
character*2  cday
!----------------------------------------------------
! open input file and read date
open(unit=10,form='formatted',status='old')   
! read over first 5 lines
do i=1,5 
read(10,'(a80)')  line1
enddo 
! read line 6 and extract date 
read(10,'(40x,a2,1x,a3,1x,i2)') cday,cmon,iyear
close(10)
!------------------------------------------------------
! write(6,*) iday,cmon,iyear 
! reformat the date 
iyear=iyear + 2000
if(cmon.eq.'JAN') c2mon='01'
if(cmon.eq.'FEB') c2mon='02' 
if(cmon.eq.'MAR') c2mon='03'
if(cmon.eq.'APR') c2mon='04'
if(cmon.eq.'MAY') c2mon='05'
if(cmon.eq.'JUN') c2mon='06'
if(cmon.eq.'JUL') c2mon='07'
if(cmon.eq.'AUG') c2mon='08'
if(cmon.eq.'SEP') c2mon='09'
if(cmon.eq.'OCT') c2mon='10'
if(cmon.eq.'NOV') c2mon='11'
if(cmon.eq.'DEC') c2mon='12'
!
!write(6,'(i4,a2,a2)') iyear,c2mon,cday 
!-------------------------------------------------------
! write result to output file 
open(unit=90,form='formatted',status='new')
write(90,'(i4,a2,a2)') iyear,c2mon,cday 
close(90) 
!-----------------------------------------------------
stop
end program read_gsdate
