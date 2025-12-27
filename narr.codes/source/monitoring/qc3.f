 PROGRAM qc3_f90

    IMPLICIT NONE

    Integer, parameter :: Nday=365
    character :: tempLine*80,datename*4
    character(len=12) :: tempName,verftime*10,obstype*6,varname*4, sign*1
    real (kind=4) :: undefined
    integer :: inIoStatus21,inIoStatus11,chCount
    integer :: lineCount,  outputLine
    integer :: iHead, blankP
    integer :: ii, jj, var,day,d1,d2,m1,m2,mm,dd,mon(12),irec,dayc,dayold
    integer :: obsadp(3,Nday,3),acadp(3,Nday,3),obsair(9,Nday,2),acair(9,Nday,2),obssfc(Nday),acsfc(Nday),a,b,sum
    real (kind=4) :: realdata, peradp(Nday,3),perair(Nday,2) ,persfc(Nday) !! HR=9 means 1~8 sum, DAY, VAR
    data mon/31,28,31,30,31,30,31,31,30,31,30,31/

!   read(5,123) Nday
!123 format(i3)

    open (21,file='cmpevn',status='unknown')
    open (31,file='qcBinary.out',form='unformatted',status='replace',access='direct',recl=4)
    open (41,file='qcASCII.out',form='formatted',status='replace')

      outputLine=0
      day=0
      undefined=-999


         do      !!!loop
          tempLine = ''
         read(21, '(A)', iostat=inIoStatus11, advance='no', size=chCount) tempLine
         print*,'tempLine=',tempLine
         print*,'inIoStatus11,chCount=',inIoStatus11,chCount
!!         if (inIoStatus11== -4003) exit !!!end of file
         if (inIoStatus11== 145) exit !!!end of file
         if (chCount==0) cycle
         ! read(21, '(A)', iostat=inIoStatus11, advance='no', size=chCount) tempLine
          do iHead=1,3
            tempLine = adjustl(tempLine)
            blankP = index(tempLine, ' ')
            tempName = tempLine(1:blankP-1)
            tempLine(1:blankP-1) = ''
            select case (iHead)
                case(1)
                    verftime = tempName(1:len(verftime))
                case(2)
                    varname = tempName(1:len(varname))
                case(3)
                    obstype = tempName(1:len(obstype))
            end select
          end do
           
           if (verftime(5:5)=='0') then
              m1=0
            else if  (verftime(5:5)=='1') then
              m1=1 
            else
            endif

            if (verftime(6:6)=='0') then
              m2=0
            else if  (verftime(6:6)=='1') then
              m2=1 
            else if  (verftime(6:6)=='2') then
              m2=2
            else if  (verftime(6:6)=='3') then
              m2=3
            else if  (verftime(6:6)=='4') then
              m2=4
            else if  (verftime(6:6)=='5') then
              m2=5
            else if  (verftime(6:6)=='6') then
              m2=6
            else if  (verftime(6:6)=='7') then
              m2=7
            else if  (verftime(6:6)=='8') then
              m2=8
            else if  (verftime(6:6)=='9') then
              m2=9
            endif

            if (verftime(7:7)=='0') then
              d1=0
            else if  (verftime(7:7)=='1') then
              d1=1
            else if  (verftime(7:7)=='2') then
              d1=2
            else if  (verftime(7:7)=='3') then
              d1=3
            else
            endif

            if (verftime(8:8)=='0') then
              d2=0
            else if  (verftime(8:8)=='1') then
              d2=1
            else if  (verftime(8:8)=='2') then
              d2=2
            else if  (verftime(8:8)=='3') then
              d2=3
            else if  (verftime(8:8)=='4') then
              d2=4
            else if  (verftime(8:8)=='5') then
              d2=5
            else if  (verftime(8:8)=='6') then
              d2=6
            else if  (verftime(8:8)=='7') then
              d2=7
            else if  (verftime(8:8)=='8') then
              d2=8
            else if  (verftime(8:8)=='9') then
              d2=9
            endif
            mm=m1*10+m2
            dd=d1*10+d2
!           print*,'mm,dd=',mm,dd
            dayc=0
            do ii=1,mm-1
             dayc=dayc+mon(ii)
            enddo
            dayc=dayc+dd
!           print*,'dayc,dayold=',dayc,dayold
            if(dayc.ne.dayold) day=day+1
!           day = mm*10+dd
!           print*,'day=',day


          if (varname=='WIND' .and. obstype=='ADPUPA') then
            if (VERFTIME(9:10)=='00') then
                read(tempLine, *) obsadp(1,day,1),sign,acadp(1,day,1)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='12') then
                read(tempLine, *) obsadp(2,day,1),sign,acadp(2,day,1)
                outputLine=outputLine+1
            else
            end if
           end if

          if (varname=='TEMP' .and. obstype=='ADPUPA') then
            if (VERFTIME(9:10)=='00') then
                read(tempLine, *) obsadp(1,day,2),sign,acadp(1,day,2)
!               print*,'obsadp(1,day,2),sign,acadp(1,day,2)=',obsadp(1,day,2),sign,acadp(1,day,2)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='12') then
                read(tempLine, *) obsadp(2,day,2),sign,acadp(2,day,2)
!               print*,'obsadp(2,day,2),sign,acadp(2,day,2)=',obsadp(2,day,2),sign,acadp(2,day,2)
                outputLine=outputLine+1
            else
            end if
           end if

          if (varname=='SPHU' .and. obstype=='ADPUPA') then
            if (VERFTIME(9:10)=='00') then
                read(tempLine, *) obsadp(1,day,3),sign,acadp(1,day,3)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='12') then
                read(tempLine, *) obsadp(2,day,3),sign,acadp(2,day,3)
                outputLine=outputLine+1
            else
            end if
           end if

          if (varname=='WIND' .and. obstype=='AIRCFT') then
            if (VERFTIME(9:10)=='00') then
                read(tempLine, *) obsair(1,day,1),sign,acair(1,day,1)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='03') then
                read(tempLine, *) obsair(2,day,1),sign,acair(2,day,1)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='06') then
                read(tempLine, *) obsair(3,day,1),sign,acair(3,day,1)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='09') then
                read(tempLine, *) obsair(4,day,1),sign,acair(4,day,1)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='12') then
                read(tempLine, *) obsair(5,day,1),sign,acair(5,day,1)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='15') then
                read(tempLine, *) obsair(6,day,1),sign,acair(6,day,1)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='18') then
                read(tempLine, *) obsair(7,day,1),sign,acair(7,day,1)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='21') then
                read(tempLine, *) obsair(8,day,1),sign,acair(8,day,1)
                outputLine=outputLine+1
            else
           end if
           endif

           if (varname=='TEMP'.and. obstype=='AIRCFT') then
            if (VERFTIME(9:10)=='00') then
                read(tempLine, *) obsair(1,day,2),sign,acair(1,day,2)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='03') then
                read(tempLine, *) obsair(2,day,2),sign,acair(2,day,2)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='06') then
                read(tempLine, *) obsair(3,day,2),sign,acair(3,day,2)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='09') then
                read(tempLine, *) obsair(4,day,2),sign,acair(4,day,2)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='12') then
                read(tempLine, *) obsair(5,day,2),sign,acair(5,day,2)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='15') then
                read(tempLine, *) obsair(6,day,2),sign,acair(6,day,2)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='18') then
                read(tempLine, *) obsair(7,day,2),sign,acair(7,day,2)
                outputLine=outputLine+1
            else if (VERFTIME(9:10)=='21') then
                read(tempLine, *) obsair(8,day,2),sign,acair(8,day,2)
                outputLine=outputLine+1
            else
           end if
           endif

          a=0
          b=0
          if (obstype=='ADPSFC') then
            if (varname=='WIND' .or. varname=='TEMP' .or.varname=='SPHU') then
                read(tempLine, *) a,sign,b
                obssfc(day)=obssfc(day)+a
                acsfc(day)=acsfc(day)+b
              outputLine=outputLine+1
            else
            end if
          end if

        dayold=dayc
        enddo !!! end do for file loop

        close(21)

      ! 00+12 for adp
      do var=1,3
      do ii=1,Nday
       sum=0
       do jj=1,2
!       if(var.eq.2) print*,'before sum, sum=',sum
        sum=sum+obsadp(jj,ii,var)
!       if(var.eq.2) print*,'after sum, sum=',sum
       enddo
       obsadp(3,ii,var)=sum
       sum=0
       do jj=1,2
         sum=sum+acadp(jj,ii,var)
       enddo
       acadp(3,ii,var)=sum
      enddo
      enddo

      ! 00+03+..+21 for air
      do var=1,2
      do ii=1,Nday
       sum=0
       do jj=1,8
        sum=sum+obsair(jj,ii,var)
       enddo
       obsair(9,ii,var)=sum
       sum=0
       do jj=1,8
         sum=sum+acair(jj,ii,var)
       enddo
       acair(9,ii,var)=sum
      enddo
      enddo

      ! per for adp and air (wind,T,q seperate)
      do var=1,3
      do ii=1,Nday
        if (abs(obsadp(3,ii,var)) .gt. 1e-5) then
          peradp(ii,var)=(real(obsadp(3,ii,var))-real(acadp(3,ii,var)))*100/real(obsadp(3,ii,var))
        else
          obsadp(3,ii,var)=undefined 
          peradp(ii,var)=undefined
        endif
!       if(var.eq.2) then
!         print*,'obsadp(3,ii,var)=',obsadp(3,ii,var)
!         print*,'real(obsadp(3,ii,var))=',real(obsadp(3,ii,var))
!         print*,'acadp(3,ii,var)=',acadp(3,ii,var)
!         print*,'real(acadp(3,ii,var))=',real(acadp(3,ii,var))
!         print*,'peradp(ii,var)=',peradp(ii,var)
!       endif
      end do
      end do

      do var=1,2
      do ii=1,Nday
         if (abs(obsair(9,ii,var)) .gt. 1e-5) then
           perair(ii,var)=(real(obsair(9,ii,var))-real(acair(9,ii,var)))*100/real(obsair(9,ii,var))
         else
           obsair(9,ii,var)=undefined
           perair(ii,var)=undefined  
         endif
      enddo
      enddo

      !per for sfc
      do ii=1,Nday
        if (abs(obssfc(ii)) .gt. 1e-5) then
         persfc(ii)=(real(obssfc(ii))-real(acsfc(ii)))*100/real(obssfc(ii))
        else
         obssfc(ii)=undefined
         persfc(ii)=undefined
        endif
      enddo

      write(41,300) 'adpupa','aircft','adpsfc'
      write(41,400) 'WIND','TEMP','SPHU','WIND','TEMP','W+T+Q'
      write(41,500) ('obs   rej(%)',ii=1,6)
      write(41,*)
300   format(20x,A6,30x,A6,15x,A6)
400   format(8x,2(A4,9x),2X,3(A4,10x),A5)
500   format(5x,6(A12,2x))

      do ii=1,Nday
      write(41,100)(obsadp(3,ii,var),peradp(ii,var),var=1,3),(obsair(9,ii,var),perair(ii,var),var=1,2),obssfc(ii),persfc(ii)
100   format(1x,6(i8,f6.1))
      enddo

      irec=1
      do ii=1,Nday
        do var=1,3
         realdata=obsadp(3,ii,var)
         write(31,rec=irec) realdata
         irec=irec+1
         write(31,rec=irec) peradp(ii,var)
         irec=irec+1
        enddo
        do var=1,2
         realdata=obsair(9,ii,var)
         write(31,rec=irec) realdata
         irec=irec+1
         write(31,rec=irec) perair(ii,var)
         irec=irec+1
        enddo
        realdata=obssfc(ii)
        write(31,rec=irec) realdata
        irec=irec+1
        write(31,rec=irec) persfc(ii)
        irec=irec+1
      enddo


    close(31)
    close(41)
    stop
    end

