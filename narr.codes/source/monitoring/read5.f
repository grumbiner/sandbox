 PROGRAM read5_f90

    IMPLICIT NONE

    integer, parameter :: scalarNum=3   !T850, T200, RH850
    integer, parameter :: vectorNum=2   !V850, V200
!   integer, parameter :: totalDay=31   !total plot days
    integer :: totalDay

    character :: tempLine*300,datename*4,inputFileName*20 ! eta_20011101.vsdb
    character :: tempLineold*300
    character(len=12) :: tempName,vNo*5, model*9, fcstTime*4, verfTime, &
            verfType*8,verfGrid*10, s_v*7, varName*6, presLevel*7, eqSign*1
    real :: obsNum
    real(kind=4) :: meanS(scalarNum,5), meanV(vectorNum,7)

    integer :: inIoStatus21,inIoStatus11,chCount
    integer :: lineCount,  outputLine
    integer :: iHead, blankP, iVar,iTemp
    integer :: mm(12), md, ii, jj, irec,kk


!   character(len=*), parameter :: &
!           inputFilePath = '/gpfsuser/narr/output/stats_test00/'
    character(len=35) :: inputFilePath
    character*4 year
    integer :: absendDay,absendMon,absstaDay,absstaMon
    integer :: endMon,endDay,staMon,staDay

    data mm/31,28,31,30,31,30,31,31,30,31,30,31/

    read(5,123) year,endMon,endDay,totalDay,inputFilePath
123 format(a4,1x,i2,1x,i2,1x,i2,1x,a35)
    print*,'year,endMon,endDay,totalDay=',year,endMon,endDay,totalDay 
    print*,'inputFilePath=',inputFilePath
    open (21,file='out5.dat',status='unknown')
    open (31,file='out5.grd',form='unformatted',status='replace',&
             access='direct',recl=4)
    absendDay=0
    do ii=1,endMon-1
      absendDay=absendDay+mm(ii)
    enddo
    absendDay=absendDay+endDay
    irec=1
!   do  ii=1,12
!   do  jj=1,mm(ii)
!   do kk=0,21,3
    do jj=1,totalDay
!   do kk=0,21,3
      absstaDay=absendDay-totalDay+jj+1
      staMon=1
      do ii=1,11
        if(absstaDay<=mm(ii)) exit
        absstaDay=absstaDay-mm(ii)
        staMon=staMon+1
      enddo
      staDay=absstaDay-1
      md=staMon*100+staDay
!     md=ii*100+jj
!     print*,'ii,jj,kk=',ii,jj,kk
!     write (datename,'(2i2.2)') ii,jj
      write (datename,'(i4.4)') md
!     print*,'datename=',datename
      inputFileName='eds_'// year//datename//'.vsdb'
      print*,'inputFilePath//inputFileName=',inputFilePath//inputFileName

      open(11,file = inputFilePath//inputFileName, iostat=inIoStatus21,&
          status = 'old', action = 'read')

      outputLine=0
      meanS=9.999e+6
      meanV=9.999e+6

      if (inIoStatus21>0) then
        print*,'inputFileName=',inputFileName, '  doesnot exist'
      else
        print*,'inputFileName=',inputFileName

        do      !!!loop in the vsdb file for one day till the end of file

          tempLine = ''
          read(11, '(A)', iostat=inIoStatus11, advance='no', size=chCount) tempLine
          tempLineold=tempLine
          print*,'inIoStatus11=',inIoStatus11
          print*,'tempLine=',tempLine
          if (inIoStatus11== 145) exit !!!end of file
          if (chCount==0) cycle

          do iHead=1,10
            tempLine = adjustl(tempLine)
            blankP = index(tempLine, ' ')
            tempName = tempLine(1:blankP-1)
            tempLine(1:blankP-1) = ''
            select case (iHead)
                case(1)
                    vNo = tempName(1:len(vNo))
                case(2)
                    model = tempName(1:len(model))
                case(3)
                    fcstTime = tempName(1:len(fcstTime))
                case(4)
                    verfTime = tempName(1:len(verfTime))
                case(5)
                    verfType = tempName(1:len(verfType))
                case(6)
                    verfGrid = tempName(1:len(verfGrid))
                case(7)
                    s_v = tempName(1:len(s_v))
                case(8)
                    varName = tempName(1:len(varName))
                case(9)
                    presLevel = tempName(1:len(presLevel))
                case(10)
                    eqSign = tempName(1:len(eqSign))
            end select
          end do
!         print*,'after select'
!         print*,'model(1:6)=',model(1:6)
!         print*,'fcstTime(1:2)=',fcstTime(1:2)
!         print*,'verfType=',verfType
!         print*,'varname(1:4)=',varname(1:4)
!         print*,'presLevel(1:5)=',presLevel(1:5)

!         if (model(1:6)=='32KMRR' .and. fcstTime(1:2)=='00'.and. verfTime(9:10)=='00'  &
!         if (model(1:6)=='32KMRR' .and. fcstTime(1:2)=='00'.and. verfTime(9:10)=='00'  &
!           .and. verfType=='ADPUPA'.and.verfGrid=='G221') then
          if (model(1:6)=='32KMRR' .and. fcstTime(1:2)=='00'  &
            .and. verfType=='ADPUPA'.and.verfGrid=='G221') then
            if (varName(1:1)=='T' .and. presLevel(1:4)=='P850') then
!               print*,'in here?'
                print*,'tempLineold=',tempLineold
                read(tempLine, *) obsNum, (meanS(1,iTemp), iTemp=1,5)
!               print*,'obsNum=',obsNum
!               do iTemp=1,5
!                  print*,'meanS(1,iTemp)=',meanS(1,iTemp)
!               enddo
                outputLine=outputLine+1
            else if (varName(1:1)=='T' .and. presLevel(1:4)=='P200') then
                read(tempLine, *) obsNum, (meanS(2,iTemp), iTemp=1,5)
                outputLine=outputLine+1
            else if (varName(1:2)=='RH' .and. presLevel(1:4)=='P850') then
                read(tempLine, *) obsNum, (meanS(3,iTemp), iTemp=1,5)
                outputLine=outputLine+1
            else if (varName(1:4)=='VWND' .and. presLevel(1:4)=='P850') then
                read(tempLine, *) obsNum, (meanV(1,iTemp), iTemp=1,7)
                outputLine=outputLine+1
            else if (varName(1:4)=='VWND' .and. presLevel(1:4)=='P200') then
                read(tempLine, *) obsNum, (meanV(2,iTemp), iTemp=1,7)

                outputLine=outputLine+1
!           else
            end if
          end if
!         print*,'outputLine=',outputLine
        end do !!! end do for file loop for oneday
      end if
!     print*, 'outputLine=', outputLine
      close(11)

      write(21,100) ((meanS(iVar,iTemp), iTemp=1,5),iVar=1,scalarNum)
      write(21,200) ((meanV(iVar,iTemp), iTemp=1,7),iVar=1,vectorNum)
100   format(5e18.9)
200   format(7e18.9)

      do iVar=1,scalarNum
      do iTemp=1,5
        WRITE(31,rec=irec) meanS(iVar,iTemp)
        irec=irec+1
      end do
      end do
      do iVar=1,vectorNum
      do iTemp=1,7
        WRITE(31,rec=irec) meanV(iVar,iTemp)
        irec=irec+1
      end do
      end do


!   enddo
    enddo
    close(21)
    close(31)

  END PROGRAM read5_f90

