 PROGRAM readvsdblike_f90

    IMPLICIT NONE

!   integer, parameter :: totalDay=31   !total plot days
    integer, parameter :: alltimes=4   ! (03,09,15,21) or (06,12,18,00) per day
    integer i
    integer :: totalDay

      integer, dimension(8) :: idat
      integer, dimension(8) :: jdat
      real, dimension(5) :: rinc

    character :: tempLine*300,datename*8,inputFileName*63 ! eta_20011101.vsdb
    character(len=12) :: tempName,vNo*5, model*9, fcstTime*4, verfTime, &
            verfType*8,verfGrid*10, s_v*7, varName*8, eqSign*1
    real :: obsNum
    real(kind=4) :: meanLH1(alltimes,5), meanLH2(alltimes,5),meanSH1(alltimes,5), meanSH2(alltimes,5)

    integer :: inIoStatus21,inIoStatus11,chCount
    integer :: lineCount,  outputLine
    integer :: iHead, blankP, times,iTemp
    integer :: mm(12), md, ii, jj, irec31,irec41
    integer :: absendDay,absendMon,absstaDay,absstaMon
    integer :: endYear,endMon,endDay,staMon,staDay


!   character(len=*), parameter :: &
!           inputFilePath = '/gpfsuser/narr/output/vsdb/'
    character(len=256) :: inputFilePath
    character(len=256) :: filename
    character*4 year
    integer :: iyear


    data mm/31,28,31,30,31,30,31,31,30,31,30,31/

    read(5,123) year,endMon,endDay,totalDay,inputFilePath
    print*,'year,endMon,endDay=',year,endMon,endDay
    print*,'inputFilePath=',inputFilePath
123 format(a4,1x,i2,1x,i2,1x,i3,1x,a256)
      read(year,*)endYear
      idat=0
      idat(1)=endYear
      idat(2)=endMon
      idat(3)=endDay

      rinc=0.0
      rinc(1)=-totalDay

      call w3movdat(rinc,idat,jdat)

    open (21,file='vsdblike.dat',status='unknown')
    open (31,file='vsdblike_03-21.grd',form='unformatted',status='replace',&
             access='direct',recl=4)
    open (41,file='vsdblike_06-00.grd',form='unformatted',status='replace',&
             access='direct',recl=4)

    irec31=1
    irec41=1
    print*,'totalDay=',totalDay
    do jj=1,totalDay
         print*,'jj=',jj
         write (datename,'(i4.4,2i2.2)') jdat(1),jdat(2),jdat(3)
      inputFileName='eta_' //datename//'.vsdb'

      filename='/'//trim(adjustl(inputFilePath))//inputFileName

      open(11,file = filename, iostat=inIoStatus21,&
          status = 'old', action = 'read')

      outputLine=0
      meanLH1=9.999e+6
      meanLH2=9.999e+6
      meanSH1=9.999e+6
      meanSH2=9.999e+6

      if (inIoStatus21>0) then
        print*,'inputFileName=',filename, '  doesnot exist'
      else
        print*,'inputFileName=',filename

        do i=1,16     !!!loop in the vsdb file for one day till the end of file

          tempLine = ''
!         read(11, '(A)', iostat=inIoStatus11, advance='no', size=chCount,end=2003) tempLine
          read(11,2001,end=2003) tempLine
2001      format(a)
          print*,tempLine,inIoStatus11,chCount
          if (inIoStatus11== -1) exit !!!end of file
!         if (chCount==0) cycle

          do iHead=1,9
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
                    eqSign = tempName(1:len(eqSign))
            end select
          end do

          if (verfTime(9:10)=='03' .and. varName(1:1)=='L') then
                read(tempLine, *) obsNum, (meanLH1(1,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='06' .and. varName(1:1)=='L') then
                read(tempLine, *) obsNum, (meanLH2(1,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='03'.and. varName(1:1)=='S') then
                read(tempLine, *) obsNum, (meanSH1(1,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='06'.and. varName(1:1)=='S') then
                read(tempLine, *) obsNum, (meanSH2(1,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='09'.and. varName(1:1)=='L') then
                read(tempLine, *) obsNum, (meanLH1(2,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='12'.and. varName(1:1)=='L') then
                read(tempLine, *) obsNum, (meanLH2(2,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='09'.and. varName(1:1)=='S') then
                read(tempLine, *) obsNum, (meanSH1(2,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='12'.and. varName(1:1)=='S') then
                read(tempLine, *) obsNum, (meanSH2(2,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='15'.and. varName(1:1)=='L') then
                read(tempLine, *) obsNum, (meanLH1(3,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='18'.and. varName(1:1)=='L') then
                read(tempLine, *) obsNum, (meanLH2(3,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='15'.and. varName(1:1)=='S') then
                read(tempLine, *) obsNum, (meanSH1(3,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='18'.and. varName(1:1)=='S') then
                read(tempLine, *) obsNum, (meanSH2(3,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='21'.and. varName(1:1)=='L') then
                read(tempLine, *) obsNum, (meanLH1(4,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='00'.and. varName(1:1)=='L') then
                read(tempLine, *) obsNum, (meanLH2(4,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='21'.and. varName(1:1)=='S') then
                read(tempLine, *) obsNum, (meanSH1(4,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else if (verfTime(9:10)=='00'.and. varName(1:1)=='S') then
                read(tempLine, *) obsNum, (meanSH2(4,iTemp), iTemp=1,5)
                outputLine=outputLine+1
          else
          endif
        end do !!! end do for file loop for oneday
2003    continue
      end if
      print*, 'outputLine=', outputLine
      close(11)

      do times=1,alltimes
       write(21,100) (meanLH1(times,iTemp), iTemp=1,5)
       write(21,100) (meanLH2(times,iTemp), iTemp=1,5)
       write(21,100) (meanSH1(times,iTemp), iTemp=1,5)
       write(21,100) (meanSH2(times,iTemp), iTemp=1,5)
      enddo
100   format(5e18.9)

      do times=1,alltimes
       do iTemp=1,5
         WRITE(31,rec=irec31) meanLH1(times,iTemp)
         irec31=irec31+1
       enddo
       do iTemp=1,5
         WRITE(31,rec=irec31) meanSH1(times,iTemp)
         irec31=irec31+1
       enddo
       do iTemp=1,5
         WRITE(41,rec=irec41) meanLH2(times,iTemp)
         irec41=irec41+1
       enddo
       do iTemp=1,5
         WRITE(41,rec=irec41) meanSH2(times,iTemp)
         irec41=irec41+1
       end do
      end do

      rinc=0.0
      rinc(1)=1.0
      idat=jdat
      call w3movdat(rinc,idat,jdat)

    enddo !! END do for all chosen days
    close(21)
    close(31)
    close(41)

  END PROGRAM readvsdblike_f90

