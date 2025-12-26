      program precipitation
      character a
      real pcp1(3000),pcp2(3000),pcp3(3000)
      integer totalHr
      open (10,file='pcpstats', status='unknown')
      open (20,file='pcp.out',form='unformatted',status='replace',access='direct',recl=4)
! *** read the data from pcpstats
      read(5,100) totalHr
      print*,'totalHr=',totalHr
100   format(i4)
      irec=1
      do i = 1,totalHr
         read (10,'(a)')a
         do j=1,4
         read (10,*) pcp1(i),pcp2(i),pcp3(i)
         write(20,rec=irec) pcp1(i)
         irec=irec+1
         write(20,rec=irec) pcp2(i)
         irec=irec+1
         write(20,rec=irec) pcp3(i)
         irec=irec+1
         print*, pcp3(i)
         enddo
         print*,'i=',i
      enddo
    close(10)
    close(20)
  
    stop
    end
