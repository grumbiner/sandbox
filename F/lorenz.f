!Date:    1 June 1990, 13:45:52 EDT
! 
!Here is my Lorenz code.  Last time I ran this, I calculated for 100,000
!iterations.  I wanted to look at the first 10000, last 10000 and all
!100000 points seperately; hence, three output files.
! 
!Good luck.
!Bob  T
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      Program generates time series using Lorenz model.  Model
c      is integrated using the improved Euler scheme.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      program lorenz
c
c      Declare all floating point numbers as double precission
c
      implicit real*8 (a-h,o-z)
c
c      Initialize parameters.
c
c      n      number of equations
c      dt      time interval; initalized as double precission
c      nits      number of iterations
c
      parameter(n=3,dt=.005d0,nits=110000)
c
c      Dimension arrays
c
      real*8 x(n),xest(n),xprime(n)
c
c      Open output unformatted file for data
c
      open(unit=21,file='RATFIRST.dat',access='sequential',
     &       form='formatted',status='new')
      open(unit=22,file='RAT.dat',access='sequential',
     &       form='formatted',status='new')
      open(unit=23,file='RATLAST.dat',access='sequential',
     &       form='formatted',status='new')
C
 10	format(1x,i6,2x,4(f10.6,2x))
c
c      Write out time series values for t=0.  Note that all output
c      is done in single precission.
c
      i=0
      t=0.d0
      x(1)=0.d0
      x(2)=1.d0
      x(3)=0.d0
      write(21,10)i,t,(x(j),j=1,n)
      write(22,10)i,t,(x(j),j=1,n)
c
c      Loop over all iterations.  Print out every nth set of values
c      where n is the second argument in the mod function.
c
      t=0.d0
      do 100 i=1,nits
      t=t+dt
            call solve(n,t,x,xest,xprime,dt)
c            write(22,10)i,t,(x(j),j=1,n)
            if(i .le. 10000) write(21,10)i,t,(x(j),j=1,n)
            if(i .ge. 100000) write(23,10)i,t,(x(j),j=1,n)
 
 100      continue
c
c      Close the output file
c
      close(21)
      close(22)
      close(23)
      stop
      end
