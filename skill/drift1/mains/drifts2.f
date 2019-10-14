      PROGRAM drifts2
C     Compute kinematic statistics from drifting buoys
C     Assume that we're working with a single buoy file.

      IMPLICIT none

      INTEGER nobs
      PARAMETER (nobs = 1300)
      CHARACTER*7 id(nobs), name
      INTEGER code(nobs), lat(nobs), long(nobs)
      REAL rlat(nobs), rlong(nobs)
      INTEGER dp(nobs), p(nobs), dt(nobs), t(nobs), ddir(nobs), 
     1    dir(nobs), dsp(nobs), sp(nobs),
     3    dsst(nobs), sst(nobs)
      CHARACTER*9 date(nobs)
      INTEGER yy(nobs), dd(nobs), mm(nobs), hh(nobs)

      INTEGER i, j, ntot

C     Kinematic Variables:
      REAL ds(nobs), tbar(nobs), time(nobs)
      REAL u(nobs), ddtp(nobs), ddtm(nobs), ddsp(nobs), ddsm(nobs)

      REAL arcdis, saccur, taccur
      INTEGER delay
      PARAMETER (saccur = 1.0)
      PARAMETER (taccur = 0.4)

C*************************************************************
      OPEN (10, FILE='tbuoy', FORM='FORMATTED', STATUS='OLD')
      OPEN (11, FILE='tout', FORM='FORMATTED')
	  
 9002 FORMAT (A60)

      i = 0
 1000 CONTINUE
        i = i + 1
        READ (10,9001,END=2000,ERR=1001)
     1       id(i), code(i), lat(i), long(i), date(i), 
     2    dp(i), p(i), dt(i), t(i), ddir(i), dir(i), dsp(i), sp(i),
     3    dsst(i), sst(i)
        READ (date(i), 9003) yy(i), mm(i), dd(i), hh(i)

C     Convert to degrees lat, long
        IF (ABS(lat(i)) .GT. 9000. .OR. long(i) .GT. 360.*100.) THEN
          i = i - 1
          GO TO 1000
         ELSE 
          rlat(i)  = lat(i)/100.
          rlong(i) = long(i)/100.
        ENDIF
C--------------------------------------
C       Now do things which depend on there being more than one data point.
        IF (i .LE. 0) THEN
	  i = 0
	  GO TO 1000
	 ELSE IF (i .EQ. 1) THEN
          time(1) = 0.0
	  GO TO 1000
	ENDIF
C     Cut out repeated times.  Take the first as the 'true'.
        IF (date(i) .EQ. date(i-1)) THEN
          i = i - 1
	  GO TO 1000
        ENDIF
C     Compute displacements and delays
        ds(i-1) = arcdis(rlong(i-1), rlat(i-1), rlong(i), rlat(i))
        time(i) = time(i-1)+delay(yy(i), mm(i), dd(i), hh(i),
     1                            yy(i-1), mm(i-1), dd(i-1), hh(i-1) )

C     Compute speeds (the if test should be redundant)
        IF (time(i) .GT. time(i-1) ) THEN
          u(i-1) = ds(i-1)/(time(i) - time(i-1))
          ddtp(i-1) = ds(i-1) / (time(i) - time(i-1) + taccur)
          ddtm(i-1) = ds(i-1) / (time(i) - time(i-1) - taccur)
          ddsp(i-1) = (ds(i-1)+saccur) / (time(i) - time(i-1))
          ddsm(i-1) = (ds(i-1)-saccur) / (time(i) - time(i-1))
         ELSE
          PRINT *,'Duplicated time ', i, date(i), date(i-1),
     1                   yy(i), mm(i), dd(i), hh(i),
     1                            yy(i-1), mm(i-1), dd(i-1), hh(i-1) 
          u(i-1) = 9.99
          ddtp(i-1) =  9.99
          ddtm(i-1) =  9.99
          ddsp(i-1) =  9.99
          ddsm(i-1) =  9.99
        ENDIF
        tbar(i-1) = 0.5*(time(i) + time(i-1) )
	
        IF (i .GE. nobs ) GO TO 2000
        GO TO 1000
 1001 CONTINUE
        i = i - 1
        PRINT *,'Read error ',i
        GO TO 1000

 2000 CONTINUE
      
 9001 FORMAT (A7, I4, I5, I6, A9, 5(I5,I4))
 9003 FORMAT (1X,4I2)
      
      ntot = i - 1
      PRINT *,'Found ',ntot,' buoys'

      tbar(ntot) = 0.5*time(ntot)
      ds(ntot)   = arcdis(rlong(1), rlat(1), rlong(ntot), rlat(ntot))
      u(ntot)    = ds(ntot) / (time(ntot) - time(1)         )
      ddtp(ntot) = ds(ntot) / (time(ntot) - time(1) + taccur)
      ddtm(ntot) = ds(ntot) / (time(ntot) - time(1) - taccur)
      ddsp(ntot) = (ds(ntot)+saccur) / (time(ntot) - time(1))
      ddsm(ntot) = (ds(ntot)-saccur) / (time(ntot) - time(1))
      PRINT *,'Passed dds'
      PRINT *,'Starting write loop'
      DO 4100 i = 1, ntot
        WRITE (11,9010) mm(i), dd(i), hh(i), 
     1           tbar(i), rlat(i), rlong(i), 
     1           u(i), ddtp(i), ddtm(i), ddsp(i), ddsm(i)
 4100 CONTINUE
 9010 FORMAT (3(I2, 1x), 2x, F6.1, 2F7.2, 5F8.3)

      STOP
      END
