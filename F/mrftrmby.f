      PROGRAM mrftrmby
C     Trim buoy files so that only have 1 'ob' per forecast point/step.
C     Bob Grumbine 21 April 1994.

      IMPLICIT none

      INTEGER nmax
      PARAMETER (nmax = 2000)

      CHARACTER*7 name(nmax)
      REAL lat(nmax), long(nmax), modp(nmax), obsp(nmax), delp(nmax)
      REAL modt(nmax), obst(nmax), delt(nmax)
      REAL modp2(nmax), obsp2(nmax)
      REAL delp2(nmax)
      REAL modt2(nmax), obst2(nmax), delt2(nmax), imp(nmax)
      INTEGER date(nmax)

      CHARACTER*60 fname
      INTEGER ntot, nrun, i
      REAL sbgt, szt, simp, simp2, szt2, sbgt2

      PRINT *,'What is the name of the spliced file?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      PRINT *,'What is the name of the trimmed file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='NEW')
 9001 FORMAT (A60)
 
      i = 1
      nrun = 1
      READ (10,9005, END=1001) name(i), lat(i), long(i), date(i), 
     1  obsp(i), delp(i), delp2(i), obst(i), delt(i), delt2(i)
     1   , imp(i)
 1000 CONTINUE
        i = i + 1
        READ (10,9005, END=1001) name(i), lat(i), long(i), date(i), 
     1    obsp(i), delp(i), delp2(i), obst(i), delt(i), delt2(i)
     1     , imp(i)
        IF (name(i) .NE. name(i-1)) THEN
          nrun = 1
          WRITE (11,9005) name(i-1), lat(i-1), long(i-1), date(i-1), 
     1    obsp(i-1), delp(i-1), delp2(i-1), obst(i-1), delt(i-1), 
     1    delt2(i-1), imp(i-1)
         ELSE IF ( ABS(date(i)-date(i-1)) .LE. 6 .OR.
     1             ((date(i) - date(i-1)) .GT. 24 .AND.
     2              (date(i) - date(i-1)) .LE. 82     )   ) THEN
          nrun = nrun + 1
          lat(i) = (lat(i-1)*(nrun-1)+lat(i))/FLOAT(nrun)
          long(i) = (long(i-1)*(nrun-1)+long(i))/FLOAT(nrun)
          obsp(i) = (obsp(i-1)*(nrun-1)+obsp(i))/FLOAT(nrun)
          delp(i) = (delp(i-1)*(nrun-1)+delp(i))/FLOAT(nrun)
          delp2(i) = (delp2(i-1)*(nrun-1)+delp2(i))/FLOAT(nrun)
          obst(i) = (obst(i-1)*(nrun-1)+obst(i))/FLOAT(nrun)
          delt(i) = (delt(i-1)*(nrun-1)+delt(i))/FLOAT(nrun)
          delt2(i) = (delt2(i-1)*(nrun-1)+delt2(i))/FLOAT(nrun)
          imp(i) = (imp(i-1)*(nrun-1)+imp(i))/FLOAT(nrun)
         ELSE
          nrun = 1
          WRITE (11,9005) name(i-1), lat(i-1), long(i-1), date(i-1), 
     1    obsp(i-1), delp(i-1), delp2(i-1), obst(i-1), delt(i-1), 
     1    delt2(i-1), imp(i-1)
        ENDIF
        GO TO 1000
 1001 CONTINUE
      ntot = i - 1

 9005 FORMAT (A7, F5.1, F6.1, I9, F7.1, 2F6.1, 2x, F6.1, 3F6.1)

      STOP
      END
