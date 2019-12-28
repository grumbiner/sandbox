C*************************************************----------++++++++++!!
      PROGRAM rkchem
C     Core code taken from Numerical Recipes
C     Typography and declarations changed from Numerical Recipes
C       format to BG format.

      IMPLICIT none

      INTEGER nvar, nstep
      PARAMETER (nvar = 12)
      REAL xstart(nvar), v(nvar), dv(nvar)
      REAL t1, t2, dt
      REAL secpyr
      PARAMETER (secpyr = 365.2422*86400.)
      CHARACTER*60 fname
  
      INTEGER i, k, nout
      REAL  x, h

      DO 1000 i = 1, 4
C       Phosphorous - present ocean mean
        xstart(i) = 2.10E-6
 1000 CONTINUE

C     C14
      PRINT *,'Initial AA del-14C?  Pres = -155.E-3'
CD      READ (*,9001) xstart(5)
      PRINT *,'Initial NA del-14C?  Pres = - 70.E-3'
CD      READ (*,9001) xstart(6)
      PRINT *,'Initial WO del-14C?  Pres = -175.E-3'
CD      READ (*,9001) xstart(7)
      PRINT *,'Initial WU del-14C?  Pres = - 65.E-3'
CD      READ (*,9001) xstart(8)
      xstart(5) = -.155
      xstart(6) = -.07
      xstart(7) = -.175
      xstart(8) = -.065

C     O2
      PRINT *,'Initial AA O2?  Pres = 235.E-6'
CD      READ (*,9001) xstart(9)
      PRINT *,'Initial NA O2?  Pres = 285.E-6'
CD      READ (*,9001) xstart(10)
      PRINT *,'Initial AA O2?  Pres = 168.E-6'
CD      READ (*,9001) xstart(11)
      PRINT *,'Initial AA O2?  Pres = 147.E-6'
CD      READ (*,9001) xstart(12)
      xstart(9) = 235.e-6
      xstart(10) = 285.e-6
      xstart(11) = 168.e-6
      xstart(12) = 147.e-6

      PRINT *,'Start time?'
CD      READ (*,9001) t1
      PRINT *,'End time?'
CD      READ (*,9001) t2
      t1 = -20000.
      t2 = 0.

      t1 = t1*secpyr
      t2 = t2*secpyr
      PRINT *,'What is the time step (yrs)?'
CD      READ (*,9001) dt
      dt = 25.
      nstep = INT((t2-t1)/dt/secpyr)+1
      PRINT *,'Output every ? steps?'
CD      READ (*,9002) nout
      nout = 40

      PRINT *,'Output file name?'
      READ (*,9004) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='NEW')

CM      PRINT *,'Time start',LONG(362)
C     Core Numerical Recipes loop  
      DO 11 i=1, nvar
        v(i)=xstart(i)
 11   CONTINUE

      x=t1
      h=dt*secpyr
      DO 13 k=1,nstep
        CALL derivs(x, v, dv)
        CALL rk4(v, dv, nvar, x, h, v, derivs)
        IF (x+h.eq.x) PAUSE 'Stepsize not significant in RKDUMB.'
        x=x+h
        IF (MOD(k-1,nout) .EQ. 0) THEN
          WRITE (*,9003) (x-h)/secpyr,
     1    CHAR(9), v(1)*1.E6,
     2    CHAR(9), v(2)*1.E6,
     3    CHAR(9), v(3)*1.E6,
     4    CHAR(9), v(4)*1.E6,
     1    CHAR(9),-v(5)*1.E3,
     2    CHAR(9),-v(6)*1.E3,
     3    CHAR(9),-v(7)*1.E3,
     4    CHAR(9),-v(8)*1.E3,
     1    CHAR(9), v(9)*1.E6,
     2    CHAR(9), v(10)*1.E6,
     3    CHAR(9), v(11)*1.E6,
     4    CHAR(9), v(12)*1.E6

          WRITE (11,9003) (x-h)/secpyr,
     1    CHAR(9), v(1)*1.E6, 
     2    CHAR(9), v(2)*1.E6,
     3    CHAR(9), v(3)*1.E6,
     4    CHAR(9), v(4)*1.E6,
     1    CHAR(9),-v(5)*1.E3,
     2    CHAR(9),-v(6)*1.E3,
     3    CHAR(9),-v(7)*1.E3,
     4    CHAR(9),-v(8)*1.E3,
     1    CHAR(9), v(9)*1.E6,
     2    CHAR(9), v(10)*1.E6,
     3    CHAR(9), v(11)*1.E6,
     4    CHAR(9), v(12)*1.E6
      ENDIF

 13   CONTINUE

CM      PRINT *,'time end',LONG(362)
 9001 FORMAT (E13.6)
 
 9002 FORMAT (I3)
 
 9003 FORMAT (F8.0,4(A1,F6.4),8(A1,F5.1))
 
 9004 FORMAT (A60)

CD      PAUSE
      END
