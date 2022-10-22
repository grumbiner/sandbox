C*************************************************----------++++++++++!!
      PROGRAM rkchem
C     Core code taken from Numerical Recipes
C     Typography and declarations changed from Numerical Recipes 
C       format to BG format.

      IMPLICIT none
  
      INTEGER nvar, nstep
      PARAMETER (nvar = 23)
      REAL xstart(nvar), v(nvar), dv(nvar), tco2(6)
      REAL t1, t2, dt, x, h, secpyr, seelev, salt
      PARAMETER (secpyr = 365.2422*86400.)
      CHARACTER*60 fname
      DOUBLE PRECISION yinit, zinit, dco2, co2, pco2, temp
      INTEGER i, k, nout

      salt = 34.7
      seelev = 3730./(3730.-150.)
      salt = salt*seelev
      temp = 20.D0
      DO 1000 i = 1, 4
C       Phosphorous - present ocean mean
        xstart(i) = 2.10E-6*seelev
C       Alkalinity - present ocean mean
        xstart(i+12) = (2365.E-6 - 2.7E-6)*seelev
        tco2(i) = (2250.E-6 - 14.4E-6)*seelev
        xstart(i+17) = tco2(i)
 1000 CONTINUE
      xstart(17) = 2280.E-6*seelev
      tco2(5)    = 1961.E-6*seelev
      tco2(6)    =  280.E-6  !Atmosphere
      xstart(22) = tco2(5)
      xstart(23) = tco2(6)
      yinit = DBLE(0.9*xstart(22))
      zinit = DBLE(0.1*xstart(22))
      co2 = dco2(DBLE(tco2(5)), DBLE(xstart(17)), temp, salt, 
     1                                 0.D0, yinit, zinit)
      PRINT *,0.0 , co2*1.E6, 
     1            pco2(co2, temp, salt, 0.D0)*1.E6
      IF (pco2(co2, temp, salt, 0.D0)*1.E6 .GT. 400.) STOP

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

      t1 = -20000.
      t2 = 0.
      t1 = t1*secpyr
      t2 = t2*secpyr
      dt = 25.
      nout = 10
      
      PRINT *,'Start time?'
CD      READ (*,9001) t1
      PRINT *,'End time?'
CD      READ (*,9001) t2
      PRINT *,'What is the time step (yrs)?'
CD      READ (*,9001) dt
      nstep = INT((t2-t1)/dt/secpyr)+1
      PRINT *,'Output every ? steps?'
CD      READ (*,9002) nout

      PRINT *,'Output file name?'
      READ (*,9004) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='NEW')

      PRINT *,'Time start',LONG(362)
C     Core Numerical Recipes loop  
      DO 11 i=1, nvar
        v(i)=xstart(i)
 11   CONTINUE

      x=t1
      h=dt*secpyr
      DO 13 k=1,nstep
        CALL derivs(x, v, dv, tco2)
        CALL rk4(v, dv, nvar, x, h, v, derivs)
        IF (x+h.eq.x) PAUSE 'Stepsize not significant in RKDUMB.'
        x=x+h
        tco2(1) = v(18)
        tco2(2) = v(19)
        tco2(3) = v(20)
        tco2(4) = v(21)
        tco2(5) = v(22)
        tco2(6) = v(23)
        IF (v(11) .LT. 0.0) v(11) = 0.0

        IF (MOD(k-1,nout) .EQ. 0) THEN
          co2 = dco2(DBLE(tco2(5)), DBLE(v(17)), temp, salt, 
     1                                 0.D0, yinit, zinit)
          WRITE (*,9003) (x-h)/secpyr, 
C         Phosphate, delta14C, Oxygen, Alkalinity, co2
CD     1    CHAR(9), v(1)*1.E6, 
CD     2    CHAR(9), v(2)*1.E6,
     3    CHAR(9), v(3)*1.E6, 
     4    CHAR(9), v(4)*1.E6,
CD     1    CHAR(9),-v(5)*1.E3,
CD     2    CHAR(9),-v(6)*1.E3,
CD     3    CHAR(9),-v(7)*1.E3,
CD     4    CHAR(9),-v(8)*1.E3,
CD     1    CHAR(9), v(9)*1.E6,
CD     2    CHAR(9), v(10)*1.E6,
     3    CHAR(9), v(11)*1.E6,
     4    CHAR(9), v(12)*1.E6,
     1    CHAR(9), v(17)*1.E6,
CD     2    CHAR(9), v(13)*1.E6,
CD     3    CHAR(9), v(14)*1.E6,
     4    CHAR(9), v(15)*1.E6,
CD     5    CHAR(9), v(16)*1.E6,
CD     1    CHAR(9), v(18)*1.E6,
CD     2    CHAR(9), v(19)*1.E6,
     3    CHAR(9), v(20)*1.E6,
     4    CHAR(9), v(21)*1.E6,
     5    CHAR(9), v(22)*1.E6,
CD     6    CHAR(9), v(23)*1.E6,
CD     1    CHAR(9),   co2*1.E6, 
     1    CHAR(9), pco2(co2, temp, salt, 0.D0)*1.E6

          WRITE (11,9003) (x-h)/secpyr, 
C         Phosphate, delta14C, Oxygen, Alkalinity, co2
CD     1    CHAR(9), v(1)*1.E6, 
CD     2    CHAR(9), v(2)*1.E6,
     3    CHAR(9), v(3)*1.E6, 
     4    CHAR(9), v(4)*1.E6,
CD     1    CHAR(9),-v(5)*1.E3,
CD     2    CHAR(9),-v(6)*1.E3,
CD     3    CHAR(9),-v(7)*1.E3,
CD     4    CHAR(9),-v(8)*1.E3,
CD     1    CHAR(9), v(9)*1.E6,
CD     2    CHAR(9), v(10)*1.E6,
     3    CHAR(9), v(11)*1.E6,
     4    CHAR(9), v(12)*1.E6,
     1    CHAR(9), v(17)*1.E6,
CD     2    CHAR(9), v(13)*1.E6,
CD     3    CHAR(9), v(14)*1.E6,
     4    CHAR(9), v(15)*1.E6,
CD     5    CHAR(9), v(16)*1.E6,
CD     1    CHAR(9), v(18)*1.E6,
CD     2    CHAR(9), v(19)*1.E6,
     3    CHAR(9), v(20)*1.E6,
     4    CHAR(9), v(21)*1.E6,
     5    CHAR(9), v(22)*1.E6,
CD     6    CHAR(9), v(23)*1.E6,
CD     1    CHAR(9),   co2*1.E6, 
     1    CHAR(9), pco2(co2, temp, salt, 0.D0)*1.E6
     
      ENDIF
        
 13   CONTINUE

      PRINT *,'time end',LONG(362)
 9001 FORMAT (E13.6)
 
 9002 FORMAT (I3)
CD  9003 FORMAT (F8.0,4(A1,F6.4),4(A1,F5.1),5(A1,F7.1))
 9003 FORMAT (F8.0,2(A1,F6.4),12(A1,F6.1))
 
 9004 FORMAT (A60)

      PAUSE
      END
