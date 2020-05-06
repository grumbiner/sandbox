      PROGRAM oeschger
C     Attempt to simulate the results of the Oeschger model
C        as presented in Broecker and Peng.
      IMPLICIT none
      
      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)
      INTEGER nlayer
      PARAMETER (nlayer = 76)
      DOUBLE PRECISION deltaz, deltat
      PARAMETER (deltaz = 50.D0)
      PARAMETER (deltat = secpyr/5.D0)
      
      DOUBLE PRECISION phos(nlayer), phost(nlayer)
      DOUBLE PRECISION w(nlayer), kappa(nlayer)
      
      INTEGER i, j, lenrun
      DOUBLE PRECISION sum1

C     Set up parameters and files      
      CALL oestart(phos, kappa, w, lenrun, deltaz)
      PRINT *,'returned from oestart'
 
C     Now ready to compute the evolution of the carbon and phosphate
C       distributions.  Equilibration time is something of a mystery.
C       Start out with a 50 year run

      PRINT *,'time at start', LONG(362)
      DO 1000 j = 1, lenrun*INT(secpyr/deltat)

        CALL advdif(deltaz, deltat, phos, kappa, w)
       
        IF (MOD((j-1),50*INT(secpyr/deltat)) .EQ. 0) THEN
          CALL theory(phost, kappa, w, deltaz, j, deltat)
          WRITE (10, 9002) (i,CHAR(9),phos(i),
     1                        CHAR(9),phost(i),i=1, nlayer)
          sum1 = 0.D0
          DO 2000 i = 2, nlayer-1
            sum1 = sum1 + phos(i)
 2000     CONTINUE
          WRITE (*, 9005) sum1
        ENDIF
        
 1000 CONTINUE

      PRINT *,'time at end', LONG(362)
      PAUSE
      CLOSE(10, STATUS='KEEP')
      
 9002 FORMAT (I2,A1,E12.4,A1,E12.4)
 
 9005 FORMAT (1X,D17.9)
      
      END
C*************************************************----------++++++++++!!
      SUBROUTINE oestart(phos, kappa, w, lenrun, deltaz)
      
      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)
      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)

      DOUBLE PRECISION phos(nlayer), kappa(nlayer), w(nlayer)
      CHARACTER*60 fname
      INTEGER lenrun, i
      DOUBLE PRECISION x, deltaz

C     Initialize the carbon and phosphate concentrations
C       Concentrations are in Moles per cubic meter
      DO 100 i = 1, nlayer
        x = (DBLE(i-1))/DBLE(nlayer-1)
        phos(i)   = x*x*(1.-x)*(1.-x)
  100 CONTINUE
  
C     Initialize the diffusivity (meters squared per second)
      DO 200 i = 1, nlayer
        x = (DBLE(i-1))/DBLE(nlayer-1)
        kappa(i) = 0.5D-4*x*(1.-x)
  200 CONTINUE
CD      DO 210 i = 16, nlayer
CD        kappa(i) = 0.5D-4
CD  210 CONTINUE
CD      kappa(15) = 1.6D-4
  
C     Initialize the vertical upwelling (meters per second)
      DO 300 i = 1, 20
        w(i) = 0.D0
  300 CONTINUE
      DO 310 i = 21, nlayer
        w(i) = 0.0D0/secpyr
  310 CONTINUE
  
C     Open the output file for ncsa.
      PRINT *,'What is the output file name?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      
C     Determine the length of the run:
      PRINT *,'How long a run (approx 8 years per second)'
      READ (*,9003) lenrun
      
 9001 FORMAT (A60)
 9003 FORMAT (BN, I5)

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE advdif(deltaz, deltat, t, kappa, w)
C     Compute a purely advective-diffusive solution, no
C       internal sources or sinks, and no flux boundary conditions.
C     BG 5/17/91.

      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)

      DOUBLE PRECISION deltaz, deltat
      DOUBLE PRECISION t(nlayer), kappa(nlayer), w(nlayer)
      DOUBLE PRECISION f2(nlayer), f3(nlayer), fp(nlayer)
        
      INTEGER i
        
      DO 1000 i = 2, nlayer-1

        f2(i) = (   kappa(i+1)          *t(i+1)
     1            -(kappa(i+1)+kappa(i))*t(i)
     2            + kappa(i)            *t(i-1) )
     3    /deltaz/deltaz
        f3(i) = - (t(i+1)-t(i-1))*(w(i+1)*w(i))/(4.*deltaz)
        fp(i) = f2(i) + f3(i)
          
 1000 CONTINUE
           
C     Extrapolate the interior
      DO 1200 i = 2, nlayer-1
        t(i)   = t(i) + deltat * fp(i)
 1200 CONTINUE
 
C     Now apply a no flux diffusion condition
      t(nlayer)   = t(nlayer-1)
      t(1)        = t(2)

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE theory(phos, kappa, w, deltaz, step, deltat)
C
C     Compute the theoretical solution for the evolution of the 
C       cos function initial condition.

      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)
        
      DOUBLE PRECISION phos(nlayer), kappa(nlayer), w(nlayer)
      DOUBLE PRECISION deltaz, deltat
      INTEGER i, step
      DOUBLE PRECISION dummy, x

      DO 100 i = 1, nlayer
        x = (DBLE(i-1))/DBLE(nlayer-1)
        phos(i)   = x*x*(1.-x)*(1.-x)
  100 CONTINUE
  
      dummy = EXP(-0.5D-4
     1            /FLOAT(nlayer-1)/deltaz/FLOAT(nlayer-1)/deltaz
     2           *DBLE(step)*deltat )

      DO 200 i = 1, nlayer
        phos(i) = phos(i)*dummy
  200 CONTINUE
  
      RETURN
      END
