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
      PARAMETER (deltat = secpyr/10.D0)
      
      DOUBLE PRECISION phos(nlayer), phost(nlayer)
      DOUBLE PRECISION w(nlayer), kappa(nlayer)
      
      INTEGER i, j, lenrun, outoft
      DOUBLE PRECISION sum1

C     Set up parameters and files
      CALL oestart(phos, kappa, w, lenrun, outoft, deltaz)
 
C     Now ready to compute the evolution of the carbon and phosphate
C       distributions.

      DO 1000 j = 1, lenrun*INT(secpyr/deltat)

        CALL advdif(deltaz, deltat, phos, kappa, w, j)
       
        IF (MOD((j-1),INT(FLOAT(outoft)*secpyr/deltat)) .EQ. 0) THEN
          WRITE (10, 9002) (i,CHAR(9),phos(i),
     1                        i=1, nlayer)
          sum1 = 0.D0
          DO 2000 i = 2, nlayer-1
            sum1 = sum1 + phos(i)
 2000     CONTINUE
          WRITE (*, 9005) sum1
        ENDIF
        
 1000 CONTINUE

      CLOSE(10, STATUS='KEEP')
      
 9002 FORMAT (I2,A1,E12.5)
CD 9002 FORMAT (I2,A1,E12.4,A1,E12.4)
 
 9005 FORMAT (1X,D17.9)
      
      END
C*************************************************----------++++++++++!!
      SUBROUTINE oestart(phos, kappa, w, lenrun, outoft, deltaz)
      
      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)
      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)

      DOUBLE PRECISION phos(nlayer), kappa(nlayer), w(nlayer)
      CHARACTER*60 fname
      INTEGER lenrun, outoft, i
      DOUBLE PRECISION deltaz

C     Initialize the carbon and phosphate concentrations
C       Concentrations are in Moles per cubic meter
      DO 100 i = 1, nlayer
        phos(i)   = DBLE(COS(ACOS(-1.)*
     1                (FLOAT(i)-1.5)/(FLOAT(nlayer)-2.))  )
  100 CONTINUE
  
C     Initialize the diffusivity (meters squared per second)
      DO 200 i = 1, nlayer
        kappa(i) = 1.6D-4
  200 CONTINUE
CD      DO 210 i = 16, nlayer
CD        kappa(i) = 0.5D-4
CD  210 CONTINUE
      kappa(15) = 0.5D-4
  
C     Initialize the vertical upwelling (meters per second)
      DO 300 i = 1, 20
        w(i) = 0.D0
  300 CONTINUE
      DO 310 i = 21, nlayer
        w(i) = 0.0D0/secpyr
  310 CONTINUE
C     The following is required to have advective scalar concentration
      w(1)        = 0.0D0
      w(2)        = 0.0D0
      w(nlayer)   = 0.0D0
      w(nlayer-1) = 0.0D0
  
C     Open the output file.
      PRINT *,'What is the output file name?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      
C     Determine the length of the run:
      PRINT *,'How long a run (approx 8 years per second)'
      READ (*,9003) lenrun
      PRINT *,'How often (years) do you want the output?'
      READ (*,9003) outoft
      
 9001 FORMAT (A60)
 9003 FORMAT (BN, I5)

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE advdif(deltaz, deltat, t, kappa, w, step)
C     Compute a purely advective-diffusive solution, no
C       internal sources or sinks, and no flux boundary conditions.
C     BG 5/17/91.

      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)

      DOUBLE PRECISION deltaz, deltat
      DOUBLE PRECISION t(nlayer), kappa(nlayer), w(nlayer)
      DOUBLE PRECISION f2(nlayer), f3(nlayer), fp(nlayer)
      
      DOUBLE PRECISION tnm1(nlayer), temp(nlayer)
      INTEGER i, step
      
      SAVE tnm1
      
      IF (step .EQ. 1) THEN
        DO 100 i = 1, nlayer
          tnm1(i) = t(i)
  100   CONTINUE
C       Since we are using a leapfrog scheme, we neet to make the first step 
C         with a different scheme.
        CALL advstr(deltaz, deltat, t, kappa, w, step)
        RETURN
      ENDIF
        
      DO 1000 i = 3, nlayer-2

        f2(i) = (   kappa(i+1)          *t(i+1)
     1            -(kappa(i+1)+kappa(i))*t(i)
     2            + kappa(i)            *t(i-1) )
     3    /deltaz/deltaz
     
        f3(i) = -(w(i+1)+w(i))*.5
     1           *(-t(i+2)+8.*t(i+1)-8.*t(i-1)+t(i-2))
     2           /(12.*deltaz)
     
        fp(i) = f2(i) + f3(i)
          
 1000 CONTINUE
      i = 2
        f2(i) = (   kappa(i+1)          *t(i+1)
     1            -(kappa(i+1)+kappa(i))*t(i)
     2            + kappa(i)            *t(i-1) )
     3    /deltaz/deltaz
        f3(i) = -( (t(i+1)-t(i  ))*w(i+1)
     1           + (t(i  )-t(i-1))*w(i  )        )/(2.*deltaz)
        fp(i) = f2(i) + f3(i)
     
      i = nlayer-1 
        f2(i) = (   kappa(i+1)          *t(i+1)
     1            -(kappa(i+1)+kappa(i))*t(i)
     2            + kappa(i)            *t(i-1) )
     3    /deltaz/deltaz
        f3(i) = -( (t(i+1)-t(i  ))*w(i+1)
     1           + (t(i  )-t(i-1))*w(i  )        )/(2.*deltaz)
        fp(i) = f2(i) + f3(i)

C     Extrapolate the interior
C     First, put the current (n) level into the temporary vector, where
C       on the next step, it will be n-1 level.

      DO 1210 i = 1, nlayer
        temp(i) = t(i)
 1210 CONTINUE
 
      DO 1200 i = 2, nlayer-1
        t(i)   = tnm1(i) + deltat * fp(i) * 2.
 1200 CONTINUE
 
C     Now apply a no flux diffusion condition
      t(nlayer)   = t(nlayer-1)
      t(1)        = t(2)
      
C     Put the temporary data into the saved vector (n-1 on the next step)
      DO 2000 i = 1, nlayer
        tnm1(i) = temp(i)
 2000 CONTINUE

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE advstr(deltaz, deltat, phos, kappa, w, j)
C     Compute a purely advective-diffusive solution, no
C       internal sources or sinks, and no flux boundary conditions.
C     BG 5/17/91.

      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)

      DOUBLE PRECISION deltaz, deltat
      DOUBLE PRECISION t(nlayer), kappa(nlayer), w(nlayer)
      DOUBLE PRECISION f2(nlayer), f3(nlayer), fp(nlayer)
      DOUBLE PRECISION phos(nlayer)
        
      INTEGER i, j
        
      DO 1000 i = 2, nlayer-1

        f2(i) = (   kappa(i+1)          *t(i+1)
     1            -(kappa(i+1)+kappa(i))*t(i)
     2            + kappa(i)            *t(i-1) )
     3    /deltaz/deltaz
     
        f3(i) = -( (t(i+1)-t(i  ))*w(i+1)
     1           + (t(i  )-t(i-1))*w(i  )        )/(2.*deltaz)
     2           + (w(i+1)+w(i))*(w(i+1)+w(i))*.25*deltat*
     3                     (t(i+1)-2.*t(i)+t(i-1))/2./deltaz/deltaz
     
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
