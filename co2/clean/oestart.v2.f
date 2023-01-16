C*************************************************----------++++++++++!!
      SUBROUTINE oestart(phos, phost, kappa, w, 
     1                   lenrun, outoft, deltaz, deltat)
      
      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)

      DOUBLE PRECISION phos(nlayer), phost(nlayer)
      DOUBLE PRECISION kappa(nlayer), w(nlayer)
      INTEGER lenrun, outoft, peryr
      DOUBLE PRECISION deltaz, deltat
      
      DOUBLE PRECISION wm, kappam, dt1, dt2
      CHARACTER*60 fname
      INTEGER i
      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)
      
C     Initialize the phosphate concentrations.
C       Concentrations are in Moles per cubic meter.
      DO 100 i = 1, nlayer
CD        phos(i)   = DBLE(COS(ACOS(-1.)*
CD     1                (FLOAT(i)-1.5)/(FLOAT(nlayer)-2.))  )
        IF (ABS(i-nlayer/2) .LE. 5) THEN
          phos(i) = 1.0
         ELSE
          phos(i) = 0.0
        ENDIF
        phost(i) = phos(i)
  100 CONTINUE
  
C     Initialize the diffusivity (meters squared per second)
      DO 200 i = 1, nlayer
        kappa(i) = 0.5D-4
  200 CONTINUE
CD      DO 210 i = 16, nlayer
CD        kappa(i) = 0.5D-4
CD  210 CONTINUE
  
C     Initialize the vertical upwelling (meters per second)
      DO 300 i = 1, 20
        w(i) = 0.D0
  300 CONTINUE
      DO 310 i = 1, nlayer
        w(i) = -5.D0/secpyr
  310 CONTINUE
C     The following is required to have advective scalar concentration
CD      w(1)        = 0.0D0
CD      w(2)        = 0.0D0
CD      w(nlayer)   = 0.0D0
CD      w(nlayer-1) = 0.0D0

C     Open the output file.
      PRINT *,'What is the output file name?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      
C     Determine the length of the run:
      PRINT *,'How long a run (approx 8 years per second)'
      READ (*,9003) lenrun
      PRINT *,'How often (years) do you want the output?'
      READ (*,9003) outoft
      PRINT *,'What is deltaz?'
      READ (*,9004) deltaz
      PRINT *,'How many steps per year do you want (integer)?'
      READ (*,9003) peryr
      
      deltat = secpyr/DBLE(peryr)
      
C     Check deltat for numerical stability.
      wm = 0.D0
      kappam = 0.D0
      DO 400 i = 1, nlayer
        wm     = DMAX1(wm, DABS(w(i)))
        kappam = DMAX1(kappam, DABS(kappa(i)))
  400 CONTINUE
      dt1 = deltaz/wm
      dt2 = deltaz*deltaz/4.D0/kappam
      IF (deltat .GT. dt1 .OR. deltat .GT. dt2) THEN
        PRINT *,'Deltat chosen is numerically unstable'
        IF (dt1 .LT. dt2) THEN
          PRINT *,'The constraint is imposed by the CFL condition',dt1
         ELSE
          PRINT *,'The constraint is imposed by the diffusion',dt2
        ENDIF
        deltat = secpyr/INT(1+secpyr/DMAX1(dt1,dt2))
        PRINT *,'Number of steps per year is now',
     1           INT(1+secpyr/DMIN1(dt1,dt2))
      ENDIF
	  
 9001 FORMAT (A60)
 9003 FORMAT (BN, I5)
 9004 FORMAT (E13.6)

      RETURN
      END