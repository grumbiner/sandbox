C*************************************************----------++++++++++!!
      SUBROUTINE oestart(phos, phost, kappa, w, 
     1                   lenrun, outoft, deltaz, deltat, ngas)
      
      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)
 
      DOUBLE PRECISION phos(nlayer), phost(nlayer)
      DOUBLE PRECISION kappa(nlayer), w(nlayer)
      INTEGER lenrun, outoft, peryr, ngas
      DOUBLE PRECISION deltaz, deltat
      
      DOUBLE PRECISION wm, kappam, dt1, dt2
      DOUBLE PRECISION var1, var2, var3, var4
      CHARACTER*60 fname, ques, merg
      INTEGER i, l, m, step

      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)
      
C     Reading in ngas variable
      PRINT *, 'Input gas variable'
      READ (*,9003) ngas
      
C     Initialize the phosphate concentrations.
C       Concentrations are in Moles per cubic meter.
C     Attempting to read in files

      PRINT *, 'Are you reading from a file (y or n)'
      READ *, ques
      
      IF (ques .EQ. 'n') THEN
           DO 100 i = 1, nlayer
CD             phos(i)   = DBLE(COS(ACOS(-1.)*
CD   1                (FLOAT(i)-1.5)/(FLOAT(nlayer)-2.))  )
CD         IF (ABS(i-nlayer/2) .LE. 5) THEN
CD            phos(i) = 1.0
CD          ELSE
CD            phos(i) = 0.0
CD         ENDIF
              phos(i)  = 0.0
              phost(i) = phos(i)
  100      CONTINUE
       ELSE
         PRINT *, 'What file do you want to read from'
         READ (*,9001) merg
         PRINT *, 'What step do you want to start at'
         READ (*,9003) step
C        
         OPEN (UNIT=12, FILE=merg, FORM='FORMATTED', STATUS='OLD')
C     
C        Reading in from file here
         DO 150 l = 1,step
            DO 175 m = 1,nlayer
               READ (12, 9010) phos(m)
  175       CONTINUE
  150    CONTINUE
	    DO 180 m = 1, nlayer
	      WRITE (*,9004) phos(m)
  180   CONTINUE
  
      ENDIF
  
C     Initialize the diffusivity (meters squared per second)
C     Reading in input here
      PRINT *, 'Input upper layer diffusivity (var1)'
      READ (*,9004) var1
      DO 200 i = 1, 15
        kappa(i) = var1
  200 CONTINUE
  
C     Reading in input here
      PRINT *, 'Input lower layer diffusivity (var2)'
      READ (*,9004) var2
      DO 210 i = 16, nlayer
        kappa(i) = var2
  210 CONTINUE
  
C     Initialize the vertical upwelling (meters per second)
C     Reading in input here
      PRINT *, 'Input upper layer upwelling (var3)'
      READ (*,9004) var3
      DO 300 i = 1, 20
        w(i) = var3
  300 CONTINUE
C     Reading in input here
      PRINT *, 'Input lower layer upwelling (var4)'
      READ (*,9004) var4
      DO 310 i = 21, nlayer
        w(i) = var4/secpyr
  310 CONTINUE
 
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
 9010 FORMAT (10X, E13.6)
 
      RETURN
      END
