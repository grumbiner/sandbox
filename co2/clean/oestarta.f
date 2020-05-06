      SUBROUTINE oestart(phos, kappa, w, lenrun)
      
      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)
      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)

      DOUBLE PRECISION phos(nlayer), kappa(nlayer), w(nlayer)
      CHARACTER*60 fname
      INTEGER lenrun, i

C     Initialize the carbon and phosphate concentrations
C       Concentrations are in Moles per cubic meter
CD      phos(1) = 0.D0
CD      phos(2) = 0.D0
      DO 100 i = 1, nlayer
        phos(i)   = DBLE(COS(8.*2.*ACOS(-1.)*
     1                FLOAT(i-1)/FLOAT(nlayer-2))  )
  100 CONTINUE
  
C     Initialize the diffusivity (meters squared per second)
CD      DO 200 i = 1, 15
CD        kappa(i) = 1.6D-4
CD  200 CONTINUE
      DO 210 i = 1, nlayer
        kappa(i) = 0.5D-4
  210 CONTINUE
  
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
CD      lenrun = 100
      
 9001 FORMAT (A60)
 9003 FORMAT (BN, I5)

      RETURN
      END