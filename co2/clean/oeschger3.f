C*************************************************----------++++++++++!!
      PROGRAM oeschger
C     Attempt to simulate the results of the Oeschger model
C        as presented in Broecker and Peng.
      IMPLICIT none
      
      DOUBLE PRECISION secpyr
      PARAMETER (secpyr = 8.64D4*365.2422)
      INTEGER nlayer
      PARAMETER (nlayer = 76)
      DOUBLE PRECISION deltaz, deltat 
      DOUBLE PRECISION q
 
      DOUBLE PRECISION phos(nlayer), phost(nlayer)
      DOUBLE PRECISION w(nlayer), kappa(nlayer)
     
      INTEGER ngas
      INTEGER lenrun, outoft
      
      INTEGER i, j
 
C     Set up parameters and files
      CALL oestart(phos, phost, kappa, w, 
     1                   lenrun, outoft, deltaz, deltat, ngas)
 
CD    ngas = 4
 
C     Now ready to compute the evolution of the carbon and phosphate
C       distributions.
      PRINT *,'time at start', LONG(362)
 
      DO 1000 j = 1, lenrun*INT(secpyr/deltat)
 
        CALL advdif(deltaz, deltat, phos, kappa, w, ngas, j)
        CALL oeout(phos, phost,  nlayer, j, outoft, secpyr, deltat)
 
 1000 CONTINUE
 
      PRINT *,'time at end', LONG(362)
      PAUSE
      CLOSE(10, STATUS='KEEP')
      
 9002 FORMAT (I2,A1,E13.6,A1,E13.6)
      
      END
