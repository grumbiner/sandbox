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
      PARAMETER (deltat = secpyr/3.D0)
      
      DOUBLE PRECISION phos(nlayer), phost(nlayer)
      DOUBLE PRECISION w(nlayer), kappa(nlayer)
      
      INTEGER i, j, lenrun
      DOUBLE PRECISION sum1

C     Set up parameters and files      
      CALL oestart(phos, kappa, w, lenrun)
	  PRINT *,'returned from oestart'
 
C     Now ready to compute the evolution of the carbon and phosphate
C       distributions.  Equilibration time is something of a mystery.
C       Start out with a 50 year run

      PRINT *,'time at start', LONG(362)
      DO 1000 j = 1, lenrun*INT(secpyr/deltat)

CD        PRINT *,'Calling advdif'
        CALL advdif(deltaz, deltat, phos, kappa, w)
		CALL theory(phost, kappa, w, deltaz, j, deltat)
       
        IF (MOD((j-1),50*INT(secpyr/deltat)) .EQ. 0) THEN
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