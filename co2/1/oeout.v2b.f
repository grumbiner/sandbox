C*************************************************----------++++++++++!!
      SUBROUTINE oeout(phos, phost, nlayer, j, outoft, secpyr, deltat)

      IMPLICIT none
      INTEGER nlayer, j, outoft
      DOUBLE PRECISION phos(nlayer), phost(nlayer)
      DOUBLE PRECISION secpyr, deltat
	  
      DOUBLE PRECISION sum1
      INTEGER i

      IF (MOD((j-1),INT(DBLE(outoft)*secpyr/deltat)) .EQ. 0) THEN
        sum1 = 0.D0
        DO 2000 i = 2, nlayer-1
          sum1 = sum1 + phos(i)
 2000   CONTINUE
        WRITE (10, 9002) (i,CHAR(9),phos(i),CHAR(9),sum1,
     1                      i=1, nlayer)
      ENDIF

 9002 FORMAT (I2,A1,E13.6,A1,E13.6)
 
      RETURN
      END
