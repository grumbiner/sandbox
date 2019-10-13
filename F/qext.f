C*************************************************----------++++++++++!!
      SUBROUTINE QEXT( xmax, ymax, q, tstep, deltat )

C     The purpose of this routine is to determine the rate of ice
C       formation over the domain of the model.  This is then used
C       as an input of salinity to the water, due to fractionation
C       in the freezing process.
C     Robert Grumbine 15 Feb 1995 

      IMPLICIT none

      INTEGER xmax, ymax
  
      REAL q(0:xmax+1, 0:ymax)


C     Local variables
      INTEGER loy, tstep, endwin, sumst, sumend
      INTEGER tofyr, i, j, yshelf
      REAL deltat, qmax

      SAVE

C     Begin the calculation.
      tofyr = MOD( tstep, loy)

      IF (tofyr .LE. endwin) THEN

C       It is the winter time.
        DO 1000 j = 0, ymax
          DO 1010 i = 0, xmax + 1 
            q(i, j) = qmax*(-FLOAT(j)/FLOAT(yshelf) + 1.0 )**2
            IF ( j .GT. yshelf ) q(i,j) = 0.0
 1010     CONTINUE
 1000   CONTINUE

       ELSE
C       It is not winter
        DO 1100 j = 0, ymax
          DO 1110 i = 0, xmax + 1
            q(i, j) = 0.0
 1110     CONTINUE
 1100   CONTINUE

      ENDIF 

      RETURN

      ENTRY QSET( xmax, ymax, q, tstep, deltat )
C     Entry pt. for initialization of routine.
      
      READ (4,9001) endwin
      READ (4,9001) sumst
      READ (4,9001) sumend
      READ (4,9001) yshelf
      READ (4,9002) qmax

      WRITE (*,9001) endwin
      WRITE (*,9001) sumst
      WRITE (*,9001) sumend
      WRITE (*,9001) yshelf
      WRITE (*,9002) qmax

      loy = INT( 1./ deltat )

 9001 FORMAT (I5)

 9002 FORMAT (F10.5)
 
      RETURN
      END
