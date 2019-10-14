      PROGRAM multln
C     Compute correlations between multiple parameters and some resultant 
C     Robert Grumbine 27 September 1994

      IMPLICIT none

      INTEGER nexp, nvar
      PARAMETER (nexp = 20)
      PARAMETER (nvar = 20)
      REAL x(nexp, nvar), y(nexp), xvar(nexp), yfit(nexp), r(nvar)

      INTEGER rnexp, rnvar      
      CHARACTER*60 fname, xform
      INTEGER i, j
      REAL avy, avfit, sdy, sdfit, cor      
      REAL avg, bnot, bwon, s
      LOGICAL yes, subtr

      s(cor) = (rnexp-3)**.5/2.*ALOG((1.+cor)/(1.-cor)) 

      PRINT *,'How many experiments have been run?'
      READ (*,9001) rnexp
      IF (rnexp .GT. nexp) STOP 'Too many experiments for this program'
      PRINT *,'How many variables are there?'
      READ (*,9001) rnvar
      IF (rnvar .GT. nvar) STOP 'Too many variables for the program'
 
      PRINT *,'Result file:'
      PRINT *,' '
      CALL readin(y, rnexp, 10)
      PRINT *,'What is the file name of the parameters?'
      READ (*,9002) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      PRINT *,'What is the format of the file?'
      READ (*,9002) xform
      DO 1000 i = 1, rnexp
        READ (12, xform) (x(i,j), j = 1, rnvar)
C       PRINT *, x
 1000 CONTINUE
      WRITE (*, xform) ((x(i,j), j = 1, rnvar), i = i, rnexp)

 9999 CONTINUE
C     Compute the corelation between parameter i and resultant
C     PRINT *,'Computing correlation'
      PRINT *,' i     bnot     bwon      r       s'
      DO 2000 i = 1, rnvar
        DO 2010 j = 1, rnexp
          xvar(j) = x(j,i)
 2010   CONTINUE
        IF (i .NE. rnvar) 
     1     CALL detrnd(xvar, y, rnexp, bnot, bwon, avg, .FALSE.)
        CALL correl(y, xvar, rnexp, sdy, sdfit, r(i), avy, avfit)
        IF (ABS(r(i)) .NE. 1.0) THEN
          WRITE (*,9004) i, bnot, bwon, r(i), s(r(i))
         ELSE
          WRITE (*,9004) i, bnot, bwon, r(i), 0.0 
        ENDIF
 2000 CONTINUE

        PRINT *,'Would you like a linear fit for one of these varables?'
        IF (.NOT. yes(.FALSE.)) GO TO 9998
        PRINT *,'Which variable would you like the fit for?'
        READ (*,9001) i
        DO 3000 j = 1, rnexp
          xvar(j) = x(j,i)
 3000   CONTINUE
        PRINT *,'Would you like to examine the residual?'
        subtr = yes(.FALSE.)
        CALL detrnd(xvar, y, rnexp, bnot, bwon, avg, .FALSE.)
        DO 3010 j = 1, rnexp
          yfit(j) = bnot + bwon*xvar(j)
 3010   CONTINUE
        CALL correl(y, yfit, rnexp, sdy, sdfit, r(i), avy, avfit)
        WRITE (*,9004) i, bnot, bwon, r(i), s(r(i))
        CALL detrnd(xvar, y, rnexp, bnot, bwon, avg, subtr)
      IF (subtr) GO TO 9999

 9998 CONTINUE
 
 9001 FORMAT (I5)

 9002 FORMAT (A60)

 9003 FORMAT (2F6.1,2F7.4,F4.1)

 9004 FORMAT (I3,2F9.4,2F8.3)
  
      END
