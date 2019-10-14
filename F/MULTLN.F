      PROGRAM multln
C     Compute a multivariate linear regression

      INTEGER nexp, nvar
      PARAMETER (nexp = 10)
      PARAMETER (nvar = 10)
      REAL x(nexp, nvar), y(nexp), yfit(nexp), a(nvar)

      INTEGER rnexp, rnvar      
      CHARACTER*60 fname, xform
      INTEGER i, j
      REAL avy, avfit, sdy, sdfit, cor      
      
      PRINT *,'How many experiments have been run?'
      READ (*,9001) rnexp
      IF (rnexp .GT. nexp) STOP 'Too many experiments for this program'
      PRINT *,'How many variables are there?'
      READ (*,9001) rnvar
      IF (rnvar .GT. nvar) STOP 'Too many variables for the program'
      IF (rnvar .GT. rnexp) STOP 'Must use SVD, underdetermined '
 
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
        PRINT *, x
 1000 CONTINUE
      WRITE (*, xform) ((x(i,j) , j = 1, rnvar), i = i, rnexp)

C     Now have the data, call the subroutine to compute the coeffs:
      PRINT *,'Calling mulreg'
      CALL mulreg(x, y, a, rnexp, rnvar)
      PRINT *,'Leaving mulreg'
      WRITE (*,9004) a
 
C     Compute the degree of fit between the 'true' and fitted lines:
      PRINT *,'Computing fit'
      DO 2000 i = 1, rnexp
        yfit(i) = 0.0
        DO 2010 j = 1, rnvar
          yfit(i) = yfit(i) + x(i,j)*a(j)
 2010   CONTINUE
 2000 CONTINUE
      CALL correl(y, yfit, rnexp, sdy, sdfit, cor, avy, avfit)
      PRINT *,'r**2 = ', cor**2

 9001 FORMAT (I5)

 9002 FORMAT (A60)

 9003 FORMAT (3F5.1)

 9004 FORMAT ('Coefficients : ',/,10(E13.6,/) )
 
      END
