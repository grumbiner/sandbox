C***********************************************************----------!!
      PROGRAM densit

C     Program to generate density values for various types of accrection tori.

      REAL G, M, Re
      PARAMETER ( Re= 1.000 )

      INTEGER xmax, ymax
      PARAMETER ( xmax = 40 )
      PARAMETER ( ymax = 40 )

      REAL rho(xmax, ymax)
      REAL r, z, alpha, l, beta
      REAL phi

      INTEGER i, j

C     Get parameters
      PRINT *, 'What is alpha?'
      READ (5,9001) alpha

      PRINT *, 'What is l in the velocity relation?'
      READ (5,9001) l

      PRINT *, 'What is beta?'
      READ (5,9001) beta

      DO 100 i = 1, xmax  
        DO 200 j = 1, ymax
C***********************************************************----------!!
          r   = 2.0*Re + Re* FLOAT(i)/5.
          z   = Re* FLOAT(j-1)/5.
          phi = Re/ (( r**2. + z**2. )**.5 - Re)
          IF ( l .NE. 0.0) THEN
            rho(i, j)= (phi + beta 
     1                 + (1./(2.*l))*alpha*alpha*r**(2.*l) )
           ELSE
            rho(i, j) = phi +
     1                  alpha*alpha*LOG(r/beta) 
          ENDIF
          IF ((i .NE. 1) .OR. (j .NE. 1)) 
     1      rho(i, j) = rho(i, j)/rho(1, 1)
  200   CONTINUE
  100 CONTINUE
      rho(1,1) = 1.0

      OPEN (10,FILE='RHO1.O',FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) rho
      CLOSE (10, STATUS='KEEP')

 9001 FORMAT (F14.4)

      END

C***********************************************************----------!!
