      PROGRAM elastic
C     Compute the magnitudes of d/dx, d2/dx2, d3/dx3 for use on 
C       a problem where fourth order solutions are needed and the
C       boundary condition is not obvious.

      INTEGER nx, ny
      PARAMETER (nx = 36)
      PARAMETER (ny = 36)

      REAL u(nx, ny)
      CHARACTER*60 fname
      INTEGER i, j, tstep
      REAL delx
      DOUBLE PRECISION sum1, sum2, sum3

      PRINT *,'What is the name of the data file?'
      READ (*,9001) fname
      OPEN (1, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      PRINT *,'What is the grid spacing?'
      READ (*,9002) delx
      PRINT *,'What time step do you want the data from?'
      READ (*,9003) tstep

      DO 100 i = 1, tstep
	READ (1) u
  100 CONTINUE

      sum1 = 0.D0
      sum2 = 0.D0
      sum3 = 0.D0

      DO 1000 j = 1, ny

	sum1 = ABS(u(2,j)-u(1,j)) + sum1
        sum2 = ABS(u(3,j)-2.*u(2,j)+u(1,j)) + sum2
	sum3 = ABS(3.*u(4,j)-3.*u(3,j)+u(2,j)-u(1,j))/3.
     1          + sum3

 1000 CONTINUE

 9001 FORMAT (A60)

 9002 FORMAT (E13.6)

 9003 FORMAT (I3)

      PRINT *, sum1, sum2, sum3

      END
