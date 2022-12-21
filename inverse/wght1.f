
      SUBROUTINE wght1(y, a, b, c, r, dt, epsilon, n, w, type)
C     11-23-93 Fill the vectors with their weights for the
C        constrained problem.  Y is input vector, a, b, c
C        are the diagonals, and r is the forcing vector in Ax = r.
C     11-23-93 Extended to work with gappy time series.
C     11-24-93 Added 'type' to handle which type of boundary treatment.
C               1 -> use 0 boundary values.
C               2 -> use natural variational conditions.

      INTEGER n, type
      REAL y(n), r(n), a(n), b(n), c(n), w(n)
      REAL dt, epsilon

      REAL e2dt
      INTEGER i

      e2dt = (epsilon*dt)**2
      IF (type .EQ. 1) THEN
        a(1) = 0.
        b(1) = 1.
        c(1) = 0.
        r(1) = 0.95
        a(n) = 0.
        b(n) = 1.
        c(n) = 0.
        r(n) = 0.95
       ELSE IF (type .EQ. 2) THEN
C       Neglecting gaps near ends of series.        
        a(1) = 0.
        b(1) = -1.
        c(1) = 1.        
        r(1) = (y(2)-y(1))/(1+2*epsilon*epsilon)
        a(n) = -1.
        b(n) = 1.
        c(n) = 0.
        r(n) = (y(n)-y(n-1))/(1+2*epsilon*epsilon) 
       ELSE
        PRINT *,'Type of boundary treatment out of range'
        STOP
      ENDIF

      DO 1000 i = 2, n-1
        IF (w(i) .NE. 0.) THEN  
          a(i) = e2dt
          b(i) = -1. -2.*e2dt
          c(i) = e2dt
          r(i) = -y(i)
         ELSE
          a(i) = e2dt
          b(i) = -2.*e2dt
          c(i) = e2dt
          r(i) = 0
        ENDIF
 1000 CONTINUE

      RETURN
      END
