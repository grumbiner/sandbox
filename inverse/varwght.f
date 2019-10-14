      SUBROUTINE varwght(y, r0, a, b, c, r, dt, beta, n, w, type)
C     Bob Grumbine
C     11-23-93 Fill the vectors with their weights for the
C        constrained problem.  Y is input vector, a, b, c
C        are the diagonals, and r is the forcing vector in Ax = r.
C     11-23-93 Extended to work with gappy time series.
C     11-24-93 Added 'type' to handle which type of boundary treatment.
C               1 -> use 0 boundary values.
C               2 -> use natural variational conditions.
C     11-26-93 Work with computing boundary values in a gappy time
C               series.
C     11-30-93 Fix error, e2dt should be beta/dt, not times.
C     12-08-93 Fix error, boundary condition is 1+beta^2, not 2*beta^2
C     Version change to varwght -- version for assimilation with
C       expected growth.
C     a = n-1 time
C     b = n time
C     c = n+1 time
C     y is the observed concentration
C     r0 is the expected concentration growth rate (an external function)
C     beta is the weight. 


      IMPLICIT none 

      INTEGER n, type
      REAL y(n), r(n), a(n), b(n), c(n), w(n)
      REAL t, dt, beta

      REAL e2dt, e2
      INTEGER i
      REAL r0, r0dot
      EXTERNAL r0, r0dot

      e2dt = (beta/dt)**2
      e2 = beta*beta

      IF (type .EQ. 1) THEN
        a(1) = 0.
        b(1) = 1.
        c(1) = 0.
        r(1) = 0.0
        a(n) = 0.
        b(n) = 1.
        c(n) = 0.
        r(n) = 0.0
       ELSE IF (type .EQ. 2) THEN
C       9000 loops handle gaps near ends of series.        
        a(1) = 0.
        b(1) = -1.
        c(1) = 1.        
        DO 9000 i = 2, n
          IF (w(i) .NE. 0.) THEN
            r(1) = (y(i)-y(1))/(1+2*e2)/(i-1)
            r(1) = r(1) - 2*e2*r0(0.)/(1. + 2*e2)
            r(1) = -r0(0.)
            GO TO 9001
          ENDIF
 9000   CONTINUE
 9001   CONTINUE
        a(n) = -1.
        b(n) = 1.
        c(n) = 0.
        DO 9100 i = n-1, 1, -1
          IF (w(i) .NE. 0.) THEN
            r(n) = (y(n)-y(i))/(1+ 2*e2)/(n-i)
            r(n) = r(n) - 2*e2*r0((n-1)*dt)/(1. + 2*e2)
            r(n) = -r0((n-2)*dt)
            GO TO 9101
          ENDIF
 9100   CONTINUE
 9101   CONTINUE
       ELSE IF (type .EQ. 3) THEN
        a(1) = 0.
        a(n) = 0.
        b(1) = 1.
        b(n) = 1.
        c(1) = 0.
        c(n) = 0.
        r(1) = y(1)
        r(n) = y(n) 
       ELSE
        PRINT *,'Type of boundary treatment out of range'
        STOP
      ENDIF

      DO 1000 i = 2, n-1
          a(i) =  e2dt 
          b(i) = -w(i) - 2.*e2dt
          c(i) =  e2dt 

          t = (i-1)*dt
          r(i) = -w(i)*y(i) - e2*r0dot(r0, t, dt)
 
 1000 CONTINUE

      RETURN
      END
