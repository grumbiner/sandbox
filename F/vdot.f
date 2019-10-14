      FUNCTION vdot(x, y, npts)
C     Compute the inner product of two vectors.  Use double precision
C       summation, though single precision output.
C     Robert Grumbine 5 April 1994.

      IMPLICIT none

      INTEGER npts
      REAL vdot, x(npts), y(npts)
 
      INTEGER i
      DOUBLE PRECISION sum

      sum = 0.D0
      DO 1000 i = 1, npts
        sum = sum + DBLE(x(i)*y(i))
 1000 CONTINUE

      vdot = SNGL(sum)

      RETURN
      END
