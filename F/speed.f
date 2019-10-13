      SUBROUTINE speed(s, u, v, npts)
C     Compute the Pythagorean speed given a vector of velocity components.
C     Robert Grumbine 5 April 1994.

      IMPLICIT none

      INTEGER k, npts
      REAL s(npts), u(npts), v(npts)

      DO 1000 k = 1, npts
        s(k) = u(k)*u(k)+v(k)*v(k)
 1000 CONTINUE
      DO 1100 k = 1, npts
        s(k) = SQRT(s(k))
 1100 CONTINUE

      RETURN
      END
