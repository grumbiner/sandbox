      SUBROUTINE vdiff (x, y, ndat, z)
C     compute the difference between two vectors, x-y = z
C     Robert Grumbine 15 Dec 1994

      IMPLICIT none

      INTEGER ndat
      REAL x(ndat), y(ndat), z(ndat)
      INTEGER i

      DO 1000 i = 1, ndat
        z(i) = x(i)-y(i)
 1000 CONTINUE

      RETURN
      END 
