      SUBROUTINE arget(y, x, point, L, M, N)
C     Pull a particular array out of an N-array of 2-d arrays.
C     Can be quite memory intensive.  Bounds are 0-L, 0-M rather
C       than 1-, because of the convention used in the sea ice
C       model.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER L, M, N
      REAL x(0:L,0:M,N), y(0:L,0:M)
      INTEGER point
      INTEGER i, j
      
      DO 1000 j = 0, M
        DO 1100 i = 0, L
          y(i,j) = x(i,j,point)
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
