      SUBROUTINE arset(x, nx, ny, val)
C     Robert Grumbine 27 March 1996.
      INTEGER nx, ny
      REAL x(nx, ny), val
      INTEGER i, j

      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        x(i,j) = val
 1000 CONTINUE

      RETURN
      END
