C***********************************************************----------!!
      SUBROUTINE arset1(x, val, nx, ny)
      INTEGER nx, ny
      REAL x(nx*ny), val
      INTEGER i

      DO 1000 i = 1, nx*ny
        x(i) = val
 1000 CONTINUE

      RETURN
      END
