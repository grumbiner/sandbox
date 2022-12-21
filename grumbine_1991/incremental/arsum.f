      SUBROUTINE arsum(x, y, nx, ny)
      INTEGER nx, ny
      REAL x(nx*ny), y(nx*ny)
      INTEGER i
      DO 1000 i = 1, nx*ny
        x(i) = x(i)+y(i)
 1000 CONTINUE
      RETURN
      END
