      SUBROUTINE arsum2(x, y, nx, ny)
      INTEGER nx, ny
      REAL x(nx,ny), y(nx,ny)
      INTEGER i, j
      DO 1100 j = 1, ny
      DO 1000 i = 1, nx
        x(i,j) = x(i,j) + y(i,j)
 1000 CONTINUE
 1100 CONTINUE

      RETURN
      END
