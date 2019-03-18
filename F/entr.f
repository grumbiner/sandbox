      SUBROUTINE entr(x, nx, ny, y)
C     Enter the array y into x

      INTEGER nx, ny
      REAL x(nx, ny), y(nx, ny)

      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          x(i,j) = y(i,j)
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
