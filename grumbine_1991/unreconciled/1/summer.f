      SUBROUTINE summer(g, nnx, nny, value)
C     Perform a 2-d integration
      INCLUDE "grid.inc"
      REAL nnx, nny
      REAL g(nx, ny)
      DOUBLE PRECISION value
      REAL tempor(ny)

      INTEGER i, j

      value = 0.

      DO 1000 j = 2, ny-1
        tempor(j) = 0.0
        DO 1100 i = 2, nx-1
          tempor(j) = tempor(j)+g(i,j)
 1100   CONTINUE
        value = value+tempor(j)
 1000 CONTINUE

      RETURN
      END
