      SUBROUTINE summer(g, nnx, nny, value)
C     Perform a 2-d integration
      INCLUDE "grid.inc"
      REAL nnx, nny
      REAL g(nx, ny)
      DOUBLE PRECISION value
      REAL tempor(ny)

      INTEGER i, j

      value = 0.

      DO 100 j = 2, ny-1
        value = value + g(1,j) + g(nx,j)
 100  CONTINUE
      DO 110 i = 2, nx-1
        value = value + g(i,1) + g(i,ny)
 110  CONTINUE
      value = value/2.
      value = value + (g(1,1)+g(1,ny)+g(nx,1)+g(nx,ny))/4.

      DO 1000 j = 2, ny-1
        tempor(j) = 0.0
        DO 1100 i = 2, nx-1
          tempor(j) = tempor(j)+g(i,j)
 1100   CONTINUE
        value = value+tempor(j)
 1000 CONTINUE

      RETURN
      ENTRY integ22(g, nnx, nny, value)
C     Perform a 2-d integration of sqaured field

      value = 0.

      DO 200 j = 2, ny-1
        value = value + g(1,j)*g(1,j) + g(nx,j)*g(nx,j)
 200  CONTINUE
      DO 210 i = 2, nx-1
        value = value + g(i,1)*g(i,1) + g(i,ny)*g(i,ny)
 210  CONTINUE
      value = value/2.
      value = value + (g(1,1)*g(1,1)+g(1,ny)*g(1,ny)
     1       +g(nx,1)*g(nx,1)+g(nx,ny)*g(nx,ny) )/4.

      DO 2000 j = 2, ny-1
        DO 2100 i = 2, nx-1
          value = value+g(i,j)*g(i,j)
 2100   CONTINUE
 2000 CONTINUE

      RETURN
      END
