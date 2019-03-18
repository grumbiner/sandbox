      SUBROUTINE neubc(g, nx, ny)
C     Apply no normal gradient boundary conditions on a field.
C     Bob Grumbine 4 April 1994

      IMPLICIT none

      INTEGER nx, ny
      REAL g(nx, ny)

      INTEGER i, j

C     No normal gradient 
      DO 1000 j = 1, ny
        g(1,j) = g(2,j)
        g(nx,j) = g(nx-1,j)
 1000 CONTINUE
      DO 1100 i = 1, nx
        g(i,1) = g(i,2)
        g(i,ny) = g(i,ny-1)
 1100 CONTINUE

      RETURN
      END
