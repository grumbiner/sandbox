C***********************************************************----------!!
C     Version from integ.out.f
      SUBROUTINE integ2(g, nx, ny, dx, dy, value)
C     Perform a 2-d integration
      IMPLICIT none
      INTEGER nx, ny
      REAL g(nx, ny), dx, dy, value
      DOUBLE PRECISION tempor(4000), summer

      INTEGER i, j

      DO 1000 j = 1, ny
        tempor(j) = 0.0
        DO 1100 i = 2, nx-1
          tempor(j) = tempor(j) + g(i,j)
 1100   CONTINUE
        tempor(j) = tempor(j) + (g(1,j)+g(nx,j))/2.
 1000 CONTINUE

      summer = 0.
      DO 1200 j = 2, ny-1
        summer = summer + tempor(j)
 1200 CONTINUE
      summer = summer + (tempor(1)+tempor(ny))/2.

      value = summer*dx*dy

      RETURN
C***********************************************************----------!!
      ENTRY integ22(g, nx, ny, dx, dy, value)
C     Perform a 2-d integration of sqaured field

      DO 2000 j = 1, ny
        tempor(j) = 0.0
        DO 2100 i = 2, nx-1
          tempor(j) = tempor(j) + g(i,j)*g(i,j)
 2100   CONTINUE
        tempor(j) = tempor(j) + (g(1,j)*g(1,j)+g(nx,j)*g(nx,j))/2.
 2000 CONTINUE

      summer = 0.
      DO 2200 j = 2, ny-1
        summer = summer + tempor(j)
 2200 CONTINUE
      summer = summer + (tempor(1)+tempor(ny))/2.

      value = summer*dx*dy

      RETURN
      END
