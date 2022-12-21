C***********************************************************----------!!
C     Version from simp.out.f
      SUBROUTINE integ2(g, nx, ny, dx, dy, value)
C     Perform a 2-d integration
      IMPLICIT none
      INTEGER nx, ny
      REAL g(nx, ny), dx, dy, value
      DOUBLE PRECISION tempor(4000), summer

      INTEGER i, j

      DO 1000 j = 1, ny
        tempor(j) = 0.0
        DO 1100 i = 2, nx-1, 2
          tempor(j) = tempor(j) + 4.*g(i,j)+2.*g(i+1,j)
 1100   CONTINUE
        tempor(j) = (tempor(j) + g(1,j)+g(nx,j))/3.
 1000 CONTINUE

      summer = 0.
      DO 1200 j = 2, ny-1, 2
        summer = summer + 4.*tempor(j)+2.*tempor(j+1)
 1200 CONTINUE
      summer = (summer + tempor(1)+tempor(ny) )/3.

      value = summer*dx*dy

      RETURN
C***********************************************************----------!!
      ENTRY integ22(g, nx, ny, dx, dy, value)
C     Perform a 2-d integration of sqaured field

      DO 2000 j = 1, ny
        tempor(j) = 0.0
        DO 2100 i = 2, nx-1, 2
          tempor(j) = tempor(j) + 4.*g(i,j)*g(i,j)
     1                           +2.*g(i+1,j)*g(i+1,j)
 2100   CONTINUE
        tempor(j) = (tempor(j) + g(1,j)*g(1,j)+g(nx,j)*g(nx,j))/3.
 2000 CONTINUE

      summer = 0.
      DO 2200 j = 2, ny-1, 2
        summer = summer + 4.*tempor(j)+2.*tempor(j+1)
 2200 CONTINUE
      summer = (summer + tempor(1)+tempor(ny) )/3.

      value = summer*dx*dy
      RETURN
      END
