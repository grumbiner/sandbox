C***********************************************************----------!!
      FUNCTION integ2s(g, nx, ny, dx, dy)
C     Perform a 2-d integration using Simpson's 1/3 rule.
C     Robert Grumbine 7 April 1994.

      IMPLICIT none

      INTEGER nx, ny
      REAL g(nx, ny), dx, dy, integ2s, integ22s
      DOUBLE PRECISION tempor(4000), summer

      INTEGER i, j

      DO 1000 j = 1, ny
        tempor(j) = 0.0
        DO 1100 i = 2, nx-2, 2
          tempor(j) = tempor(j) + 4.*g(i,j)+2.*g(i+1,j)
 1100   CONTINUE
        tempor(j) = (tempor(j) + g(1,j)+g(nx,j))/3.
 1000 CONTINUE

      summer = 0.
      DO 1200 j = 2, ny-2, 2
        summer = summer + 4.*tempor(j)+2.*tempor(j+1) 
 1200 CONTINUE
      summer = (summer + tempor(1)+tempor(ny) )/3.
      
      integ2s = summer*dx*dy

      RETURN
C***********************************************************----------!!
      ENTRY integ22s(g, nx, ny, dx, dy)
C     Perform a 2-d integration of squared field using Simpson's 1/3 rule.
C     Robert Grumbine 7 April 1994.

      DO 2000 j = 1, ny
        tempor(j) = 0.0
        DO 2100 i = 2, nx-2, 2
          tempor(j) = tempor(j) + 4.*g(i,j)*g(i,j)
     1                           +2.*g(i+1,j)*g(i+1,j)
 2100   CONTINUE
        tempor(j) = (tempor(j) + g(1,j)*g(1,j)+g(nx,j)*g(nx,j))/3.
 2000 CONTINUE

      summer = 0.
      DO 2200 j = 2, ny-2, 2
        summer = summer + 4.*tempor(j)+2.*tempor(j+1) 
 2200 CONTINUE
      summer = (summer + tempor(1)+tempor(ny) )/3.
      
      integ22s = summer*dx*dy

      RETURN
      END
