      SUBROUTINE rescale(x, nx, ny, m, b)
C     Do a linear rescaling of the array x, making a new x = mx +b
C     Robert Grumbine 30 April 1998

      INTEGER nx, ny
      REAL x(nx, ny), m, b
      INTEGER i, j
      DO 1000 j = 1, ny
      DO 1100 i = 1, nx
        x(i,j) = m*x(i,j) + b
 1100 CONTINUE
 1000 CONTINUE

      RETURN
      END
