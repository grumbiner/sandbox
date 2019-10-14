      SUBROUTINE randar(a, nx, ny, range)
C     Robert Grumbine 15 Dec 1994

      IMPLICIT none

      INTEGER nx, ny
      REAL a(nx, ny), range

      DOUBLE PRECISION drand48
      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          a(i,j) = SNGL(drand48())*range*2. - range
          PRINT *,a(i,j)
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
