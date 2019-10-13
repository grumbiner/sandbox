C***********************************************************----------!!
      SUBROUTINE summer(a, nx, ny, sum)
C     SUM AN ARRAY
C     Robert Grumbine 15 February 1995

      INTEGER nx, ny
      REAL a(nx, ny)
      DOUBLE PRECISION sum

      INTEGER i, j

      sum = 0.D0
      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          sum = sum+DBLE(a(i,j))
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
