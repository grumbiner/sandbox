      SUBROUTINE ssmiavg(x, nday, nx, ny, avg)
C     Compute an average of nday raster maps (as for NSIDC archive)
C       which are nx by ny each.
C     Bob Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER nx, ny, nday
      CHARACTER*1 x(nday, nx, ny)
      REAL avg(nx, ny)

      INTEGER i, j, k

      DO 100 j = 1, ny
        DO 110 i = 1, nx
          avg(i,j) = 0.0
  110   CONTINUE
  100 CONTINUE

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          DO 1200 k = 1, nday
            avg(i,j) = avg(i,j) + FLOAT(ICHAR(x(k, i, j)))
 1200     CONTINUE
          avg(i,j) = avg(i,j)/FLOAT(nday)
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
