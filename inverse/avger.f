      SUBROUTINE avger(x, nday, nx, ny, avg)
C     Given a number (nday) of maps which are nx by ny, compute
C       a map which is the average of the input map.
C     Does not allow for the fact that some values may be 'bad'.
C     Bob Grumbine 1/22/94.
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
          IF (avg(i,j) .GT. 255. .OR. avg(i,j) .LT. 0.) THEN
            avg(i,j) = 168.
          ENDIF
 1100   CONTINUE
 1000 CONTINUE

      RETURN
      END
