      PROGRAM diff
C     Difference two gisst ice maps
      INTEGER nx, ny
      PARAMETER (nx = 360)
      PARAMETER (ny = 180)
      
      REAL new(nx, ny), old(nx, ny)
      REAL del(nx, ny)
      INTEGER i, j

      OPEN (10, FILE="new.7206", FORM="UNFORMATTED", STATUS="OLD")
      OPEN (11, FILE="old.7206", FORM="UNFORMATTED", STATUS="OLD")
      OPEN (12, FILE="dif.7206", FORM="UNFORMATTED", STATUS="OLD")
      READ (10) new
      READ (11) old

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          del(i,j) = new(i,j) - old(i,j)
          IF (del(i,j) .NE. 0.) THEN
            PRINT *,i,j,del(i,j)
          ENDIF
 1100   CONTINUE
 1000 CONTINUE

      STOP
      END
