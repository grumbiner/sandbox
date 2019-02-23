      PROGRAM a
      IMPLICIT none

      INTEGER nx, ny
      PARAMETER (nx = 720)
      PARAMETER (ny = 360)
      REAL temp(nx, ny), delta(nx, ny)
      INTEGER i,j

      OPEN(10, FILE="cressout", FORM="UNFORMATTED",STATUS="OLD")
      READ(10) temp
      READ (10) delta

      PRINT *,maxval(delta), minval(delta)
      DO i = 1, nx
      DO j = 1, ny
        IF (ABS(delta(i,j)) .GT. 1.0) THEN
          PRINT *,i,j,delta(i,j)
        ENDIF
      ENDDO
      ENDDO

      STOP
      END
