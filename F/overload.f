      LOGICAL FUNCTION overload(x, nx, ny, limit, name)
C     Function to decide if any elements of x exceed (in absolute value)
C       the limit that is passed in.
C     Robert Grumbine 10/17/1996

      IMPLICIT none
      REAL limit
      INTEGER nx, ny
      REAL x(nx, ny)

      INTEGER i, j
      LOGICAL name, temp

CD      PRINT *,'Entering overload'
      temp = .FALSE.
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (ABS(x(i,j)) .GT. limit) THEN
          temp = .TRUE.
          IF (name) THEN
            PRINT *,i,j,x(i,j)
          ENDIF
          x(i,j) = SIGN(limit, x(i,j) )
        ENDIF
 1000 CONTINUE

      overload = temp

CD      PRINT *,'Leaving overload'
      RETURN
      END
