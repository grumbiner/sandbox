      LOGICAL FUNCTION range(x, nx, ny, lower, upper)
      INTEGER nx, ny
      REAL lower, upper, x(nx, ny)
      LOGICAL tmp, nan
      INTEGER i, j

      tmp = .FALSE.

      DO j = 1, ny
      DO i = 1, nx
        IF (x(i,j) .LT. lower .OR. x(i,j) .GT. upper 
     1                 .OR. nan(x(i,j)) ) THEN
          PRINT *,"out of range ",i,j,x(i,j)
C          x(i,j) = 0.0
          tmp = .TRUE.
        ENDIF
      ENDDO
      ENDDO

      range = tmp

      RETURN 
      END

      LOGICAL FUNCTION nan(x)
      nan = .NOT. ( (x .GT. 0) .OR. (x .LE. 0) )
      RETURN
      END
