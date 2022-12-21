      SUBROUTINE findw2(a0, w, nx, ny)
C     Find the weighting function for a0
      INTEGER nx, ny, i, j
      REAL a0(nx,ny), w(nx, ny)
    
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (a0(i,j) .GT. 1.28) THEN
          IF (ABS(a0(i,j) - 1.77) .LT. 0.01) THEN
            w(i,j) = 1.0
            a0(i,j) = 0.0
           ELSE
            w(i,j) = 0.0
          ENDIF
        ELSE IF (a0(i,j) .GT. 1.00) THEN
CD          a0(i,j) = 1.0
          w(i,j) = 1.0
        ELSE
          w(i,j) = 1.0
        ENDIF
 1000 CONTINUE

      RETURN
      END
