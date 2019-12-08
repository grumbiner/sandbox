
      SUBROUTINE findw(a0, w, nx, ny, nt)
C     Find the weighting function for a0
      INTEGER nx, ny, i, j, k
      REAL a0(nx,ny, nt), w(nx, ny, nt)
    
      DO 1000 k = 1, nt
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (a0(i,j, k) .GT. 1.28) THEN
          w(i,j, k) = 0.0
        ELSE IF (a0(i,j, k) .GT. 1.00) THEN
CD          a0(i,j, k) = 1.0
          w(i,j, k) = 1.0
        ELSE
          w(i,j, k) = 1.0
        ENDIF
 1000 CONTINUE

      RETURN
      END
