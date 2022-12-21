      SUBROUTINE findw3(a0, w, nx, ny, nt)
C     Find the weighting function for a0
C     Robert Grumbine
C     Last Modified 28 May 1996

      IMPLICIT none

      INTEGER nx, ny, nt, i, j, k
      REAL a0(nx,ny, nt), w(nx, ny, nt)
    
      DO k = 1, nt
      DO j = 1, ny
      DO i = 1, nx
        IF (a0(i,j, k) .GT. 1.28) THEN
          w(i,j, k) = 0.0
        ELSE IF (a0(i,j, k) .GT. 1.00) THEN
CD          a0(i,j, k) = 1.0
          w(i,j, k) = 1.0
        ELSE
          w(i,j, k) = 1.0
        ENDIF
      ENDDO
      ENDDO
      ENDDO

      RETURN
      END
