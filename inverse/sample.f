      SUBROUTINE sample(w, n, gaps, seed)
C     Simulate random gaps in data by setting w to
C       zero (weight in variational scheme, just a vector
C       otherwise) at randomly determined points.
C     Bob Grumbine 1/22/94.

      IMPLICIT none

      INTEGER n
      REAL w(n), gaps, RAN2
      INTEGER i, seed

      DO 1000 i = 1, n
        IF (RAN2(seed) .GT. gaps) THEN
          w(i) = 1.
         ELSE
          w(i) = 0.
        ENDIF
 1000 CONTINUE
      
      RETURN
      END
