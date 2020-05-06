      SUBROUTINE autocor(x, y, n, corr, nlag)
C     Compute autocorrelations between two vectors.  Truncate
C       rather than wrap around.
C     Bob Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER n, nlag
      REAL x(n), y(n), corr(nlag)
      INTEGER lag
      REAL r2, mux, muy, sigx, sigy
      
      DO 1000 lag = 1, nlag
        CALL correl(x(lag), y(lag), n-lag, r2, mux, muy, sigx, sigy)
        corr(lag) = r2
 1000 CONTINUE
 
      RETURN
      END
