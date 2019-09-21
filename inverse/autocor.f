      SUBROUTINE autocor(t, x, n, corr, nlag)
C     Compute autocorrelations
      INTEGER n, nlag
      REAL x(n), y(20000), z(20000), t(n), corr(nlag)
      INTEGER i, lag
      REAL r2, mux, muy, sigx, sigy
      
      DO 1000 lag = 1, nlag
        DO 1010 i = 1, n-lag
          z(i) = t(i)-x(i)
          y(i) = t(i+lag)-x(i+lag)
 1010   CONTINUE
        CALL correl(z, y, n-lag, r2, mux, muy, sigx, sigy)
        corr(lag) = r2
 1000 CONTINUE
 
      RETURN
      END
