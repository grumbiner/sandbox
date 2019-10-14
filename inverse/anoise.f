      SUBROUTINE marnoise(y, n, rsize, alpha, seed)
C     Create autocorrelated first order Markov noise with 
C       magnitude rsize and persistence alpha.
C     Robert Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER n, seed
      REAL y(n), rsize, alpha, ran2

      INTEGER i
      REAL noise, onoise

      noise = rsize*(.5-ran2(seed))*2
      y(1) = noise
      onoise = noise

      DO 1000 i = 2, n
        noise  = rsize*(.5-ran2(seed))*2
        y(i)   = alpha*onoise + (1-alpha)*noise
        onoise = y(i)
 1000 CONTINUE

      RETURN
      END
