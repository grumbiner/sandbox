      SUBROUTINE synth(x, x2, w, n, gaps, useall, m, w2)
C     Subroutine to construct a synthetic time series, subsampled
C       proportional to 1/(1-gaps).  w determines whether there is
C       a gap at a given time.  
C       Useall is a logical which controls whether the full time
C        series (ignoring gaps) is to be used.
      INTEGER n, m
      REAL gaps, x(n), x2(n), w(n), w2(n)
      LOGICAL useall

      INTEGER i, j, k, nsamp, resamp

      resamp = INT(1./(1.-gaps))
      m = n / resamp

      IF (resamp .EQ. 1) THEN
        DO 1 i = 1, n
          x2(i) = x(i)
          w2(i) = 1.0
   1    CONTINUE
        RETURN
      ENDIF

C     Note, must deal with series which are not integral multiples.
C     Currently ignoring. 11-24-93 BG      
      IF (useall) THEN
        DO 1000 i = 1, n, resamp
          k = 1+i/resamp
          x2(k) = 0.
          DO 1100 j = 1, resamp
            x2(k) = x2(k)+x(i-1+j)
 1100     CONTINUE
          x2(k) = x2(k)/resamp
 1000   CONTINUE
      
       ELSE
        DO 2000 i = 1, n, resamp
          k = 1+i/resamp
          x2(k) = 0.
          w2(k) = 0
          nsamp = 0
          DO 2100 j = 1, resamp
            IF (w(i+j-1) .NE. 0) THEN
              nsamp = nsamp + 1
              x2(k) = x2(k) + x(i+j-1)
              w2(k) = 1.
            ENDIF
 2100     CONTINUE
          IF (nsamp .NE. 0) THEN
            x2(k) = x2(k) / nsamp
          ENDIF
 2000   CONTINUE

      ENDIF



      RETURN
      END
