      PROGRAM bc2
C     Program to create inversions of sample data.
C     For support of the data inversion report.
C     11-23-93  Version 1: Apply a smoothness constraint 
C                            to a scalar field in time.
C     11-23-93  Version 2: Put gaps into the observing series.
C     11-24-93  Version 3: Synthetically smooth very gappy series --
C                            average the true, 'observed', and smoothed
C                            while using full data in computing smoothed.
C     11-24-93  Version 4: Determine boundary values from time series,
C                            rather than by specification.
C     11-26-93  Version 5: Remove unused variables.  Make gappy versus
C                            full series correlation computation automatic.
C
C     Subroutines:
C       Mksmpl - 11-23-93 Make a time series for a single point.
C              - 11-24-93 -- option for a sinusoidal series.
C       Mkrand - 11-23-93 Add random noise to a vector.
C       Wght1  - 11-23-93 Create terms in weighting matrix for inversion
C              - 11-24-93 -- modified for determining boundary values
C       Tridig - 11-23-93 Invert Tri-diagonal matrix (Num. Recipes)
C       Correl - XX-XX-XX BG correlation functions.
C       Sample - 11-23-93 Determine whether data was successfully sampled.
C       Synth  - 11-24-93 Construct a synthetic series based on gappiness,
C                           If original series is half gaps, new series has
C                           time resolution of 2dt.

      IMPLICIT none
      
      INTEGER tzero, tline, thi
      PARAMETER (tzero = 4)
      PARAMETER (tline = 20)
      PARAMETER (thi   = 121)

      REAL dt, epsilon, rsize, gaps
      PARAMETER (dt = 1.0)
      INTEGER m, n, i, j
      PARAMETER (n = thi)
      REAL x(n), y(n), z(n), w(n)
      REAL x2(n), y2(n), z2(n), w2(n)
      REAL a(n), b(n), c(n), r(n)

      REAL rxy, rxz, ryz, ran2
      REAL sxy, sxz, syz
      REAL d1, d2, d3, d4, ia
      CHARACTER*60 fname
      INTEGER npts, nser, seed

      PRINT *,'What size random errors would you like?'
      READ (*,9001) rsize
      PRINT *,'What magnitude of epsilon would you like?'
      READ (*,9001) epsilon
      PRINT *,'What is the probability of a gap occurring?'
      READ (*,9001) gaps
 9001 FORMAT (E13.6)
      PRINT *,'How many series to average?'
      READ (*,9010) nser
 9010 FORMAT (I3)

      PRINT *,'What is the name of the output file?'
      READ (*,9009) fname
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='NEW')
 9009 FORMAT (A60) 
      
      seed = -1
      ia = RAN2(seed)
      sxy = 0.
      sxz = 0.
      syz = 0.

      DO 9998 j = 1, nser
        
        CALL mksmpl(x, n, tzero, tline, thi, 2)
        CALL veceq(x, y, n)
        CALL mkrand(x, y, n, rsize, seed)
        CALL sample(w, n, gaps, seed)
        
        CALL wght1(y, a, b, c, r, dt, epsilon, n, w, 2)
        CALL tridig(a, b, c, r, z, n)
        
        CALL synth(x, x2, w, n, gaps, m, w2)
        CALL synth(y, y2, w, n, gaps, m, w2)
        CALL synth(z, z2, w, n, gaps, m, w2)

        DO 9999 i = 1, n
          WRITE (10,9002) i, CHAR(9), x(i), CHAR(9), y(i), CHAR(9), z(i)
 9999   CONTINUE
 9002   FORMAT (I4, 3(A1,F7.3))

        IF (m .EQ. n) THEN
          CALL correl(x, y, n, rxy, d1, d2, d3, d4)
          WRITE (*,9003) rxy, d1, d2, d3, d4
          CALL correl(x, z, n, rxz, d1, d2, d3, d4)
          WRITE (*,9003) rxz, d1, d2, d3, d4
          CALL correl(y, z, n, ryz, d1, d2, d3, d4)
          WRITE (*,9003) ryz, d1, d2, d3, d4
          PRINT *,' '
         ELSE
          npts = 0
          DO 1000 i = 1, m
            IF (w2(i) .NE. 0) THEN
              npts = npts + 1
              x(npts) = x2(i)
              y(npts) = y2(i)
              z(npts) = z2(i)
            ENDIF
 1000     CONTINUE
          CALL correl(x, y, npts, rxy, d1, d2, d3, d4)
          WRITE (*,9003) rxy, d1, d2, d3, d4, FLOAT(npts)/FLOAT(m), m
          CALL correl(x, z, npts, rxz, d1, d2, d3, d4)
          WRITE (*,9003) rxz, d1, d2, d3, d4, FLOAT(npts)/FLOAT(m), m
          CALL correl(y, z, npts, ryz, d1, d2, d3, d4)
          WRITE (*,9003) ryz, d1, d2, d3, d4, FLOAT(npts)/FLOAT(m), m
          PRINT *,' '
        ENDIF

        sxy = sxy+rxy
        sxz = sxz+rxz
        syz = syz+ryz

 9998 CONTINUE
      
      PRINT *,' '
      WRITE (*,9003) sxy/nser, sxz/nser, syz/nser
      
 9003 FORMAT (6F7.3, I4)

      PAUSE
      STOP
      END
