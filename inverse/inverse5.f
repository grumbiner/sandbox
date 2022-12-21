      PROGRAM inverse
C     Program to create inversions of sample data.
C     For support of the data inversion report.
C     11-23-93  Version 1: Apply a smoothness constraint 
C                            to a scalar field in time.

C     Subroutines:
C       Mksmpl - 11-23-93 Make a time series for a single point.
C       Mkrand - 11-23-93 Add random noise to a vector.
C       Wght1  - 11-23-93 Create terms in weighting matrix for inversion
C       Tridig - 11-23-93 Invert Tri-diagonal matrix (Num. Recipes)
C       Correl - XX-XX-XX BG correlation functions.
C       Sample - 11-23-93 Determine whether data was successfully sampled.

      IMPLICIT none
      
      INTEGER tzero, tline, thi
      PARAMETER (tzero = 4)
      PARAMETER (tline = 5)
      PARAMETER (thi   = 90)

      REAL dt, epsilon, rsize, gaps
      PARAMETER (dt = 1.0)
      INTEGER n, i, j
      PARAMETER (n = 2*tzero+2*tline+thi)
      REAL x(n), y(n), z(n), w(n)
      REAL a(n), b(n), c(n), r(n)

      REAL rxy, rxz, ryz, ran2
      REAL sxy, sxz, syz
      REAL d1, d2, d3, d4, ia
      CHARACTER*60 fname
      INTEGER nser, seed

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
        CALL mksmpl(x, n, tzero, tline, thi)
        CALL veceq(x, y, n)
        CALL mkrand(x, y, n, rsize, seed)
        CALL sample(w, n, gaps, seed)
        CALL wght1(y, a, b, c, r, dt, epsilon, n, w)
        CALL tridig(a, b, c, r, z, n)

CD        DO 9999 i = 1, n
CD          WRITE (10,9002) i, x(i), y(i), z(i)
CD 9999   CONTINUE
 9002 FORMAT (I4, 3F7.3)
      
        CALL correl(x, y, n, rxy, d1, d2, d3, d4)
        WRITE (*,9003) rxy, d1, d2, d3, d4
        CALL correl(x, z, n, rxz, d1, d2, d3, d4)
        WRITE (*,9003) rxz, d1, d2, d3, d4
        CALL correl(y, z, n, ryz, d1, d2, d3, d4)
        WRITE (*,9003) ryz, d1, d2, d3, d4
        sxy = sxy+rxy
        sxz = sxz+rxz
        syz = syz+ryz

CD        CALL agree(x, d1, y, n, ia)
CD        PRINT *,'Index of agreement x-y',ia
CD        CALL agree(x, d1, z, n, ia)
CD        PRINT *,'Index of agreement x-z',ia
CD        CALL agree(z, d1, y, n, ia)
CD        PRINT *,'Index of agreement z-y',ia
 
 9998 CONTINUE
      PRINT *,' '
      WRITE (*,9003) sxy/nser, sxz/nser, syz/nser
 
 9003 FORMAT (5F7.3)

      STOP
      END
