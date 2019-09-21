      PROGRAM kohler
C     Program to compute the values on a Kohler curve for CCN

      REAL a, b, r, i
      REAL start, stop, incr

      PRINT *,'What is i?'
      READ (*,9001) i

      a = 9.73E-7
      b = 2.885E-16*i 

      PRINT *,'What size (in cm) do you want to use as the:'
      PRINT *,'  Starting size, final size, increment?'
      READ (*,9001) start, stop, incr
      WRITE (*,9001) start, stop, incr

      DO 1000 r = start, stop, incr
        WRITE (*,9001) r, 1. + a/r - b/r**3
 1000 CONTINUE

 9001 FORMAT (3G13.6)

      END
