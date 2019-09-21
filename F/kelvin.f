      PROGRAM kelvin
C     Program to compute the values on a kelvin curve for CCN
C     Robert Grumbine 27 Sep 1995.

      REAL a, b, r, i
      REAL start, stop, incr

C     PRINT *,'What is i?'
C     READ (*,9001) i

      a = 9.73E-7
C     b = 1.00E-15 

      PRINT *,'What size (in cm) do you want to use as the:'
      PRINT *,'  Starting size, final size, increment?'
      READ (*,9001) start, stop, incr
      WRITE (*,9001) start, stop, incr

      DO 1000 r = start, stop, incr
        WRITE (*,9001) r, exp( a/r )
 1000 CONTINUE

 9001 FORMAT (3G13.6)

      END
