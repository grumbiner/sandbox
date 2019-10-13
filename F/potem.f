      FUNCTION potem(t, s, pd)
C     Compute the potential temperature from the Bryden 1973 formula
C       Given in Gill 1982, p. 602.
C     Input is temperature, salinity, pressure (or depth)
C     Units are Deg. C., psu, dbar
C     Written by Robert Grumbine 12-29-1988 for compatibility with 
C       Bryan and Cox 1972 program for computing an approximate equation
C       of state.

      REAL potem
      REAL t, s, pd

      REAL p, sp

C     Convert to bars:
      p = 0.1 * pd
C     Formula from Bryden uses deviation in s from 35 ppt.
      sp = s - 35.

      potem = t 
     1 - p* ( 3.6504E-4 + t*(8.3198E-5 + t*(-5.4065E-7 + t*4.0274E-9)) )
     2 - p*sp* (1.7439E-5 - t*2.9778E-8 )
     3 - p*p*  (8.9309E-7 + t*(-3.1628E-8 + t*2.1987E-10) )
     4 + p*p*sp*4.1057E-9
     5 - p*p*p * (-1.6056E-10 + 5.0484E-12*t)

      RETURN
      END
