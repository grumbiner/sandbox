C***********************************************************----------!!
      FUNCTION rho(s, t, p)
C     Compute the density of water as a function of salinity,
C       temperature, and pressure.
C     Use the entire equation of state Given in Gill, 1982.
C     s in psu, t in degrees C, p in bars

      REAL rho
      REAL s, t, p

      DOUBLE PRECISION sigmaw, sigma, sigmap
      DOUBLE PRECISION deltaw, delta, deltap

      sigmaw = -.157406 + t*(6.793952D-2 + t*(-9.09529D-3
     1      + t*(1.001685D-4 +t*(-1.120083D-6 + 6.536332D-9*t))))

      sigma  = sigmaw +
     1    s* ( .824493 + t*(-4.0899D-3 + t*(7.6438D-5
     2                      + t*(-8.2467D-7 + t*5.3875D-9))) ) +
     3    s**1.5*( -5.72466D-3 + t*(1.0227D-4 - t*1.6546D-6)) +
     4    s*s   *( 4.8314D-4 )

C     Now compute the compressibility terms:
      deltaw = -347.79 + t*(148.4206 + t*(-2.327105 +
     1                     t*(-1.360477D-2 - t*5.155288D-5 )))

      delta  = deltaw +
     1    s  * (54.676 + t*(-.603459 + t*(1.09987D-2 - t*6.167D-5)) )
     2  + s**1.5*( 7.944D-2 + t*(1.6483D-2 - t*5.3009D-4) )

      deltap = delta +
     1   p *  (3.239908+t*(1.43713D-3+t*(1.16092D-4-t*5.77905D-7)) )
     2  +p*s* (2.2838D-3 + t*(-1.0981D-5*t - t*1.6078D-6) )
     3  +p*s**1.5*( 1.91075D-4 )
     4  +p*p* (8.50935D-5 + t*(-6.12293D-6 + t*5.2787D-8) )
     5  +p*p*s*(-9.9348D-7 +t*(2.0816D-8 + t*9.1697D-10) )

C     Now compute the density:
      rho = (1000. + sigma)/ (1 - p/(20000.+ deltap) )

      RETURN
      END
