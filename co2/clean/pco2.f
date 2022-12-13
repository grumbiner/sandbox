      FUNCTION pco2(co2, temp, rsalt, press)
C     Compute the equilibrium partial pressure of co2 in the ocean,
C       given the co2 concentration in atmosphere, the temperature, 
C       pressure and the salinity in the ocean.
C     Robert Grumbine 8 April 1994.

      IMPLICIT none

      DOUBLE PRECISION pco2, co2, temp, press
      REAL rsalt, h, tk

C     Kelvin conversion:
      REAL tkelvin
      PARAMETER (tkelvin = 273.15)

C     PARAMETERS for the henry's law constant for co2
      REAL a1, a2, a3, b1, b2, b3
      PARAMETER (a1 = -60.2409)
      PARAMETER (a2 =  93.4517)
      PARAMETER (a3 =  23.3585)
      PARAMETER (b1 =  0.023517)
      PARAMETER (b2 = -0.023656)
      PARAMETER (b3 =  0.0047036)

      tk = tkelvin + temp
      h = a1 + a2*(100./tk)+a3*LOG(tk/100.)
     1  + rsalt*(b1+b2*(tk/100.) + b3*(tk/100.)*(tk/100.))
      h = EXP(h)

      pco2 = co2/h

      RETURN
      END
