      SUBROUTINE fluxes(fluxaa, fluxna, time)
      IMPLICIT none
      REAL fluxaa, fluxna

C     Declare the flux history variables of aabw and nadw
      INTEGER ptime, pfaa, pfna, ntime
      PARAMETER (ptime = 1)
      PARAMETER (pfaa  = 2)
      PARAMETER (pfna  = 3)
      PARAMETER (ntime = 20)
      REAL table(ntime, 3)

C     Temporary variables used in the solutions:
      REAL fract, time
      INTEGER i, itime

C*************************************************----------++++++++++!!
C     Define the flux history (this should really be data)

      INCLUDE "bwhist.inc"

C*************************************************----------++++++++++!!
C     Polar to World Ocean Flux

      fluxaa = table(time, times, pfaa) 
      fluxna = table(time, times, pfna) 

      RETURN
      END
