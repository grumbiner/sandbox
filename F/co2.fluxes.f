      SUBROUTINE fluxes(fluxaa, fluxna, time)
C     Robert Grumbine 27 Sep 1995

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
C     Now figure the right hand side for the given time and x's
      i = 0
C     Interpolate to fluxaa, fluxna
 100    i = i + 1
        IF (time .GT. table(i,ptime) .AND. i .LT. ntime) GO TO 100
C         the tabled time is less than (older) the chosen time.
C       Having reached this point, we've bracketed the time.
C       Compute the fraction of the two times to use for linearly
C         interpolating between table entries.
      itime = i

C     Polar to World Ocean Flux
      fract = (time - table(itime-1,ptime) )/
     1        (table(itime,ptime)-table(itime-1,ptime))
          
      fluxaa = fract *table(itime  ,pfaa) + 
     1                 (1.-fract)*table(itime-1,pfaa)
          
      fluxna = fract *table(itime  ,pfna) + 
     1                 (1.-fract)*table(itime-1,pfna)

      RETURN
      END
