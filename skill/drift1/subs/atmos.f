      SUBROUTINE atmos(slp, t, ztopo, nlat, nlong, ua, va, 
     1                 mwave, twave)
C     Derive required atmospheric information for the virtual
C       drift sea ice model(s).
C     Required input are the spectral coefficients for the
C       sea level pressure, surface temperature, and surface
C       topography.
C     Note that the topography is not necessarily zero over the
C       ocean, due to the spectral representation.
C     Output is the u and v components of the wind field.  
C       Currently these are the geostrophic winds.
C     Bob Grumbine 4 April 1994.

      IMPLICIT none
CD      INCLUDE "sicedrft.inc"

C     Declare arguments      
      INTEGER nlat, nlong, mwave, twave
      REAL slp(mwave), t(mwave), ztopo(mwave)
      REAL ua(nlong, nlat), va(nlong, nlat)

C     Call the spectrally-differenced geostrophic wind routine.
      CALL geowin(ua, va, t, slp, ztopo, twave)

      RETURN
      END
