      SUBROUTINE getsig(slp, temp, ztopo, iunit, twave)
C     Read in the spectral coefficients for sea level pressure,
C       temperature, and topography.
C     Note that the files must be attached externally, i.e.
C       assign -a /tmp/mrfprz/s2d.fxxx fort.yy 
C       where xxx is the forecast time, and yy is the unit number.
C     Bob Grumbine 4 April 1994.

C     IMPLICIT none is a non-standard feature.  It compels 
C       all variables to be typed, on systems which recognize it.
      IMPLICIT none

      INCLUDE "sicedrft.inc"

      INTEGER iunit
      REAL slp(mwave), temp(mwave), ztopo(mwave)
      INTEGER twave

C     Header info for sigma files
      CHARACTER*8 lab(4)
      REAL fhour
CD      INTEGER*4 idate(4)
      INTEGER idate(4)
      REAL EXT(245)      !Peculiar variable needed to get to the number
                         !  of levels+wave number
C     Local variables. 
      INTEGER i, k

C     Program version for using sigma files from forecast.

      READ (iunit)                   !Dummy read needed
      READ (iunit) fhour, idate, ext

      WRITE (*,*) fhour, (idate(i),i=1,4)
CD      PRINT *,'ext 202, 203 ',EXT(202), EXT(203)
      twave = EXT(202)

      ztopo = 0
      slp   = 0
      temp  = 0

      READ (iunit) (ztopo(i),i=1,(twave+1)*(twave+2))
      READ (iunit) (slp(i),i=1,(twave+1)*(twave+2))
      READ (iunit) (temp(i),i=1,(twave+1)*(twave+2))

C     Note that the slp field is actually LN(Ps), where Ps in in
C       kPa.

      RETURN
      END
