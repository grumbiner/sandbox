      SUBROUTINE getdat(slp, temp, ztopo, iunit)
C     Note that the files must be attached externally, i.e.
C       assign -a /tmp/mrfprz/s2d.fxxx fort.yy 
C     where xxx is the forecast time, and yy is the unit number.
C
C     LAST MODIFIED: 23 September 1993

C     IMPLICIT none is a non-standard feature.  It compels 
C       all variables to be typed, on systems which recognize it.
      IMPLICIT none

      INCLUDE "skile.inc"

      INTEGER iunit
      REAL slp(mwave), temp(mwave), ztopo(mwave)

C     Header info for sigma files
      CHARACTER*8 lab(4)
      REAL fhour, dphi(kdim+1), dlam(kdim)
      INTEGER*4 idate(4)

C     Local variables. 
      INTEGER i, k 

C     Program version for using sigma files from forecast.

      READ (iunit) lab
      READ (iunit) fhour, (idate(i),i=1,4), (dphi(k),k=1,kdim+1),
     1                    (dlam(k),k=1,kdim)
CD      WRITE (*,*) lab
      WRITE (*,*) fhour, (idate(i),i=1,4)

      READ (iunit) (ztopo(i),i=1,mwave)
      READ (iunit) (slp(i),i=1,mwave)
      READ (iunit) (temp(i),i=1,mwave)

C     Note that the slp field is actually LN(Ps), where Ps in in
C       kPa.

      RETURN
      END
