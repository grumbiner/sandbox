      SUBROUTINE getsig(slp, temp, ztopo, qsig, iunit, sigma)
C=======================================================================
C  Programmed by:
C     Robert W. Grumbine          NMC  Camp Springs, MD          Dec '92
C  Purpose:
C     Read in MRF forecat sigma fields.
C  EXTERNALS:
C     None
C=======================================================================
      IMPLICIT none
      INCLUDE "mgrid.inc"
C=======================================================================

      INTEGER iunit
      REAL slp(mwave), temp(mwave), ztopo(mwave), qsig(mwave)
      REAL sigma, dummy(mwave)

C     Grid parameters.
      INTEGER ijdim
      PARAMETER (ijdim = idim*jdim)

C     Header info for sigma files
      CHARACTER*8 lab(4)
      REAL fhour, dphi(kdim+1), dlam(kdim)
      INTEGER*4 idate(4)
      
C     Local variables. 
      INTEGER i, k, klev
      REAL delsig, olddel
C=======================================================================

C     Program version for using sigma files from forecast.
C     Read in from sigma files
      READ (iunit) lab
      READ (iunit) fhour, (idate(i),i=1,4), (dphi(k),k=1,kdim+1),
     1                    (dlam(k),k=1,kdim)

C     Determine which level is nearest the desired sigma level.
C     Assume that the levels are arranged in monotonically in/de-
C       creasing order.
      k = 1
      olddel = ABS(sigma-dlam(1))
 1000 CONTINUE
        delsig = ABS(sigma-dlam(k+1))
        IF (delsig .GT. olddel) THEN
          klev = k
          GO TO 2000
         ELSE
          olddel = delsig
          k = k + 1
          GO TO 1000
        ENDIF 
 2000 CONTINUE

      READ (iunit) (ztopo(i),i=1,mwave)
      READ (iunit) (slp(i),i=1,mwave)
C     Note that the slp field is actually LN(Ps), where Ps in in
C       kPa.

      DO 3000 k = 1, klev
        READ (iunit) (temp(i),i=1,mwave)
 3000 CONTINUE
      DO 3100 k = klev+1, kdim
        READ (iunit) (dummy(i),i=1,mwave)
 3100 CONTINUE

C     Dummy reads through the divergence fields
      DO 4000 k = 1, kdim
        READ (iunit) (dummy(i),i=1,mwave)
 4000 CONTINUE
C     Dummy reads through the vorticity fields
      DO 4100 k = 1, kdim
        READ (iunit) (dummy(i),i=1,mwave)
 4100 CONTINUE
C     Read to the q field
      DO 4200 k = 1, klev
        READ (iunit) (qsig(i),i=1,mwave)
 4200 CONTINUE
CD      DO 4300 k = klev+1, kdim
CD        READ (iunit) (dummy(i),i=1,mwave)
CD 4300 CONTINUE
C     Now in a position to read the rain field.      
  
      RETURN
      END
