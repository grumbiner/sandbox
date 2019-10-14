      PROGRAM radcomp
C     Compare the longwave radiation calculated by the MRF with
C       that computed by sea ice algorithms.
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "mgrid.inc"
      INCLUDE "physical.inc"
C     Interpolation parameters
      INTEGER tlat, tlong
      PARAMETER (tlong = 360./dlonm   )
      PARAMETER (tlat  = 180./dlatm +1)

      REAL tair(0:L, 0:M), cloud(0:L, 0:M), es(0:L, 0:M), rh(0:L,0:M)
      REAL lwmrf(0:L, 0:M), lwice(0:L, 0:M)
      REAL msum, isum

      REAL swdn(tlong, tlat), lwdn(tlong, tlat), t2(tlong, tlat)
      REAL uten(tlong, tlat), qsen(tlong, tlat), clouda(tlong, tlat)
      REAL mask(tlong, tlat), q2(tlong, tlat), vten(tlong, tlat)
      REAL qlat(tlong, tlat), ps(tlong, tlat)

      INTEGER iunit
      DOUBLE PRECISION W3FA09

      INTEGER i, j, k

C     For conversion between spherical meteo grid coords and
C       spherical ice coords
      INTEGER imet, jmet, im, jm
      REAL longm, latm
      imet(longm) = 1 + INT(longm/dlonm)
      jmet(latm)  = 1 + INT((90.-latm)/dlatm)
C=========================
CD      ps = 1.E5

C     Get the data
      iunit = 10
      CALL getflx(swdn, lwdn, t2, q2, uten, vten, 
     1            qsen, qlat, clouda, mask, iunit)

      DO 1000 j = 0, MM
        DO 1100 i = 2, LM
          latm = latmin + (j-0.5)*dlat
          longm = -10.+i*dlon
          im = imet(longm)
          jm = jmet(latm)
 
          tair(i,j) = (t2(im, jm)+t2(im,jm+1))/2.
          cloud(i,j) = (clouda(im, jm)+clouda(im,jm+1))/2.
          lwmrf(i,j) = (lwdn(im, jm)+lwdn(im,jm+1))/2.

 1100   CONTINUE
 1000 CONTINUE


CD      k = (LM-1)*(MM+1)
      k = (L+1)*(M+1)
      CALL VAPOR(tair, es, 1, k)
      DO 1200 j = 0, MM
        DO 1300 i = 2, LM
          latm = latmin + (j-0.5)*dlat
          longm = -10.+i*dlon
          im = imet(longm)
          jm = jmet(latm)
          rh(i,j) = (q2(im, jm)+q2(im, jm+1))/2./1.E4
CD     1               * ps(i, j) / w3fa09(tair(i,j)) / 1.E3
     1               * ps(i, j) / es(i,j)
     2    * EPSI 
          IF (rh(i,j) .LT. 0.) rh(i,j) = 0.
          PRINT *,i,j, rh(i,j)
 1300   CONTINUE
 1200 CONTINUE

C     Compute the radiative balance
      DO 2000 j = 0, MM
        DO 2100 i = 2, LM
          lwice(i,j) = (1.+0.3*cloud(i,j)**2) 
     1 * D3 * tair(i,j)**4
     1 * (0.605+5.95E-7*rh(i,j)*es(i,j)*EXP(1500./tair(i,j)))
CD          PRINT *,i,j,cloud(i,j), tair(i,j), rh(i,j), es(i,j), 
CD     1 lwice(i,j)
 2100   CONTINUE
 2000 CONTINUE
  
C     Print out result
      msum = 0.0
      isum = 0.0
      DO 3000 j = 0, MM
        DO 3100 i = 2, LM
          PRINT *,i, j, lwmrf(i,j), lwice(i,j), 
     1     (lwmrf(i,j)-lwice(i,j))/(lwmrf(i,j)+lwice(i,j))*2.
          msum = msum+lwmrf(i,j)
          isum = isum+lwice(i,j)
 3100   CONTINUE
 3000 CONTINUE
      msum = msum/(MM+1)/(LM-1)
      isum = isum/(MM+1)/(LM-1)
      PRINT *,'net ',msum, isum, (msum-isum)/(msum+isum)*2.
      STOP
      END
