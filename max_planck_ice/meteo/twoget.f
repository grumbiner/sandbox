      SUBROUTINE twoget(pole, siglev, u850, v850, t850, rh850,
     1   ps, precip, cloud, lwuc, lwic, swic, step)
C=====================================================================--
C  Programmed by:
C     Robert W. Grumbine          NMC  Camp Springs, MD         Dec. '92
C  LAST MODIFIED: 9 November 1994.
C  Purpose:
C     Get data from MRF forecasts for ice model, in the case that the
C       ice model grid is a 2.5*5.0 lat-long spherical grid.  (PTYPE=2)
C     Obsolete now due to lack of maintenance 25 October 1994. BG.
C  EXTERNALS:
C     getsig  -  Get sigma file data from MRF
C     getflx  -  Get flux file data from MRF
C     SPHERT  -  Convert sigma file data to regular spherical grid
C=====================================================================--
      IMPLICIT none
      INCLUDE "mgrid.inc"
C=====================================================================--
      INTEGER idim, jdim
C     Meteorological Grid parameters.
      PARAMETER (idim = 3*nwave+6     )
      PARAMETER (jdim = (3*nwave+2)/2 )

      INCLUDE "icegrid.inc"
C     Interpolation parameters
      INTEGER tlat, tlong
      PARAMETER (tlong = 360./dlonm   )
      PARAMETER (tlat  = 180./dlatm +1)

C     Physical Parameters
      INCLUDE "physical.inc"

      INTEGER pole
C     Sigma file data
      REAL siglev

C     Flux file data 
      REAL slpf (tlong, tlat)
      REAL lwup(tlong, tlat), lwdn(tlong, tlat), swdn(tlong, tlat)
      REAL uten(tlong, tlat), vten(tlong, tlat)
      REAL t2(tlong, tlat)  , q2(tlong, tlat)
      REAL mask(tlong, tlat), clouda(tlong, tlat)

C     Arguments to return
      REAL u850(L, M), v850(L, M)
      REAL rh850(0:L, 0:M), ps(0:L, 0:M)
      REAL t850(0:L, 0:M), precip(0:L, 0:M), cloud(0:L, 0:M)
      REAL lwuc(0:L, 0:M), lwic(0:L, 0:M), swic(0:L, 0:M)
      INTEGER step
C=====================================================================--

C     Local variables. 
      INTEGER i, j, iunit
      REAL*8 W3FA09
C=====================================================================--

C     For conversion between spherical meteo grid coords and
C       spherical ice coords
      INTEGER imet, jmet, im, jm
      REAL longm, latm
      imet(longm) = 1 + INT(longm/dlonm)
      jmet(latm)  = 1 + INT((90.-latm)/dlatm)

C=====================================================================--
C     Begin the routine 

      PRINT *,'twoget is no longer supported!'
      STOP
      iunit = 50+step
      CALL getflx(slpf, swdn, lwup, lwdn, t2, q2, uten, vten, 
     1            clouda, mask, iunit)
      CLOSE (iunit)

C     Convert the sigma files to lat-long grid

C     Transfer the data to the stossel grid

      DO 2000 j = 0, MM
        DO 2100 i = 2, LM
          latm = latmin + (j-0.5)*dlat
          longm = -10.+i*dlon
          im = imet(longm)
          jm = jmet(latm)
 
          precip(i,j) = 0.0
          ps(i,j) = (slpf(im, jm)+slpf(im,jm+1))/2.
          t850(i,j) = (t2(im, jm)+t2(im,jm+1))/2.
          cloud(i,j) = (clouda(im, jm)+clouda(im,jm+1))/2.
          lwic(i,j)  = (lwdn(im, jm) + lwdn(im,jm+1))/2.
          lwuc(i,j)  = (lwup(im, jm) + lwup(im,jm+1))/2.
          swic(i,j)  = (swdn(im, jm) + swdn(im,jm+1))/2.

          rh850(i,j) = (q2(im, jm)+q2(im, jm+1))/2./1.E4
     1               * ps(i, j) * EPSI /
     2           (W3FA09(t850(i,j))*10.) * 100.
          IF (rh850(i,j) .LT. 0.) rh850(i,j) = 0.
          IF (rh850(i,j) .GT. 100.) rh850(i,j) = 100.
C         Stossel reads in degrees C rather than K
          t850(i,j) = t850(i,j) - TMELT
C         Stossel reads in PA , rather than mb
          ps(i,j) = ps(i,j)*100.
 2100   CONTINUE
 2000 CONTINUE
 9001 FORMAT (4F12.5)

      DO 2200 j = 1, MM
        DO 2300 i = 2, LM
          latm = latmin + (j-0.5)*dlat
          longm = -10.+i*dlon
          im = imet(longm)
          jm = jmet(latm)
          u850(i,j) = (uten(im, jm)+uten(im, jm+1))/2.
          v850(i,j) = (vten(im, jm)+vten(im, jm+1))/2.
 2300   CONTINUE
 2200 CONTINUE

C     Carry out the wrap-around
      DO 3000 j = 1, M
        ps(0,j)   = ps(LM2,j)
        t850(0,j) = t850(LM2,j)
        rh850(0,j) = rh850(LM2,j)
        precip(0,j) = precip(LM2,j)
        cloud(0,j) = cloud(LM2,j)

        ps(1,j)   = ps(LM,j)
        t850(1,j) = t850(LM,j)
        rh850(1,j) = rh850(LM,j)
        precip(1,j) = precip(LM,j)
        cloud(1,j) = cloud(LM,j)
        u850(1,j) = u850(LM,j)
        v850(1,j) = v850(LM,j)

        ps(L,j)     = ps(2,j)
        t850(L,j)   = t850(2,j)
        rh850(L,j)  = rh850(2,j)
        precip(L,j) = precip(2,j)
        cloud(L,j)  = cloud(2,j)
        u850(L,j)   = u850(2,j)
        v850(L,j)   = v850(2,j)
 3000 CONTINUE

      DO 30 J=1,M
       U850(1,J)=U850(LM,J)
       V850(1,J)=V850(LM,J)
       U850(L,J)=U850(2 ,J)
       V850(L,J)=V850(2 ,J)
   30 CONTINUE
      DO 31 I=1,L
       U850(I,M)=U850(I,MM)
       V850(I,M)=V850(I,MM)
   31 CONTINUE
C-----------------------------------------------------------------------
      DO 12 I=0,L
       T850(I,MM)=T850(I,MM2)
       T850(I,0)=T850(I,1)
       T850(I,M)=T850(I,MM)
   12 CONTINUE
C-----------------------------------------------------------------------
      DO 41 I=0,L
       rh850(I,MM)=rh850(I,MM2)
       rh850(I,0)=rh850(I,1)
       rh850(I,M)=rh850(I,MM)
   41 CONTINUE
C-----------------------------------------------------------------------
      DO 13 I=0,L
       ps(I,MM)=ps(I,MM2)
       ps(I,0)=ps(I,1)
       ps(I,M)=ps(I,MM)
   13 CONTINUE
C-----------------------------------------------------------------------
      DO 14 I=0,L
       cloud(I,MM)=cloud(I,MM2)
       cloud(I,0)=cloud(I,1)
       cloud(I,M)=cloud(I,MM)
   14 CONTINUE
C-----------------------------------------------------------------------
      DO 42 I=0,L
       precip(I,MM)=precip(I,MM2)
       precip(I,0)=precip(I,1)
       precip(I,M)=precip(I,MM)
   42 CONTINUE

      RETURN
      END
