      SUBROUTINE geo(ug, vg, temp, lnp, ztopo)

C     Test geostrophic wind computation
      IMPLICIT none
      INCLUDE "skile.inc"

      INTEGER nlat, nlong
      PARAMETER (nlat =  180./dlat + 1)
      PARAMETER (nlong = 360./dlon    )
 
      REAL lnp(mwave), temp(mwave), ztopo(mwave)

      REAL temper(nlong, nlat), c2(nlong, nlat), c3(nlong, nlat)
      REAL dlnpdx(nlong, nlat), dlnpdy(nlong, nlat)
      REAL dzdx(nlong, nlat), dzdy(nlong, nlat)
      REAL ug(nlong, nlat), vg(nlong, nlat)
      REAL ut(nlong, nlat), vt(nlong, nlat)
      
      REAL psmean, zmean
      REAL fac((nwave+1)*(nwave+2)/2)
      REAL work(idim, jdim), cosl(jdim)

C     Physical Parameters
      REAL g, r, omega, pi, f, rearth
      PARAMETER (g = 9.8062)
      PARAMETER (r = 287.06)
      PARAMETER (omega = 7.292116E-5)
      PARAMETER (pi    = 3.141592654)
      PARAMETER (rearth = 6.370949E6)
      INTEGER i, j, K

C     Convert temperature to regular grid.
CD      PRINT *,'Calling sphert'
      CALL SPHERT(-101, temper, temp, 0, fac, nlong, nlat, nwave, 0)
CD      PRINT *,'returning sphert'

C     Compute constant arrays:
      DO 1000 j = 1, nlat
        f = 2.*omega*SIN(pi/2.-FLOAT(j-1)/FLOAT(nlat-1) *pi)
        IF (ABS(f) .LT. 1.E-5) f = SIGN(1.E-5, f)
C     Preparation, compute cosines of the latitudes
        cosl(j) = COS(pi/2. - FLOAT(j-1)/FLOAT(nlat-1) *pi)
        DO 1010 i = 1, nlong
          c2(i,j) = r*temper(i,j)/f
          c3(i,j) = g/f
 1010   CONTINUE
 1000 CONTINUE

C     Find the gradients of LnP and z
      psmean = lnp(1)
CD      PRINT *,'Calling spherd 1'
      CALL SPHERD(-101, dlnpdx, dlnpdy, psmean, lnp, work(1,1), cosl,
     1   nlong, nlat, nwave, 0)
CD      PRINT *,'Calling spherd 1'

      zmean = ztopo(1) 
CD      PRINT *,'Calling spherd 2'
      CALL SPHERD(-101, dzdx, dzdy, zmean, ztopo, work(1,1), cosl,
     1   nlong, nlat, nwave, 0) 
CD      PRINT *,'Calling spherd 2'

C     Compute the geostrophic winds
      DO 2000 j = 1, nlat
        DO 2010 i = 1, nlong
          ut(i,j) = (-c2(i,j)*dlnpdy(i,j) - c3(i,j)*dzdy(i,j))
          vt(i,j) = ( c2(i,j)*dlnpdx(i,j) + c3(i,j)*dzdx(i,j))
 2010   CONTINUE
 2000 CONTINUE

      DO 3000 j = 1, nlat
        DO 3010 i = 1, nlong
          ug(i,nlat+1-j) = ut(i,j)
          vg(i,nlat+1-j) = vt(i,j)
 3010   CONTINUE
 3000 CONTINUE
    
      RETURN
      END
