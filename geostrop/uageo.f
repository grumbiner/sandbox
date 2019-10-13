      SUBROUTINE uageo(f, rho, slp, ug, vg,
     1                  nlong, nlat)
C     Compute the geostrophic wind by fourth order finite differences.

      IMPLICIT none

      INCLUDE "skile.inc"

C     Physical items
      REAL radius, rdpdg
      PARAMETER (radius = 6.370949E6)
      PARAMETER (rdpdg  = 3.141592654/180.)

C     Declare the arguments.
      INTEGER nlong, nlat
      REAL slp(nlong, nlat), rho(nlong, nlat), f(nlat)
      REAL ug(nlong, nlat), vg(nlong, nlat)

C     Declare local utility variables
      INTEGER i, j
      REAL c1

C     Declare bullet-proofing variables
      REAL fmax, fmin, rhohi, rholow, slphi, slplow, uglim
      PARAMETER (fmax = 2.*7.292116E-5)
      PARAMETER (rhohi  = 1.85 )
      PARAMETER (rholow = 0.95 )
      PARAMETER (slphi  = 1.15E5)
      PARAMETER (slplow = 0.85E5)
      PARAMETER (uglim  = 40.0  )
      REAL latmax, latmin
      PARAMETER (latmax =  5.0  )
      PARAMETER (latmin =  0.10 )
CD      PARAMETER (fmin = fmax*latmin*rdpdg/2.)
      PARAMETER (fmin = 1.E-5)

C     Bullet-proofing code: verify that latitude range is on
C       the planet, that the coriolis parameter, density,
C       and pressure are withing acceptable ranges.  Also
C       check that f does not equal zero (greater than about
C       2*omega*sin(0.1 degrees)

      DO 9000 j = 1, nlat
        DO 9010 i = 1, nlong
          IF (rho(i,j) .LT. rholow) THEN
            PRINT *,'resetting rho(i,j), to rholow ',i,j,rho(i,j)
            rho(i,j) = rholow
          ENDIF
          IF (rho(i,j) .GT. rhohi ) THEN
            PRINT *,'resetting rho(i,j), to rhohi  ',i,j,rho(i,j)
            rho(i,j) = rhohi 
          ENDIF
          IF (slp(i,j) .LT. slplow) THEN
            PRINT *,'resetting slp(i,j), to slplow ',i,j,slp(i,j)
            slp(i,j) = slplow
          ENDIF
          IF (slp(i,j) .GT. slphi ) THEN
            PRINT *,'resetting slp(i,j), to slplow ',i,j,slp(i,j)
            slp(i,j) = slphi 
          ENDIF
 9010   CONTINUE
 9000 CONTINUE

      DO 9100 i = 1, nlat
        IF (ABS(f(i)) .GT. fmax) THEN
          PRINT *,'Magnitude of f(i) too great, resetting to limit.'
          IF (f(i) .GT. 0.) THEN
            f(i) = fmax
           ELSE
            f(i) = -fmax
          ENDIF
         ELSE
          IF (ABS(f(i)) .LT. fmin) THEN
            PRINT *,'Magnitude of f(i) too small, resetting to limit.'
            IF (f(i) .GE. 0.) THEN
              f(i) = fmin
             ELSE
              f(i) = -fmin
            ENDIF
          ENDIF
        ENDIF
 9100 CONTINUE

C     Operational code 
      DO 1000 j = 3, nlat-2
        c1 = 1./f(j)/radius/rdpdg
        DO 1010 i = 3, nlong-2
           vg(i,j) =  c1/rho(i,j)/dlon/12.
     1     /COS((lat1+(j-1)*dlat)*rdpdg)
     2 *(-slp(i+2,j)+8.*slp(i+1,j)-8.*slp(i-1,j)+slp(i-2,j))
           ug(i,j) = -c1/rho(i,j)/dlat/12.*
     1  (-slp(i,j+2)+8.*slp(i,j+1)-8.*slp(i,j-1)+slp(i,j-2))
 1010   CONTINUE
 1000 CONTINUE
      DO 1020 i = 2, nlong-1
        j = 2
        vg(i,j)=  c1/rho(i,j)*(slp(i+1,j)-slp(i-1,j))
     1       /dlon/2./COS((lat1+(j-1)*dlat)*rdpdg)
        ug(i,j)=  -c1/rho(i,j)*(slp(i,j+1)-slp(i,j-1))
     1       /dlat/2.
        j = nlat-1
        vg(i,j)=  c1/rho(i,j)*(slp(i+1,j)-slp(i-1,j))
     1       /dlon/2./COS((lat1+(j-1)*dlat)*rdpdg)
        ug(i,j)=  -c1/rho(i,j)*(slp(i,j+1)-slp(i,j-1))
     1       /dlat/2.
 1020 CONTINUE
      DO 1030 j = 2, nlat-1
        i = 2
        vg(i,j)=  c1/rho(i,j)*(slp(i+1,j)-slp(i-1,j))
     1       /dlon/2./COS((lat1+(j-1)*dlat)*rdpdg)
        ug(i,j)=  -c1/rho(i,j)*(slp(i,j+1)-slp(i,j-1))
     1       /dlat/2.
        i = nlong-1
        vg(i,j)=  c1/rho(i,j)*(slp(i+1,j)-slp(i-1,j))
     1       /dlon/2./COS((lat1+(j-1)*dlat)*rdpdg)
        ug(i,j)=  -c1/rho(i,j)*(slp(i,j+1)-slp(i,j-1))
     1       /dlat/2.
 1030 CONTINUE

      RETURN
      END
