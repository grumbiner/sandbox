      SUBROUTINE movice(ug, vg, x0, y0, x, y, dx, dy, npts)
C     Compute the motion of the non-interacting floes.
C     Robert Grumbine 9 December 1992 
C     Move to un-numbered do, 1 May 2014

      IMPLICIT none
      INCLUDE "sicedrft.inc"

      INTEGER atm_npts, npts, nlat, nlong
      PARAMETER (nlat  = (lat2-lat1)/dlat + 1)
      PARAMETER (nlong = (long2-long1)/dlon  )
      PARAMETER (atm_npts = nlat*nlong)

C     Physical items
      REAL radius, rdpdg
      PARAMETER (radius = 6.370949E6)
      PARAMETER (rdpdg  = 3.141592654/180.)
 
C     Declare the arguments.
      REAL ug(nlong, nlat), vg(nlong, nlat)
      REAL x0(npts), y0(npts), x(npts), y(npts), dx(npts), dy(npts)

C     Declare passing argument array variables.
      REAL ui(nlong, nlat), vi(nlong, nlat)
      REAL uf(atm_npts), vf(atm_npts), eta(atm_npts), lambda(atm_npts)
      INTEGER ig(atm_npts), jg(atm_npts)

C     Declare local utility variables
      INTEGER k
      REAL xt

!CD      PRINT *,'npts = ',npts,' atm_npts = ',atm_npts
C     Operational code.
C     Calls to crho, cf, and uageo moved to atmos. 3/23/92.

C     Compute the gridded ice drift velocity field
      CALL uice(ug, vg, ui, vi, nlong, nlat)

C     Now find where the marked floes are.
      CALL fndflo(x, y, ig, jg, eta, lambda, npts)

C     Compute the velocity of the floes by bilinear interpolation.
      CALL flovel(ui, vi, nlong, nlat,
     1            uf, vf, eta, lambda, ig, jg, npts) 

C     Now are ready to extrapolate the position of the floe.
      DO k = 1, npts
        IF (ABS(y(k)) .LT. 89.5) THEN !COS is < 0.01 poleward of this
          dx(k) = dx(k) + dt*uf(k)/radius/rdpdg/COS(rdpdg*y(k))
C         Avoid multiple wrap-arounds when near, but not at pole
          dx(k) = AMOD(dx(k), 360.)
         ELSE
!CD          PRINT *,'Floe is sitting at pole, cannot compute longitudinal
!CD     1 displacements.  Floe number ',k
          dx(k) = 0.0
        ENDIF 
        dy(k) = dy(k) + dt*vf(k)/radius/rdpdg
      ENDDO
      DO k = 1, npts
        x(k) = x0(k) + dx(k)
        y(k) = y0(k) + dy(k)
      ENDDO

C     Bullet-proofing: verify that extrapolated positions lie
C       within the domain.
C     Note that the new dx values must also be adjusted.
      DO k = 1, npts
 9200   CONTINUE
        IF (x(k) .LT. long1) THEN
!CD          PRINT *,'Extrapolated longitude too far west. ',k,x(k)
          xt = 360.+x(k)
          IF (xt .LT. long2 .AND. xt .GT. long1) THEN
!CD            PRINT *,'wrapping around ',xt
            x(k) = xt
           ELSE
            PRINT *,'Resetting to ',long1
            x(k) = long1
          ENDIF
          dx(k) = x(k)-x0(k)
        ENDIF
        IF (x(k) .GT. long2) THEN
          IF (x(k) .GT. long2 + dlon/2.) 
     1        PRINT *,'Extrapolated longitude too far east. ',k,x(k)
          xt = AMOD(x(k),360.)
          IF (xt .GE. long1 .AND. xt .LT. long2) THEN
!            PRINT *,'Wrapping around ',xt
            x(k) = xt
           ELSE
            PRINT *,'Resetting to ',long2
            x(k) = long2
          ENDIF
          dx(k) = x(k)-x0(k)
        ENDIF
        IF (y(k) .LT. lat1) THEN
          PRINT *,'Extrapolated latitude too far south. ',k,y(k)
          PRINT *,'Resetting to ',lat1
          y(k) = lat1
          dy(k) = y(k)-y0(k)
        ENDIF
        IF (y(k) .GT. lat2) THEN
          PRINT *,'Extrapolated latitude too far north. ',k,y(k)
          IF (lat2 .EQ. 90.) THEN
            PRINT *,'Resetting to ',180.-y(k)
            PRINT *,'trying to fix floe longitude ',k,x(k)
            y(k)  = 180.-y(k)
            dy(k) = y(k)-y0(k)
            x(k)  = x(k)-180.
            dx(k) = x(k)-x0(k)
            GO TO 9200
           ELSE
            PRINT *,'resetting to lat2 ',lat2
            y(k) = lat2
            dy(k) = y(k)-y0(k)
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END
