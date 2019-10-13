      SUBROUTINE fndflo(x, y, ig, jg, eta, lambda, npts)
C     Locate the ice floes with respect to the model grid.
C     Bob Grumbine 4 April 1994.

      IMPLICIT none
      INCLUDE "sicedrft.inc"

C     Declare the arguments.
      INTEGER npts
      REAL x(npts), y(npts)
      REAL eta(npts), lambda(npts)
      INTEGER ig(npts), jg(npts)

C     Declare local utility variables
      INTEGER k
      REAL xt

C     Bullet-proofing code: verify that the x-y coordinates of
C       the floes are inside the domain.
CC    Should fix the algorithms to deal with floes on boundary
CC      cells.  Also flovel and movice.
      DO 9000 k = 1, npts
        IF (x(k) .LT. long1) THEN
          PRINT *,'Floe too far west. x= ',k,x(k)
          xt = 360.+x(k)
          IF (xt .GE. long1 .AND. xt .LE. long2) THEN
            PRINT *,'Wrapping floe around grid',xt
            x(k) = xt
           ELSE
            PRINT *,'Resetting to western bndy.',long1
            x(k) = long1
          ENDIF
        ENDIF
        IF (x(k) .GE. long2) THEN
          PRINT *,'Floe too far east ',k,x(k)
          xt = AMOD(x(k),360.)
          IF (xt .GE. long1 .AND. xt .LT. long2) THEN
            PRINT *,'Wrapping floe around grid ',xt
            x(k) = xt
           ELSE
            PRINT *,'Resetting to eastern boundary ',long2
            x(k) = long2
          ENDIF  
        ENDIF

        IF (y(k) .LT. lat1) THEN
          PRINT *,'Floe too far south. y= ',k,y(k)
          PRINT *,'Resetting to southern bndy.'
          y(k) = lat1
        ENDIF
 9000 CONTINUE


C     Operational code.
C     Now find where the marked floes are.
C       ig, jg is the nearest (in a greatest integer sense) grid
C       point.  eta is the distance down the x axis the floe is
C       from that point, lambda is the same for y.  Both are measured
C       as fractions of a grid spacing.
      DO 3000 k = 1, npts
        ig(k) = INT( (x(k)-long1)/dlon ) + 1
        jg(k) = INT( (y(k)-lat1 )/dlat  ) + 1
        eta(k)    = (x(k)-long1)/dlon - ig(k) + 1
        lambda(k) = (y(k)-lat1)/dlat   - jg(k) + 1
 3000 CONTINUE

C     Bullet-proofing: ensure that the grid point assignments
C       lie on the grid, and that the interpolation parameters
C       are in the half open interval [0,1)
      DO 9100 k = 1, npts
        IF (ig(k) .LT. 1) THEN
          PRINT *,'i-coordinate computed west of domain ',k,ig(k)
CBG          ig(k) = 1
          ig(k) = INT(360. - dlon*(ig(k)-1) + 0.5)
          IF (ig(k) .LT. 1) THEN 
            PRINT *,'Crashing on point too far west ',k,ig(k)
            STOP
          ENDIF
        ENDIF
        IF (jg(k) .LT. 1) THEN
          PRINT *,'j-coordinate computed south of domain ',k,jg(k)
          jg(k) = 1
        ENDIF
        IF (eta(k) .LT. 0.) THEN
          PRINT *,'eta interpolation parameter estimated negative.'
          PRINT *,'Resetting to zero.'
          eta(k) = 0.0
        ENDIF
        IF (lambda(k) .LT. 0.) THEN
          PRINT *,'lambda interpolation parameter estimated negative.'
          PRINT *,'Resetting to zero.'
          lambda(k) = 0.0
        ENDIF
        IF (eta(k) .GE. 1.) THEN
          PRINT *,'eta interpolation parameter estimated GE 1.0.'
          PRINT *,'Resetting to 0.9999.'
          eta(k) = 0.9999
        ENDIF
        IF (lambda(k) .GE. 1.) THEN
          PRINT *,'lambda interpolation parameter estimated negative.'
          PRINT *,'Resetting to 0.9999.'
          lambda(k) = 0.9999
        ENDIF
 9100 CONTINUE

      RETURN
      END
