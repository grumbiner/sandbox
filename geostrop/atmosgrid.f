      SUBROUTINE atmos(slp, q, rho, t, ztopo, uten, vten,
     1                 nlat, nlong,
     2                 ua, va, ptype)
C     Compute atmospheric winds for ice drift given gridded input, and
C       for a variety of wind types.
C     Bob Grumbine 4 April 1994.

      IMPLICIT none
      INCLUDE "skile.inc"
      
      INTEGER nlat, nlong, ptype
      REAL slp(nlong, nlat), q(nlong, nlat), rho(nlong, nlat)
      REAL t(nlong, nlat), ztopo(nlong, nlat)
      REAL uten(nlong, nlat), vten(nlong, nlat)

      REAL ua(nlong, nlat), va(nlong, nlat)

C     For testing purposes, need following array and vars
      INTEGER ng, na, k, l
      LOGICAL test

CD      PARAMETER (ng = (long2-long1)/dlon+1)
      PARAMETER (ng = (long2-long1)/dlon  )
      PARAMETER (na = (lat2-lat1)/dlat+1)
      PARAMETER (k  = ng*na)
      PARAMETER (test = .FALSE.)
      REAL u(k,3), v(k,3), uexac(ng, na), vexac(ng, na)
      
      REAL f(na)
      INTEGER i, j, m, type

C     Bullet-proofing variables
      REAL ualim
      PARAMETER (ualim = 40.0)

      l = k
      CALL cf(f, nlat)

      IF (ptype .EQ. 1) THEN
        CALL crho(slp, t, q, rho, nlat, nlong)
        CALL uageo(f, rho, slp, ua, va,
     1             nlong, nlat)
       ELSEIF (ptype .EQ. 2) THEN
        CALL seadj(slp, t, ztopo, nlat, nlong)
        CALL crho(slp, t, q, rho, nlat, nlong)
        CALL uageo(f, rho, slp, ua, va,
     1             nlong, nlat)
       ELSEIF (ptype .EQ. 3) THEN
        DO 3000 j = 1, nlat
          DO 3010 i = 1, nlong
            ua(i,j) = uten(i,j)
            va(i,j) = vten(i,j)
 3010     CONTINUE
 3000   CONTINUE
       ELSE
        PRINT *,'error, type out of range, using surface-reduced winds'
        CALL seadj(slp, t, ztopo, nlat, nlong)
        CALL crho(slp, t, q, rho, nlat, nlong)
        CALL uageo(f, rho, slp, ua, va,
     1             nlong, nlat)
      ENDIF

      RETURN
      END
