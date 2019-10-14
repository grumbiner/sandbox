      SUBROUTINE cfll(f, nlat)
C     Compute the coriolis parameter on a regular global
C      latitude-longitude grid.
C     Start at north pole and work south.
C     Revised to be 'bullet-proof' -- check inputs and outputs
C       for reasonability.  Use best physical parameters.
C     Bob Grumbine 4 April 1994. 
C     Future note - generalize to not require skile.inc, make
C       a proper library routine.
C     Future note - make variant for polar stereographic grid.

      IMPLICIT none
      INCLUDE "skile.inc"

      INTEGER nlat
      REAL f(nlat)

      INTEGER i

      REAL omega, pi, rdpdg
      PARAMETER (omega = 7.292116E-5)
      PARAMETER (pi    = 3.141592654)
      PARAMETER (rdpdg = pi/180.)

C     Run-time variables
      REAL fpole, theta1, dtheta

C     Operational code
      theta1 = (lat1 - dlat)*rdpdg
      dtheta = dlat*rdpdg
      fpole  = 2.*omega
      DO 1000 i = 1, nlat
        f(i) = fpole*SIN( theta1 + dtheta*i )
 1000 CONTINUE

C     Bullet-proofing -- check that output is reasonable
      DO 2000 i = 1, nlat
        IF (ABS(f(i)) .GT. fpole) THEN
          PRINT *,'FATAL error in attempting to compute coriolis'
          PRINT *,'lat1, dlat, i, f = ',lat1, dlat, i, f(i)
          STOP
        ENDIF
 2000 CONTINUE

      RETURN
      END
