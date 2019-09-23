      SUBROUTINE drift(lat1, lon1, lat2, lon2, dx, dy, dist, dir)
C     Compute kinematic statistics from drifting buoys.
C     Bob Grumbine 4 April 1995.

      IMPLICIT none
C     Arguments
      REAL lat1, lon1, lat2, lon2
      REAL dx, dy, dist, dir

C     Local variables
      REAL arcdis, saccur, taccur
      PARAMETER (saccur = 1.0)
      PARAMETER (taccur = 0.4)

      REAL WDIR, dtheta

C*************************************************************
C     Compute displacements and delays
      dx = ABS(arcdis(lon1, lat1, lon2, lat1))
      dy = ABS(arcdis(lon1, lat1, lon1, lat2)) 
      dist = ABS(arcdis(lon1, lat1, lon2, lat2))
      dtheta = lon2 - lon1
      IF (dtheta .GT. 180) dtheta = dtheta - 360.
      dx = SIGN(dx, lon2 - lon1)
      dy = SIGN(dy, lat2 - lat1)
      dir = WDIR(dx, dy, dtheta)
    
      RETURN
      END
