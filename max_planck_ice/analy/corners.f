      PROGRAM findcorn
C     Print out the lat, long of the corner points of the map.
C     Author Robert Grumbine
C     LAST MODIFIED 16 August 1994

      IMPLICIT none

      INCLUDE "icegrid.inc"
      REAL llat, llong, rlat, rlong

      CALL mapxy(xorig, yorig, llat, llong, slat, slon, sgn, 
     1    SQRT(eccen2), rearth)

      CALL mapxy(L*dx+xorig, M*dy+yorig, rlat, rlong, slat, slon, sgn, 
     1     SQRT(eccen2), rearth)

      WRITE (*,9001) llong
      WRITE (*,9001) rlong
      WRITE (*,9001) llat
      WRITE (*,9001) rlat      
C     Lat-long interval.
      WRITE (*,9002) 5

C     Projection type
      WRITE (*,9002) 1
      WRITE (*,9001) slat
      WRITE (*,9001) -90.-slon

 9001 FORMAT (F13.7)
 9002 FORMAT (I2)
  
      STOP
      END
