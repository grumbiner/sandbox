      PROGRAM listpts 
C     Author Robert Grumbine
C     LAST MODIFIED 9 May 1994
C     Generate files listing the lat-long locations of the grid points.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      INTEGER i, j
      REAL lat(0:L,0:M), long(0:L,0:M)
      REAL E

      E = SQRT(eccen2)
      DO 1000 j = 0, M
        DO 1010 i = 0, L
          CALL mapxy(i*dx+xorig, j*dy+yorig, lat(i,j), long(i,j), 
     1               slat, slon, sgn, E, rearth)
          WRITE (*,*) i, j, long(i,j), lat(i,j)
 1010   CONTINUE
 1000 CONTINUE 
CD      i = polei
CD      j = polej
CD      CALL mapxy(i*dx+xorig, j*dy+yorig, lat(i,j), long(i,j), 
CD     1           slat, slon, sgn, E, rearth)
CD      WRITE (*,*) i, j, long(i,j), lat(i,j)
    
      OPEN(10, FILE='latlongpts', FORM='UNFORMATTED', STATUS='NEW')
      WRITE (10) lat
      WRITE (10) long
      CLOSE (10, STATUS='KEEP')
 
      STOP
      END
