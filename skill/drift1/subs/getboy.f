
      SUBROUTINE getboy(lat, lon, nbuoy, x, y)

      IMPLICIT none
      INCLUDE "skile1.inc"

      INTEGER nbuoy, i
      CHARACTER*60 fname
      REAL lat(50000), lon(50000)
      REAL x(50000), y(50000)
      REAL pi, r, toler, arcdis
      PARAMETER (toler = 30.0)
      REAL eccen, dtrad, xold, yold

      eccen = SQRT(eccen2)
 
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='OLD')
     
      i = 0
 1000 CONTINUE
        READ (12, 9002, END=1100) lat(i+1), lon(i+1)
        lon(i+1) = 360. - lon(i+1)
C       Convert from degrees west of buoys to degrees east of
C       rest of world
        IF (i+1 .GE. 2) THEN
          IF (arcdis(lon(i+1), lat(i+1), lon(i), lat(i)) .GT. toler) 
     1     THEN
            i = i + 1
            GO TO 1000
           ELSE
            GO TO 1000
          ENDIF
         ELSE
          i = i + 1
          GO TO 1000
        ENDIF
 1100 CONTINUE
      nbuoy = i
      PRINT *,'nbuoy = ', nbuoy

      pi = 4.*ATAN(1.)
      dtrad = pi/180.
      DO 2000 i = 1, nbuoy
        CALL mapll(x(i), y(i), lat(i)*dtrad, lon(i)*dtrad, 
     1             slat, slon, sgn, eccen, rearth)
        x(i) = (xorig+x(i)) / DX + 1
        y(i) = (yorig+y(i)) / DY + 1
 2000 CONTINUE
 9009 FORMAT (4F7.2, 3F9.5)

      DO 2001 i = 1, nbuoy
        x(i) = x(i) / FLOAT(L+1)
        y(i) = y(i) / FLOAT(M+1)
        IF (x(i) .GT. 1. .OR. y(i) .GT. 1.) THEN
          PRINT *,'x, y, out of bounds ',x(i), y(i), lon(i), lat(i)
          x(i) = 0.0
          y(i) = 0.0
        ENDIF
 2001 CONTINUE
 
 9001 FORMAT (A60)
 9002 FORMAT (7x, 9x, F5.1, F6.1)

      RETURN
      END
