      PROGRAM grid
      IMPLICIT none
      INCLUDE "driftgrid.north"

      INTEGER i, j, k
      REAL lat, lon
      REAL x, y, e 
      REAL lons(NX*NY), lats(NX*NY)

      k = 0

      PRINT *,'nx*ny = ',NX*NY

      e = sqrt(eccen2)
      DO j = 1, NY
      DO i = 1, NX
        k = k + 1
        x = xorig + i*dx
        y = yorig + j*dy
        CALL mapxy(x, y, lat, lon, slat, slon, sgn, e, rearth)
        lons(k) = lon
        lats(k) = lat
CD        PRINT *,i, j, lat, lon
      ENDDO
      ENDDO

      END
