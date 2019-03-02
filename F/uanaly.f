      SUBROUTINE uanaly(slp, temp, q, rho, f, u, v,
     1                  nlat, nlong, dlat, dlong,
     2                  long1, lat1, long2, lat2)
C     Compute the analytically-derived atmospheric velocity
C       field for the specified pressure, temperature, q, f.

      IMPLICIT none

      INTEGER nlat, nlong
      REAL dlat, dlong, lat1, long1, lat2, long2
      REAL slp(nlong, nlat), temp(nlong, nlat), q(nlong, nlat)
      REAL f(nlat), u(nlong, nlat), v(nlong, nlat)
      REAL rho(nlat, nlong)

      REAL omega, rearth, siglat, rdpdg, plow, x0, y0
      PARAMETER (omega = 7.292116E-5)
      PARAMETER (rearth = 6.730949E+6)
      PARAMETER (siglat = 3.0)
      PARAMETER (rdpdg  = 3.141592654/180.)
      PARAMETER (plow   = -1.8E+3          )

      REAL lambda, theta, c1
      INTEGER i, j

      x0 = (long1+long2)/2.
      y0 = (lat1 + lat2)/2.

      DO 1000 j = 1, nlat
        theta = lat1+(j-1)*dlat
        c1    = plow/rdpdg/f(j)/rearth/siglat/siglat
        DO 1010 i = 1, nlong
          lambda = long1+(i-1)*dlong
          v(i,j) = -(lambda-x0)/COS(theta*rdpdg)*c1/rho(i,j)*
     1     EXP( -( (lambda-x0)**2 + (theta-y0)**2 ) /2./siglat**2 )
          u(i,j) =  (theta-y0)*c1/rho(i,j)* 
     1     EXP( -( (lambda-x0)**2 + (theta-y0)**2 ) /2./siglat**2 )

 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
