C***********************************************************__________!!
      SUBROUTINE vext(vnp1, vnm1, vn, un, etan,
     1               scale, radius, deltat, dellat, dellon, gee, omega,
     2               dissip)
      IMPLICIT none
      INCLUDE "grid.inc"
      
      REAL vnp1(nlong, nlat), vnm1(nlong, nlat), vn(nlong, nlat)
      REAL un(nlong, nlat), etan(nlong, nlat)
      
      REAL scale, radius, deltat, dellat, dellon, gee, omega, dissip
      
      INTEGER i, j
      REAL c1, c2, theta, pi
      
      c1 = -4.*deltat*omega
      c2 = -2.*deltat*gee/dellat/2.
      DO 1000 j = 2, nlat-1
        DO 1010 i = 1, nlong-1
          vnp1(i,j) = vnm1(i,j) + c1*un(i,j) - dissip*deltat*vnm1(i,j)
     1                          + c2*(etan(i,j+1)-etan(i,j-1))
 1010   CONTINUE
 1000 CONTINUE
 
      DO 2000 j = 1, nlat-1
        vnp1(nlong,j) = vnp1(1,j)
 2000 CONTINUE
 
C     Apply a no normal gradient boundary condition at north and south
C     No, don't, that won't conserve mass, apply no normal velocity
      DO 3000 i = 1, nlong
        vnp1(i,1)      = 0.0
        vnp1(i,nlat)   = 0.0
 3000 CONTINUE

      RETURN
      END
