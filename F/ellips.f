      SUBROUTINE ellips(x, y, npts, xnot, ynot, radius, eccen)
      IMPLICIT none
      INTEGER npts, x(npts), y(npts), xnot, ynot, radius
      REAL eccen
      REAL pi, dtheta, r, theta
      INTEGER i
	
      pi = ACOS(-1.0)
      dtheta = 2.*pi/FLOAT(npts-1)
      DO 1000 i = 1, npts
        theta = FLOAT(i)*dtheta
        r = FLOAT(radius)*(1.-eccen*eccen) / (1.-eccen*cos(theta) )
        x(i) = xnot+INT(r*cos(theta)+0.5)
        y(i) = ynot+INT(r*sin(theta)+0.5)
 1000 CONTINUE
 
      RETURN
      END
