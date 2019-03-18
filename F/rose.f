C***********************************************************----------!!
	SUBROUTINE rose(x, y, npts, xnot, ynot, radius, nleaf)
	IMPLICIT none
      INTEGER npts, x(npts), y(npts), xnot, ynot, nleaf
	REAL radius, pi, dtheta
	INTEGER i

      pi = ACOS(-1.0)
	dtheta = 2.*pi/FLOAT(npts-1)
	DO 1000 i = 1, npts
	  x(i) = xnot + INT(radius*cos(nleaf*i*dtheta)*cos(i*dtheta))
	  y(i) = ynot + INT(radius*cos(nleaf*i*dtheta)*sin(i*dtheta))
 1000 CONTINUE
      
	RETURN
	END
